//! Language server for Jarvil, speaking LSP over stdio.
//!
//! # Capabilities
//!
//! - **Diagnostics**, republished on every change, including the empty list
//!   that clears them once the last error is fixed
//! - **Go to definition**, for variables, functions, types and interfaces
//! - **Hover**, showing a signature or inferred type plus any doc comment
//!
//! # Design
//!
//! Scope is deliberately small. Jarvil has no module system, so a document is
//! the whole compilation unit: there is no workspace graph, no cross-file
//! resolution, and no reason for an incremental front end. Every request
//! re-analyses the buffer it concerns, which at the size of programs this
//! compiler handles is fast and, more usefully, means the server can never
//! serve an answer that disagrees with the source.
//!
//! The one piece of state is the document store, because the protocol expects
//! the server to remember the current text of each open file.
//!
//! Analysis runs through [`compiler::analysis::with_analysis`], which reports
//! every diagnostic rather than only the first, and answers position queries
//! even for a program that does not compile -- which is the normal case while
//! someone is typing.

mod line_index;

use compiler::analysis::{Severity, with_analysis};
use compiler::queries::{definition_at, hover_at};
use line_index::LineIndex;
use std::collections::HashMap;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    // Keyed by URI so that the shape is already right for the day modules land
    // and a request has to reach beyond the file it arrived on.
    documents: RwLock<HashMap<Url, LineIndex>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Backend {
            client,
            documents: RwLock::new(HashMap::new()),
        }
    }

    async fn set_document(&self, uri: Url, text: String) {
        self.documents
            .write()
            .await
            .insert(uri, LineIndex::new(&text));
    }

    // Analyses `uri`'s current text and pushes the result to the client.
    //
    // Diagnostics are *always* published, including the empty list, since that
    // is what clears stale squiggles once the last error is fixed.
    async fn publish_diagnostics(&self, uri: Url, version: Option<i32>) {
        let documents = self.documents.read().await;

        let Some(index) = documents.get(&uri) else {
            return;
        };

        let diagnostics = with_analysis(index.text(), |ctx| {
            ctx.diagnostics
                .iter()
                .map(|diagnostic| {
                    let range = diagnostic.primary_range();

                    // miette's `help` is prose aimed at a terminal reader; it
                    // belongs in the message rather than being dropped, since
                    // an editor shows no equivalent of a help line.
                    let message = match &diagnostic.help {
                        Some(help) => format!("{}\n\n{}", diagnostic.message, help),
                        None => diagnostic.message.clone(),
                    };

                    // secondary labels explain *why*, so surface them where the
                    // client supports it rather than losing them
                    let related: Vec<DiagnosticRelatedInformation> = diagnostic
                        .labels
                        .iter()
                        .filter(|label| !label.is_primary)
                        .filter_map(|label| {
                            let text = label.message.as_ref()?;

                            Some(DiagnosticRelatedInformation {
                                location: Location {
                                    uri: uri.clone(),
                                    range: index.range(
                                        label.range.start().into(),
                                        label.range.end().into(),
                                    ),
                                },
                                message: text.clone(),
                            })
                        })
                        .collect();

                    Diagnostic {
                        range: index.range(range.start().into(), range.end().into()),
                        severity: Some(match diagnostic.severity {
                            Severity::Error => DiagnosticSeverity::ERROR,
                            Severity::Warning => DiagnosticSeverity::WARNING,
                            Severity::Advice => DiagnosticSeverity::INFORMATION,
                        }),
                        code: diagnostic.code.clone().map(NumberOrString::String),
                        source: Some("jarvil".to_string()),
                        message,
                        related_information: if related.is_empty() {
                            None
                        } else {
                            Some(related)
                        },
                        ..Default::default()
                    }
                })
                .collect::<Vec<_>>()
        });

        drop(documents);

        self.client
            .publish_diagnostics(uri, diagnostics, version)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "jarvil-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            capabilities: ServerCapabilities {
                // Full sync: the server re-analyses whole buffers anyway, so
                // there is nothing to gain from reassembling incremental edits.
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "jarvil-lsp ready")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        // tower-lsp's read loop sits on `stdin.next()` and only notices that the
        // service has exited when another frame arrives, so an `exit`
        // notification alone does not end the process -- the client closing the
        // pipe does. VS Code closes it, but a client that sends `exit` and then
        // waits would leave this process orphaned.
        //
        // The spec has the client send `shutdown` and then `exit`, so once
        // `shutdown` has been answered there is no legitimate work left. Give
        // the client a moment to close the pipe on its own, and bail out if it
        // does not.
        tokio::spawn(async {
            tokio::time::sleep(std::time::Duration::from_secs(5)).await;

            std::process::exit(0);
        });

        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = params.text_document;

        self.set_document(document.uri.clone(), document.text).await;
        self.publish_diagnostics(document.uri, Some(document.version))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // FULL sync means each notification carries the entire document, so the
        // last change is the current text.
        let Some(change) = params.content_changes.into_iter().next_back() else {
            return;
        };

        let uri = params.text_document.uri;

        self.set_document(uri.clone(), change.text).await;
        self.publish_diagnostics(uri, Some(params.text_document.version))
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.publish_diagnostics(params.text_document.uri, None)
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        self.documents.write().await.remove(&uri);

        // clear the squiggles for a file that is no longer open
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;

        let Some(index) = documents.get(&uri) else {
            return Ok(None);
        };

        let offset = index.offset(position);

        let response = with_analysis(index.text(), |ctx| {
            definition_at(&ctx, offset).map(|definition| {
                // `LocationLink` lets the editor highlight the reference that
                // was resolved, not just drop the cursor at the target
                GotoDefinitionResponse::Link(vec![LocationLink {
                    origin_selection_range: Some(index.range(
                        definition.origin_range.start().into(),
                        definition.origin_range.end().into(),
                    )),
                    target_uri: uri.clone(),
                    target_range: index.range(
                        definition.target_range.start().into(),
                        definition.target_range.end().into(),
                    ),
                    target_selection_range: index.range(
                        definition.target_range.start().into(),
                        definition.target_range.end().into(),
                    ),
                }])
            })
        });

        Ok(response)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;

        let Some(index) = documents.get(&uri) else {
            return Ok(None);
        };

        let offset = index.offset(position);

        let response = with_analysis(index.text(), |ctx| {
            hover_at(&ctx, offset).map(|hover| Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover.contents,
                }),
                range: Some(index.range(hover.range.start().into(), hover.range.end().into())),
            })
        });

        Ok(response)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
