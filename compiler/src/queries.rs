// Position-driven queries over a finished analysis: what is declared at this
// offset, and what should be said about it.
//
// There is no general "find the node at this offset" tree walk here, and that
// is deliberate. Name resolution already records every identifier it resolved
// in `identifier_in_use_binding_table`, keyed by the very node the cursor would
// land on, so the lookup is a scan of that table rather than a descent through
// the AST. Fewer moving parts, and it cannot disagree with the resolver about
// what counts as a reference.

use crate::analysis::AnalysisCtx;
use crate::ast::ast::{OkIdentifierInDeclNode, OkIdentifierInUseNode};
use crate::ast::traits::Node;
use crate::scope::symbol::core::{ConcreteSymbolDataEntry, SymbolDataEntry};
use crate::types::core::TypeStringifyContext;
use crate::types::traits::TypeLike;
use text_size::TextRange;

// Where a symbol was declared, and what to call it.
#[derive(Debug, Clone)]
pub struct Definition {
    // the identifier's declaration site, to jump to
    pub target_range: TextRange,
    // the reference that was resolved, to highlight as the origin
    pub origin_range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Hover {
    // markdown
    pub contents: String,
    pub range: TextRange,
}

fn contains(range: TextRange, offset: u32) -> bool {
    // inclusive of the end so that a cursor resting just past the last
    // character of a name still hits it, which is what an editor sends when
    // you put the caret at the end of a word
    u32::from(range.start()) <= offset && offset <= u32::from(range.end())
}

// The name part of a use-site identifier, excluding any `<...>` type arguments,
// so that hovering inside a turbofish does not resolve to the base name.
fn use_name_range(node: &OkIdentifierInUseNode) -> TextRange {
    node.core_ref().name.range()
}

fn decl_name_range(node: &OkIdentifierInDeclNode) -> TextRange {
    node.core_ref().name.range()
}

// Declaration range recorded for whichever namespace the symbol lives in.
fn decl_range_of_use(ctx: &AnalysisCtx<'_>, entry: &ConcreteSymbolDataEntry) -> TextRange {
    let namespace = ctx.semantic_db.namespace_ref();

    match entry {
        ConcreteSymbolDataEntry::Variable(index) => namespace
            .variables_ref()
            .symbol_ref(index.symbol_index())
            .decl_line_number(),
        ConcreteSymbolDataEntry::Function(index) => namespace
            .funcs_ref()
            .symbol_ref(index.symbol_index())
            .decl_line_number(),
        ConcreteSymbolDataEntry::Type(index) => namespace
            .types_ref()
            .symbol_ref(index.symbol_index())
            .decl_line_number(),
        ConcreteSymbolDataEntry::Interface(index) => namespace
            .interfaces_ref()
            .symbol_ref(index.symbol_index())
            .decl_line_number(),
    }
}

// Finds the resolved reference under the cursor, if any.
fn use_at<'ctx>(
    ctx: &'ctx AnalysisCtx<'ctx>,
    offset: u32,
) -> Option<(&'ctx OkIdentifierInUseNode, TextRange)> {
    ctx.semantic_db
        .identifier_in_use_binding_table_ref()
        .keys()
        .map(|node| (node, use_name_range(node)))
        .filter(|(_, range)| contains(*range, offset))
        // several ranges can contain the offset when names abut; the tightest
        // one is the one the cursor is really on
        .min_by_key(|(_, range)| u32::from(range.end()) - u32::from(range.start()))
}

// Finds a declaration site under the cursor.
fn decl_at<'ctx>(
    ctx: &'ctx AnalysisCtx<'ctx>,
    offset: u32,
) -> Option<(&'ctx OkIdentifierInDeclNode, TextRange)> {
    ctx.semantic_db
        .identifier_in_decl_binding_table_ref()
        .keys()
        .map(|node| (node, decl_name_range(node)))
        .filter(|(_, range)| contains(*range, offset))
        .min_by_key(|(_, range)| u32::from(range.end()) - u32::from(range.start()))
}

pub fn definition_at(ctx: &AnalysisCtx<'_>, offset: u32) -> Option<Definition> {
    // a reference jumps to its declaration
    if let Some((node, origin_range)) = use_at(ctx, offset) {
        let entry = ctx.semantic_db.symbol_for_identifier_in_use(node)?;

        return Some(Definition {
            target_range: decl_range_of_use(ctx, &entry),
            origin_range,
        });
    }

    // a declaration resolves to itself, so "go to definition" on one is a no-op
    // rather than a dead end
    let (_, range) = decl_at(ctx, offset)?;

    Some(Definition {
        target_range: range,
        origin_range: range,
    })
}

// Renders a callable's signature as it would be written in source, e.g.
// `(x: int, y: int) -> int`. Parameter *names* are not kept on the prototype,
// only their types, so the types stand alone.
fn prototype_to_string(
    data: &crate::scope::symbol::function::CallableData,
    context: TypeStringifyContext<'_>,
) -> String {
    let prototype = data.structural_prototype();

    let params: Vec<String> = prototype
        .params()
        .iter()
        .map(|ty| ty.to_string(context))
        .collect();

    let return_ty = prototype.return_ty();

    if return_ty.is_void() {
        return format!("({})", params.join(", "));
    }

    format!(
        "({}) -> {}",
        params.join(", "),
        return_ty.to_string(context)
    )
}

// Type of a symbol, rendered for display. Only variables carry a type directly;
// for the rest the kind is the useful thing to show.
fn describe_use(ctx: &AnalysisCtx<'_>, entry: &ConcreteSymbolDataEntry, name: &str) -> String {
    let namespace = ctx.semantic_db.namespace_ref();
    let context = TypeStringifyContext::new(ctx.semantic_db.interner(), namespace);

    match entry {
        ConcreteSymbolDataEntry::Variable(index) => {
            let data = namespace
                .variables_ref()
                .symbol_ref(index.symbol_index())
                .data_ref();

            format!("let {}: {}", name, data.ty().to_string(context))
        }
        ConcreteSymbolDataEntry::Function(index) => {
            let data = namespace
                .funcs_ref()
                .symbol_ref(index.symbol_index())
                .data_ref();

            format!("def {}{}", name, prototype_to_string(data, context))
        }
        ConcreteSymbolDataEntry::Type(_) => format!("type {}", name),
        ConcreteSymbolDataEntry::Interface(_) => format!("interface {}", name),
    }
}

fn describe_decl(ctx: &AnalysisCtx<'_>, entry: &SymbolDataEntry, name: &str) -> String {
    let namespace = ctx.semantic_db.namespace_ref();
    let context = TypeStringifyContext::new(ctx.semantic_db.interner(), namespace);

    match entry {
        SymbolDataEntry::Variable(index) => {
            let data = namespace.variables_ref().symbol_ref(*index).data_ref();

            format!("let {}: {}", name, data.ty().to_string(context))
        }
        SymbolDataEntry::Function(index) => {
            let data = namespace.funcs_ref().symbol_ref(*index).data_ref();

            format!("def {}{}", name, prototype_to_string(data, context))
        }
        SymbolDataEntry::Type(_) => format!("type {}", name),
        SymbolDataEntry::Interface(_) => format!("interface {}", name),
    }
}

// Doc comment for a declaration: the unbroken run of `//` lines directly above
// the line the declaration starts on.
//
// This reads the source rather than the token trivia on purpose. Comments do
// survive lexing as trivia, but they attach to the next token, which for
// `// doc\ndef add(..)` is the `def` keyword -- not the name node we are
// holding. Walking back over source lines sidesteps that entirely and gives the
// same answer regardless of which token a comment happened to land on.
fn doc_comment(ctx: &AnalysisCtx<'_>, decl_range: TextRange) -> Option<String> {
    let offset = usize::from(decl_range.start());
    let source = ctx.source;

    if offset > source.len() {
        return None;
    }

    // start of the line the declaration sits on
    let line_start = source[..offset]
        .rfind('\n')
        .map(|index| index + 1)
        .unwrap_or(0);

    let mut lines: Vec<&str> = vec![];
    let mut cursor = line_start;

    // walk upwards while the preceding line is a `//` comment
    while cursor > 0 {
        let previous_end = cursor - 1; // the '\n' that ended the previous line
        let previous_start = source[..previous_end]
            .rfind('\n')
            .map(|index| index + 1)
            .unwrap_or(0);

        let line = source[previous_start..previous_end].trim();

        let Some(text) = line.strip_prefix("//") else {
            break;
        };

        lines.push(text.trim());
        cursor = previous_start;
    }

    if lines.is_empty() {
        return None;
    }

    // collected bottom-up
    lines.reverse();

    Some(lines.join("\n"))
}

pub fn hover_at(ctx: &AnalysisCtx<'_>, offset: u32) -> Option<Hover> {
    let interner = ctx.semantic_db.interner();

    if let Some((node, range)) = use_at(ctx, offset) {
        let entry = ctx.semantic_db.symbol_for_identifier_in_use(node)?;
        let name = interner.lookup(node.core_ref().name.token_value(ctx.code_handler, interner));
        let signature = describe_use(ctx, &entry, &name);

        return Some(Hover {
            contents: format!("```jarvil\n{}\n```", signature),
            range,
        });
    }

    let (node, range) = decl_at(ctx, offset)?;
    let entry = ctx.semantic_db.symbol_for_identifier_in_decl(node)?;
    let name = interner.lookup(node.core_ref().name.token_value(ctx.code_handler, interner));
    let signature = describe_decl(ctx, entry, &name);

    let contents = match doc_comment(ctx, range) {
        Some(docs) => format!("```jarvil\n{}\n```\n\n---\n\n{}", signature, docs),
        None => format!("```jarvil\n{}\n```", signature),
    };

    Some(Hover { contents, range })
}
