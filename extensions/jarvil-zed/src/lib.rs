//! Zed extension for Jarvil.
//!
//! Zed extensions are WebAssembly modules, so this cannot start a process
//! itself -- it tells Zed *what* to run and Zed spawns it. All this has to do
//! is locate the `jarvil-lsp` binary; the server is the same one the VS Code
//! extension talks to, so both editors get identical diagnostics,
//! go-to-definition and hover from a single implementation.
//!
//! Syntax highlighting needs no code here either. Zed highlights exclusively
//! through tree-sitter, and it builds the grammar named in `extension.toml`
//! itself, then applies the queries in `languages/jarvil/`. Neither path runs
//! through this module, so highlighting works whether or not the language
//! server is installed.

use zed_extension_api::{self as zed, LanguageServerId, Result};

struct JarvilExtension;

const BINARY: &str = "jarvil-lsp";

impl zed::Extension for JarvilExtension {
    fn new() -> Self {
        JarvilExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        // `which` consults the worktree's own environment rather than the
        // extension host's, which matters because an editor launched from a
        // dock or from Spotlight does not inherit a login shell's `PATH`.
        //
        // Deliberately the only lookup. The sandbox has no way to test whether
        // a path exists -- only `read_text_file`, which cannot distinguish a
        // missing binary from an unreadable one -- so guessing at build
        // directories would mean handing Zed a path that may not exist and
        // turning a clear error into a confusing one.
        let command = worktree.which(BINARY).ok_or_else(|| {
            format!(
                "could not find `{BINARY}` on PATH.\n\n\
                 Install it with:\n    \
                 cargo install --path crates/jarvil-lsp\n\n\
                 If it is installed but still not found, Zed may not have \
                 inherited your shell's PATH; launching Zed from a terminal \
                 usually fixes that."
            )
        })?;

        Ok(zed::Command {
            command,
            args: vec![],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(JarvilExtension);
