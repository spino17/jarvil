import { existsSync } from "fs";
import { join } from "path";
import { ExtensionContext, window, workspace } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

// Locates the language server binary.
//
// Checked in order: the explicit setting, then PATH, then the workspace's cargo
// output directories. The last of those is what makes the extension usable
// straight after `cargo build` without any configuration, which is how it will
// be run while the language itself is still being developed.
function resolveServerPath(): string | undefined {
  const configured = workspace
    .getConfiguration("jarvil")
    .get<string>("server.path");

  if (configured) {
    return existsSync(configured) ? configured : undefined;
  }

  const folders = workspace.workspaceFolders ?? [];

  for (const folder of folders) {
    for (const profile of ["release", "debug"]) {
      const candidate = join(folder.uri.fsPath, "target", profile, "jarvil-lsp");

      if (existsSync(candidate)) {
        return candidate;
      }
    }
  }

  // fall back to PATH; spawning fails loudly if it is not there
  return "jarvil-lsp";
}

export function activate(context: ExtensionContext) {
  const command = resolveServerPath();

  if (!command) {
    window.showErrorMessage(
      "Jarvil: the configured `jarvil.server.path` does not exist. " +
        "Clear the setting to fall back to PATH or a cargo build directory.",
    );

    return;
  }

  const serverOptions: ServerOptions = {
    run: { command, transport: TransportKind.stdio },
    debug: { command, transport: TransportKind.stdio },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "jarvil" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.jv"),
    },
  };

  // Stopping an already-running client guards against `activate` being called
  // twice in one extension host, which would otherwise leave an orphaned server
  // answering requests alongside the new one — the symptom being every hover
  // and completion appearing duplicated.
  void client?.stop();

  client = new LanguageClient(
    "jarvil",
    "Jarvil Language Server",
    serverOptions,
    clientOptions,
  );

  // Registering the client for disposal means VS Code shuts the server down
  // when the extension is disabled or uninstalled, rather than relying on
  // `deactivate` alone.
  context.subscriptions.push(client);

  client.start().catch((error) => {
    window.showErrorMessage(
      `Jarvil: could not start the language server (${command}). ` +
        `Build it with \`cargo build --release -p jarvil-lsp\`, or set ` +
        `\`jarvil.server.path\`. ${error}`,
    );
  });
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
