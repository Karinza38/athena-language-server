import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";
import { Disposable } from "vscode";
import { AthenaEditor, isAthenaEditor } from "./util";
import { createClient } from "./extension";
import { Executable, ServerOptions } from "vscode-languageclient/node";
import { Cmd } from "./commands";

export type CtxInit = Ctx & {
    readonly client: lc.LanguageClient;
};

export type CommandFactory = {
    enabled: (ctx: CtxInit) => Cmd;
    disabled?: (ctx: Ctx) => Cmd;
};

export class Ctx {
    private _client: lc.LanguageClient | undefined;

    private commandFactories: Record<string, CommandFactory>;
    private commandDisposables: Disposable[];

    constructor(
        readonly extCtx: vscode.ExtensionContext,
        commandFactories: Record<string, CommandFactory>
    ) {
        extCtx.subscriptions.push(this);
        this.commandFactories = commandFactories;
        this.commandDisposables = [];
    }

    dispose() {
        this.commandDisposables.forEach((disposable) => disposable.dispose());
    }

    private async getOrCreateClient() {
        if (!this._client) {
            const traceOutputChannel = vscode.window.createOutputChannel(
                "Athena Language Server trace"
            );
            this.pushExtCleanup(traceOutputChannel);
            const command = process.env.SERVER_PATH || "athena-language-server";
            const run: Executable = {
                command,
                options: {
                    env: {
                        ...process.env,
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        RUST_LOG: "debug,salsa=info",
                        RUST_BACKTRACE: "1",
                    },
                },
            };

            // If the extension is launched in debug mode then the debug server options are used
            // Otherwise the run options are used
            const serverOptions: ServerOptions = {
                run,
                debug: run,
            };
            const client = await createClient(
                traceOutputChannel,
                serverOptions
            );
            this._client = client;

            return client;
        }
    }

    async start() {
        const client = await this.getOrCreateClient();
        if (!client) {
            return;
        }
        await client.start();
        this.updateCommands();
    }

    async stop() {
        if (!this._client) {
            return;
        }
        this.updateCommands("disable");
        await this._client.stop();
    }

    private updateCommands(forceDisable?: "disable") {
        this.commandDisposables.forEach((disposable) => disposable.dispose());
        this.commandDisposables = [];

        const clientRunning =
            (!forceDisable && this._client?.isRunning()) ?? false;
        const isClientRunning = function (_ctx: Ctx): _ctx is CtxInit {
            return clientRunning;
        };

        for (const [name, factory] of Object.entries(this.commandFactories)) {
            const fullName = `athena-language-server.${name}`;
            let callback: Cmd;
            if (isClientRunning(this)) {
                callback = factory.enabled(this);
            } else if (factory.disabled) {
                callback = factory.disabled(this);
            } else {
                callback = () =>
                    vscode.window.showErrorMessage(
                        `command ${fullName} not available`
                    );
            }

            this.commandDisposables.push(
                vscode.commands.registerCommand(fullName, callback)
            );
        }
    }

    get client() {
        return this._client;
    }

    get subscriptions(): Disposable[] {
        return this.extCtx.subscriptions;
    }

    get activeAthenaEditor(): AthenaEditor | undefined {
        const editor = vscode.window.activeTextEditor;
        return editor && isAthenaEditor(editor) ? editor : undefined;
    }

    pushExtCleanup(d: Disposable) {
        this.extCtx.subscriptions.push(d);
    }
}
