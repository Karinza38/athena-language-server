import { CtxInit } from "./ctx";
import * as vscode from "vscode";
import { isAthenaDocument, isAthenaEditor, sleep } from "./util";
import * as ath from "./lsp_ext";

export type Cmd = (...args: any[]) => unknown;

export function syntaxTree(ctx: CtxInit): Cmd {
    const tdcp = new (class implements vscode.TextDocumentContentProvider {
        readonly uri = vscode.Uri.parse(
            "athena-language-server-syntax-tree://syntaxtree/tree.ast"
        );
        readonly eventEmitter = new vscode.EventEmitter<vscode.Uri>();
        constructor() {
            vscode.workspace.onDidChangeTextDocument(
                this.onDidChangeTextDocument,
                this,
                ctx.subscriptions
            );
            vscode.window.onDidChangeActiveTextEditor(
                this.onDidChangeActiveTextEditor,
                this,
                ctx.subscriptions
            );
        }

        private onDidChangeTextDocument(event: vscode.TextDocumentChangeEvent) {
            if (isAthenaDocument(event.document)) {
                // We need to order this after language server updates, but there's no API for that.
                // Hence, good old sleep().
                void sleep(10).then(() => this.eventEmitter.fire(this.uri));
            }
        }
        private onDidChangeActiveTextEditor(
            editor: vscode.TextEditor | undefined
        ) {
            if (editor && isAthenaEditor(editor)) {
                this.eventEmitter.fire(this.uri);
            }
        }

        async provideTextDocumentContent(
            uri: vscode.Uri,
            ct: vscode.CancellationToken
        ): Promise<string> {
            const athenaEditor = ctx.activeAthenaEditor;
            if (!athenaEditor) return "";
            const client = ctx.client;

            const params = { uri: athenaEditor.document.uri.toString() };
            return client.sendRequest(ath.syntaxTree, params, ct);
        }

        get onDidChange(): vscode.Event<vscode.Uri> {
            return this.eventEmitter.event;
        }
    })();

    // ctx.pushExtCleanup(new AstInspector(ctx));
    ctx.pushExtCleanup(
        vscode.workspace.registerTextDocumentContentProvider(
            "athena-language-server-syntax-tree",
            tdcp
        )
    );

    return async () => {
        const editor = vscode.window.activeTextEditor;
        const rangeEnabled = !!editor && !editor.selection.isEmpty;

        const uri = rangeEnabled
            ? vscode.Uri.parse(`${tdcp.uri.toString()}?range=true`)
            : tdcp.uri;

        const document = await vscode.workspace.openTextDocument(uri);

        tdcp.eventEmitter.fire(uri);

        void (await vscode.window.showTextDocument(document, {
            viewColumn: vscode.ViewColumn.Two,
            preserveFocus: true,
        }));
    };
}
