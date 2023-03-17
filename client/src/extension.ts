/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { workspace, ExtensionContext, window } from "vscode";

import * as vscode from "vscode";
import * as commands from "./commands";

import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from "vscode-languageclient/node";
import { CommandFactory, Ctx } from "./ctx";
import { setContextValue } from "./util";

const ATHENA_PROJECT_CONTEXT = "inAthenaProject";

export interface AthenaExtensionApi {
    readonly client?: LanguageClient;
}

let ctx: Ctx;

export async function createClient(
    traceOutputChannel: vscode.OutputChannel,
    serverOptions: ServerOptions
): Promise<LanguageClient> {
    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [
            { scheme: "file", language: "athena" },
            { scheme: "file", language: "ath" },
        ],
        synchronize: {},
        traceOutputChannel,
    };

    const client = new LanguageClient(
        "athena-language-server",
        "Athena Language Server",
        serverOptions,
        clientOptions
    );

    return client;
}

export function activate(context: ExtensionContext) {
    // The server is implemented in node

    // Create the language client and start the client.

    const ctx = new Ctx(context, createCommands());

    setContextValue(ATHENA_PROJECT_CONTEXT, true);
    // Start the client. This will also launch the server
    ctx.start();
}

export function deactivate(): Thenable<void> | undefined {
    setContextValue(ATHENA_PROJECT_CONTEXT, undefined);
    return ctx.stop();
}

function createCommands(): Record<string, CommandFactory> {
    return {
        syntaxTree: { enabled: commands.syntaxTree },
    };
}
