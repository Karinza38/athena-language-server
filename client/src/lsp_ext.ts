import * as lc from "vscode-languageclient";

export type SyntaxTreeParams = lc.TextDocumentIdentifier;

export const syntaxTree = new lc.RequestType<SyntaxTreeParams, string, void>(
    "athena-language-server/dumpSyntaxTree"
);
