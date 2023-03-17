import * as vscode from "vscode";

export type AthenaDocument = vscode.TextDocument & {
    languageId: "athena";
};

export type AthenaEditor = vscode.TextEditor & {
    document: AthenaDocument;
};

export function isAthenaDocument(
    document: vscode.TextDocument
): document is AthenaDocument {
    return document.languageId === "athena" && document.uri.scheme === "file";
}

export function isAthenaEditor(
    editor: vscode.TextEditor
): editor is AthenaEditor {
    return isAthenaDocument(editor.document);
}

export function sleep(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

/** Sets ['when'](https://code.visualstudio.com/docs/getstarted/keybindings#_when-clause-contexts) clause contexts */
export function setContextValue(key: string, value: any): Thenable<void> {
    return vscode.commands.executeCommand("setContext", key, value);
}
