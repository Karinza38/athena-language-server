//! Semantic Tokens helpers
use std::{ops, sync::atomic::AtomicU32};
use syntax::{
    ast::{self},
    match_ast, AstNode, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, WalkEvent, T,
};
use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensEdit,
};

use crate::line_index::LineIndex;

macro_rules! define_semantic_token_types {
    (
        standard {
            $($standard:ident),*$(,)?
        }
        custom {
            $(($custom:ident, $string:literal)),*$(,)?
        }

    ) => {
        $(#[allow(dead_code)] pub(crate) const $standard: SemanticTokenType = SemanticTokenType::$standard;)*
        $(pub(crate) const $custom: SemanticTokenType = SemanticTokenType::new($string);)*

        pub(crate) const SUPPORTED_TYPES: &[SemanticTokenType] = &[
            $(SemanticTokenType::$standard,)*
            $($custom),*
        ];
    };
}

define_semantic_token_types![
    standard {
        COMMENT,
        DECORATOR,
        ENUM_MEMBER,
        ENUM,
        FUNCTION,
        INTERFACE,
        KEYWORD,
        MACRO,
        METHOD,
        NAMESPACE,
        NUMBER,
        OPERATOR,
        PARAMETER,
        PROPERTY,
        STRING,
        STRUCT,
        TYPE_PARAMETER,
        VARIABLE,
        TYPE,
        CLASS,
    }

    custom {
        (ANGLE, "angle"),
        (ARITHMETIC, "arithmetic"),
        (ATTRIBUTE, "attribute"),
        (ATTRIBUTE_BRACKET, "attributeBracket"),
        (BITWISE, "bitwise"),
        (BOOLEAN, "boolean"),
        (BRACE, "brace"),
        (BRACKET, "bracket"),
        (BUILTIN_ATTRIBUTE, "builtinAttribute"),
        (BUILTIN_TYPE, "builtinType"),
        (CHAR, "character"),
        (COLON, "colon"),
        (COMMA, "comma"),
        (COMPARISON, "comparison"),
        (CONST_PARAMETER, "constParameter"),
        (DERIVE, "derive"),
        (DERIVE_HELPER, "deriveHelper"),
        (DOT, "dot"),
        (ESCAPE_SEQUENCE, "escapeSequence"),
        (FORMAT_SPECIFIER, "formatSpecifier"),
        (GENERIC, "generic"),
        (LABEL, "label"),
        (LIFETIME, "lifetime"),
        (LOGICAL, "logical"),
        (MACRO_BANG, "macroBang"),
        (PARENTHESIS, "parenthesis"),
        (PUNCTUATION, "punctuation"),
        (SELF_KEYWORD, "selfKeyword"),
        (SELF_TYPE_KEYWORD, "selfTypeKeyword"),
        (SEMICOLON, "semicolon"),
        (TYPE_ALIAS, "typeAlias"),
        (TOOL_MODULE, "toolModule"),
        (UNION, "union"),
        (UNRESOLVED_REFERENCE, "unresolvedReference"),
    }
];

macro_rules! define_semantic_token_modifiers {
    (
        standard {
            $($standard:ident),*$(,)?
        }
        custom {
            $(($custom:ident, $string:literal)),*$(,)?
        }

    ) => {

        $(#[allow(dead_code)] pub(crate) const $standard: SemanticTokenModifier = SemanticTokenModifier::$standard;)*
        $(pub(crate) const $custom: SemanticTokenModifier = SemanticTokenModifier::new($string);)*

        pub(crate) const SUPPORTED_MODIFIERS: &[SemanticTokenModifier] = &[
            $(SemanticTokenModifier::$standard,)*
            $($custom),*
        ];
    };
}

define_semantic_token_modifiers![
    standard {
        DOCUMENTATION,
        DECLARATION,
        STATIC,
        DEFAULT_LIBRARY,
    }
    custom {
        (ASYNC, "async"),
        (ATTRIBUTE_MODIFIER, "attribute"),
        (CALLABLE, "callable"),
        (CONSTANT, "constant"),
        (CONSUMING, "consuming"),
        (CONTROL_FLOW, "controlFlow"),
        (CRATE_ROOT, "crateRoot"),
        (INJECTED, "injected"),
        (INTRA_DOC_LINK, "intraDocLink"),
        (LIBRARY, "library"),
        (MUTABLE, "mutable"),
        (PUBLIC, "public"),
        (REFERENCE, "reference"),
        (TRAIT_MODIFIER, "trait"),
        (UNSAFE, "unsafe"),
    }
];

#[derive(Default)]
pub(crate) struct ModifierSet(pub(crate) u32);

impl ops::BitOrAssign<SemanticTokenModifier> for ModifierSet {
    fn bitor_assign(&mut self, rhs: SemanticTokenModifier) {
        let idx = SUPPORTED_MODIFIERS
            .iter()
            .position(|it| it == &rhs)
            .unwrap();
        self.0 |= 1 << idx;
    }
}

/// Tokens are encoded relative to each other.
///
/// This is a direct port of <https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45>
pub(crate) struct SemanticTokensBuilder<'a> {
    id: String,
    prev_line: u32,
    prev_char: u32,
    data: Vec<SemanticToken>,
    line_index: &'a LineIndex,
}

impl<'a> SemanticTokensBuilder<'a> {
    pub(crate) fn new(id: String, line_index: &'a LineIndex) -> Self {
        SemanticTokensBuilder {
            id,
            prev_line: 0,
            prev_char: 0,
            data: Default::default(),
            line_index,
        }
    }

    /// Push a new token onto the builder
    pub(crate) fn push_with_mods(
        &mut self,
        text_range: TextRange,
        token_type: SemanticTokenType,
        modifier_bitset: u32,
    ) {
        let range = self.line_index.range(text_range);
        let token_index = type_index(token_type);
        let mut push_line = range.start.line;
        let mut push_char = range.start.character;

        if !self.data.is_empty() {
            push_line -= self.prev_line;
            if push_line == 0 {
                push_char -= self.prev_char;
            }
        }

        // A token cannot be multiline
        let token_len = range.end.character - range.start.character;

        let token = SemanticToken {
            delta_line: push_line,
            delta_start: push_char,
            length: token_len,
            token_type: token_index,
            token_modifiers_bitset: modifier_bitset,
        };

        self.data.push(token);

        self.prev_line = range.start.line;
        self.prev_char = range.start.character;
    }

    pub(crate) fn push(&mut self, text_range: TextRange, token_type: SemanticTokenType) {
        self.push_with_mods(text_range, token_type, 0);
    }

    pub(crate) fn build(self) -> SemanticTokens {
        SemanticTokens {
            result_id: Some(self.id),
            data: self.data,
        }
    }
}

#[allow(dead_code)]
pub(crate) fn diff_tokens(old: &[SemanticToken], new: &[SemanticToken]) -> Vec<SemanticTokensEdit> {
    let offset = new
        .iter()
        .zip(old.iter())
        .take_while(|&(n, p)| n == p)
        .count();

    let (_, old) = old.split_at(offset);
    let (_, new) = new.split_at(offset);

    let offset_from_end = new
        .iter()
        .rev()
        .zip(old.iter().rev())
        .take_while(|&(n, p)| n == p)
        .count();

    let (old, _) = old.split_at(old.len() - offset_from_end);
    let (new, _) = new.split_at(new.len() - offset_from_end);

    if old.is_empty() && new.is_empty() {
        vec![]
    } else {
        // The lsp data field is actually a byte-diff but we
        // travel in tokens so `start` and `delete_count` are in multiples of the
        // serialized size of `SemanticToken`.
        vec![SemanticTokensEdit {
            start: 5 * offset as u32,
            delete_count: 5 * old.len() as u32,
            data: Some(new.into()),
        }]
    }
}

pub(crate) fn type_index(ty: SemanticTokenType) -> u32 {
    SUPPORTED_TYPES.iter().position(|it| *it == ty).unwrap() as u32
}

fn punctuation(builder: &mut SemanticTokensBuilder, tok: SyntaxToken) {
    match tok.kind() {
        T!['{'] | T!['}'] => builder.push(tok.text_range(), BRACE),
        T!['('] | T![')'] => builder.push(tok.text_range(), PARENTHESIS),
        T!['['] | T![']'] => builder.push(tok.text_range(), BRACKET),
        T![!] => builder.push(tok.text_range(), OPERATOR),
        T![:] => builder.push(tok.text_range(), COLON),
        T![;] => builder.push(tok.text_range(), SEMICOLON),
        T![,] => builder.push(tok.text_range(), COMMA),
        T![:=] => builder.push(tok.text_range(), OPERATOR),
        _ => builder.push(tok.text_range(), PUNCTUATION),
    }
}

fn keyword(builder: &mut SemanticTokensBuilder, tok: SyntaxToken) {
    builder.push(tok.text_range(), KEYWORD);
}

fn literal(builder: &mut SemanticTokensBuilder, tok: SyntaxToken) {
    match tok.kind() {
        SyntaxKind::STRING => builder.push(tok.text_range(), STRING),
        SyntaxKind::CHAR => builder.push(tok.text_range(), CHAR),
        _ => todo!(),
    }
}

fn token(builder: &mut SemanticTokensBuilder, tok: SyntaxToken) {
    let kind = tok.kind();
    if kind.is_keyword() {
        keyword(builder, tok);
    } else if kind.is_punct() {
        punctuation(builder, tok);
    } else if kind.is_literal() {
        literal(builder, tok);
    } else if kind == SyntaxKind::COMMENT {
        builder.push(tok.text_range(), COMMENT);
    } else if kind == SyntaxKind::IDENT {
        //FIXME: we shouldn't really hit this case
        builder.push(tok.text_range(), VARIABLE);
    } else {
        log::warn!("unhandled token: {:?}", kind);
    }
}

fn traverse(builder: &mut SemanticTokensBuilder, node: &SyntaxNode) {
    for event in node.preorder_with_tokens() {
        use WalkEvent::{Enter, Leave};

        let element = match event {
            Enter(NodeOrToken::Token(tok)) if tok.kind() == SyntaxKind::WHITESPACE => continue,
            Enter(n) => n,
            Leave(_) => continue,
        };

        match element {
            NodeOrToken::Node(n) => {
                if let Some(id) = ast::Identifier::cast(n.clone()) {
                    identifier(builder, id);
                } else {
                    if let Some(meta) = ast::MetaIdent::cast(n) {
                        builder.push(meta.syntax().text_range(), MACRO);
                    }
                    continue;
                }
            }
            NodeOrToken::Token(tok) => token(builder, tok),
        };
    }
}

fn identifier(builder: &mut SemanticTokensBuilder, ident: ast::Identifier) {
    let Some(parent) = ident.syntax().parent() else {
        return;
    };
    match_ast! {
        match parent {
            ast::DeclareDir(_d) => {
                builder.push(ident.syntax().text_range(), FUNCTION);
                return;
            },
            ast::Sort(_f) => {
                builder.push(ident.syntax().text_range(), TYPE);
                return;
            },
            _ => {}
        }
    }
    builder.push(ident.syntax().text_range(), VARIABLE);
}

pub(crate) fn semantic_tokens_for_file(
    file: syntax::SourceFile,
    index: &LineIndex,
) -> SemanticTokens {
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    let mut builder = SemanticTokensBuilder::new(
        COUNTER
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
            .to_string(),
        index,
    );

    traverse(&mut builder, file.syntax());

    builder.build()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn from(t: (u32, u32, u32, u32, u32)) -> SemanticToken {
        SemanticToken {
            delta_line: t.0,
            delta_start: t.1,
            length: t.2,
            token_type: t.3,
            token_modifiers_bitset: t.4,
        }
    }

    #[test]
    fn test_diff_insert_at_end() {
        let before = [from((1, 2, 3, 4, 5)), from((6, 7, 8, 9, 10))];
        let after = [
            from((1, 2, 3, 4, 5)),
            from((6, 7, 8, 9, 10)),
            from((11, 12, 13, 14, 15)),
        ];

        let edits = diff_tokens(&before, &after);
        assert_eq!(
            edits[0],
            SemanticTokensEdit {
                start: 10,
                delete_count: 0,
                data: Some(vec![from((11, 12, 13, 14, 15))])
            }
        );
    }

    #[test]
    fn test_diff_insert_at_beginning() {
        let before = [from((1, 2, 3, 4, 5)), from((6, 7, 8, 9, 10))];
        let after = [
            from((11, 12, 13, 14, 15)),
            from((1, 2, 3, 4, 5)),
            from((6, 7, 8, 9, 10)),
        ];

        let edits = diff_tokens(&before, &after);
        assert_eq!(
            edits[0],
            SemanticTokensEdit {
                start: 0,
                delete_count: 0,
                data: Some(vec![from((11, 12, 13, 14, 15))])
            }
        );
    }

    #[test]
    fn test_diff_insert_in_middle() {
        let before = [from((1, 2, 3, 4, 5)), from((6, 7, 8, 9, 10))];
        let after = [
            from((1, 2, 3, 4, 5)),
            from((10, 20, 30, 40, 50)),
            from((60, 70, 80, 90, 100)),
            from((6, 7, 8, 9, 10)),
        ];

        let edits = diff_tokens(&before, &after);
        assert_eq!(
            edits[0],
            SemanticTokensEdit {
                start: 5,
                delete_count: 0,
                data: Some(vec![
                    from((10, 20, 30, 40, 50)),
                    from((60, 70, 80, 90, 100))
                ])
            }
        );
    }

    #[test]
    fn test_diff_remove_from_end() {
        let before = [
            from((1, 2, 3, 4, 5)),
            from((6, 7, 8, 9, 10)),
            from((11, 12, 13, 14, 15)),
        ];
        let after = [from((1, 2, 3, 4, 5)), from((6, 7, 8, 9, 10))];

        let edits = diff_tokens(&before, &after);
        assert_eq!(
            edits[0],
            SemanticTokensEdit {
                start: 10,
                delete_count: 5,
                data: Some(vec![])
            }
        );
    }

    #[test]
    fn test_diff_remove_from_beginning() {
        let before = [
            from((11, 12, 13, 14, 15)),
            from((1, 2, 3, 4, 5)),
            from((6, 7, 8, 9, 10)),
        ];
        let after = [from((1, 2, 3, 4, 5)), from((6, 7, 8, 9, 10))];

        let edits = diff_tokens(&before, &after);
        assert_eq!(
            edits[0],
            SemanticTokensEdit {
                start: 0,
                delete_count: 5,
                data: Some(vec![])
            }
        );
    }

    #[test]
    fn test_diff_remove_from_middle() {
        let before = [
            from((1, 2, 3, 4, 5)),
            from((10, 20, 30, 40, 50)),
            from((60, 70, 80, 90, 100)),
            from((6, 7, 8, 9, 10)),
        ];
        let after = [from((1, 2, 3, 4, 5)), from((6, 7, 8, 9, 10))];

        let edits = diff_tokens(&before, &after);
        assert_eq!(
            edits[0],
            SemanticTokensEdit {
                start: 5,
                delete_count: 10,
                data: Some(vec![])
            }
        );
    }
}
