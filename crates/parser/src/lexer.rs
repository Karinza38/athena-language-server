mod generated;

use std::ops::Range;

use crate::SyntaxKind;
use generated::LexerToken;
use logos::Logos;

pub struct LexedInput<'i> {
    input: &'i str,
    kind: Vec<SyntaxKind>,
    start: Vec<u32>,
    error: Vec<LexerError>,
}

struct LexerError {
    msg: String,
    token: u32,
}

impl<'i> LexedInput<'i> {
    pub fn new(input: &'i str) -> LexedInput<'i> {
        let lexer = LexerToken::lexer(input);
        let mut kind = Vec::new();
        let mut start = Vec::new();
        let mut error = Vec::new();

        let mut pos = 0;
        for (i, token) in lexer.enumerate() {
            let syntax_kind = token.to_syntax_kind();
            if syntax_kind == SyntaxKind::ERROR {
                error.push(LexerError {
                    // FIXME: this is a terrible error message
                    msg: "lexer error".into(),
                    token: i as u32,
                });
            }
            let len = token.len();
            kind.push(syntax_kind);
            start.push(pos as u32);
            pos += len;
        }

        kind.push(SyntaxKind::EOF);
        start.push(pos as u32);

        LexedInput {
            input,
            kind,
            start,
            error,
        }
    }

    pub fn as_str(&self) -> &str {
        self.input
    }

    pub fn len(&self) -> usize {
        self.kind.len() - 1 // -1 for EOF
    }

    pub fn kind(&self, idx: usize) -> SyntaxKind {
        assert!(idx < self.len());
        self.kind[idx]
    }

    pub fn kinds(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        self.kind[..self.len()].iter().copied()
    }

    pub fn text_for_range(&self, range: Range<usize>) -> &str {
        assert!(range.start < range.end && range.end <= self.len());
        let start = self.start[range.start] as usize;
        let end = self.start[range.end] as usize;
        &self.input[start..end]
    }

    pub fn range_for_token(&self, idx: usize) -> Range<usize> {
        assert!(idx < self.len());
        let start = self.start[idx] as usize;
        let end = self.start[idx + 1] as usize;
        start..end
    }

    pub fn text(&self, idx: usize) -> &str {
        self.text_for_range(idx..idx + 1)
    }

    pub fn text_start(&self, idx: usize) -> usize {
        assert!(idx <= self.len());
        self.start[idx] as usize
    }

    pub fn text_len(&self, idx: usize) -> usize {
        assert!(idx < self.len());
        let r = self.range_for_token(idx);
        r.end - r.start
    }

    pub fn error(&self, idx: usize) -> Option<&str> {
        assert!(idx < self.len());
        let err = self
            .error
            .binary_search_by_key(&(idx as u32), |e| e.token)
            .ok()?;
        Some(self.error[err].msg.as_str())
    }

    pub fn errors(&self) -> impl Iterator<Item = (usize, &str)> + '_ {
        self.error
            .iter()
            .map(|err| (err.token as usize, err.msg.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::LexedInput;

    use crate::SyntaxKind::{self, *};

    fn assert_lex<const N: usize>(input: &str, expected: [SyntaxKind; N]) {
        let lexed = LexedInput::new(input);
        assert_eq!(lexed.len(), N);
        for (i, kind) in expected.iter().enumerate() {
            assert_eq!(lexed.kind(i), *kind);
        }
    }

    #[test]
    fn successful_lex() {
        assert_lex(
            "module Foo { } # comment",
            [
                MODULE_KW, WHITESPACE, IDENT, WHITESPACE, L_CURLY, WHITESPACE, R_CURLY, WHITESPACE,
                COMMENT,
            ],
        );
    }
}
