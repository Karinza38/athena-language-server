use std::sync::atomic::{AtomicU32, Ordering};

use ide::{Cancellable, Highlight, HlRange};
use ide_db::{
    base_db::{FileId, FileRange},
    line_index::LineIndex,
    SymbolKind,
};
use paths::AbsPath;
use syntax::{TextRange, TextSize};
use tower_lsp::lsp_types;

use crate::{global_state::GlobalStateSnapshot, semantic_tokens};

pub(crate) fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.to_utf16(line_index.line_col(offset));
    lsp_types::Position::new(line_col.line, line_col.col)
}

pub(crate) fn range(line_index: &LineIndex, text_range: TextRange) -> lsp_types::Range {
    lsp_types::Range::new(
        position(line_index, text_range.start()),
        position(line_index, text_range.end()),
    )
}

pub(crate) fn url(snap: &GlobalStateSnapshot, file_id: FileId) -> Cancellable<lsp_types::Url> {
    snap.file_id_to_url(file_id)
}

pub(crate) fn location(
    snap: &GlobalStateSnapshot,
    frange: FileRange,
) -> Cancellable<lsp_types::Location> {
    let url = url(snap, frange.file_id)?;
    let line_index = snap.file_line_index(frange.file_id)?;
    let range = range(&line_index, frange.range);
    let loc = lsp_types::Location::new(url, range);
    Ok(loc)
}

pub(crate) fn url_from_abs_path(path: &AbsPath) -> lsp_types::Url {
    lsp_types::Url::from_file_path(path.to_path_buf()).unwrap()
}

static TOKEN_RESULT_COUNTER: AtomicU32 = AtomicU32::new(1);

pub(crate) fn semantic_tokens(
    text: &str,
    line_index: &LineIndex,
    highlights: Vec<HlRange>,
) -> lsp_types::SemanticTokens {
    let id = TOKEN_RESULT_COUNTER
        .fetch_add(1, Ordering::Relaxed)
        .to_string();

    let mut builder = semantic_tokens::SemanticTokensBuilder::new(id, line_index);

    for highlight_range in highlights {
        if highlight_range.highlight.is_empty() {
            continue;
        }

        let (ty, mods) = semantic_token_type_and_modifiers(highlight_range.highlight);
        let modifier_bitset = mods.0;

        for mut text_range in line_index.lines(highlight_range.range) {
            if text[text_range].ends_with('\n') {
                text_range =
                    TextRange::new(text_range.start(), text_range.end() - TextSize::of('\n'));
            }
            builder.push_with_mods(text_range, ty.clone(), modifier_bitset);
        }
    }

    builder.build()
}

fn semantic_token_type_and_modifiers(
    highlight: Highlight,
) -> (lsp_types::SemanticTokenType, semantic_tokens::ModifierSet) {
    let mods = semantic_tokens::ModifierSet::default();

    let type_ = match highlight.tag {
        ide::HlTag::Symbol(sym) => match sym {
            SymbolKind::FnSym => semantic_tokens::ENUM_MEMBER,
            SymbolKind::Value => semantic_tokens::VARIABLE,
            SymbolKind::Sort => semantic_tokens::TYPE,
            SymbolKind::Func => semantic_tokens::METHOD,
            SymbolKind::Module => semantic_tokens::NAMESPACE,
            SymbolKind::Const => semantic_tokens::ENUM_MEMBER,
        },
        ide::HlTag::Comment => semantic_tokens::COMMENT,
        ide::HlTag::Keyword => semantic_tokens::KEYWORD,
        ide::HlTag::StringLiteral => semantic_tokens::STRING,
        ide::HlTag::CharLiteral => semantic_tokens::CHAR,
        ide::HlTag::IdentLiteral => semantic_tokens::CHAR,
        ide::HlTag::NumberLiteral => semantic_tokens::NUMBER,
        ide::HlTag::Punct(punct) => match punct {
            ide::HlPunct::Bracket => semantic_tokens::BRACKET,
            ide::HlPunct::Brace => semantic_tokens::BRACE,
            ide::HlPunct::Parenthesis => semantic_tokens::PARENTHESIS,
            ide::HlPunct::Comma => semantic_tokens::COMMA,
            ide::HlPunct::Dot => semantic_tokens::DOT,
            ide::HlPunct::Colon => semantic_tokens::COLON,
            ide::HlPunct::Semi => semantic_tokens::SEMICOLON,
            ide::HlPunct::Bang => semantic_tokens::MACRO_BANG,
            ide::HlPunct::ColonEq => semantic_tokens::OPERATOR,
            ide::HlPunct::Other => semantic_tokens::PUNCTUATION,
        },
        ide::HlTag::None => semantic_tokens::GENERIC,
    };
    (type_, mods)
}
