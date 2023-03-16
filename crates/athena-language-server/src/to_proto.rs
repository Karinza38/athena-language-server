use ide_db::line_index::LineIndex;
use syntax::{TextRange, TextSize};
use tower_lsp::lsp_types;

pub(crate) fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.to_utf16(line_index.line_col(offset));
    lsp_types::Position::new(line_col.line, line_col.col)
}

pub fn range(line_index: &LineIndex, text_range: TextRange) -> lsp_types::Range {
    lsp_types::Range::new(
        position(line_index, text_range.start()),
        position(line_index, text_range.end()),
    )
}
