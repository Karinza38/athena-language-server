use ide::Cancellable;
use ide_db::{
    base_db::{FilePathId, FileRange},
    line_index::LineIndex,
};
use paths::AbsPath;
use syntax::{TextRange, TextSize};
use tower_lsp::lsp_types;

use crate::global_state::GlobalStateSnapshot;

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

pub(crate) fn url(snap: &GlobalStateSnapshot, file_id: FilePathId) -> lsp_types::Url {
    snap.file_id_to_url2(file_id)
}

pub(crate) fn location(
    snap: &GlobalStateSnapshot,
    frange: FileRange,
) -> Cancellable<lsp_types::Location> {
    let url = url(snap, frange.file_id);
    let line_index = snap.file_line_index(frange.file_id)?;
    let range = range(&line_index, frange.range);
    let loc = lsp_types::Location::new(url, range);
    Ok(loc)
}

pub(crate) fn url_from_abs_path(path: &AbsPath) -> lsp_types::Url {
    lsp_types::Url::from_file_path(path.to_path_buf()).unwrap()
}
