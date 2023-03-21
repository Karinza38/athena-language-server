use crate::{global_state::GlobalStateSnapshot, Result};
use anyhow::anyhow;
use ide_db::{
    base_db::{FileId, FilePosition},
    line_index::{LineColUtf16, LineIndex},
};
use paths::AbsPathBuf;
use syntax::TextSize;
use tower_lsp::lsp_types;

pub(crate) fn abs_path(url: &lsp_types::Url) -> Result<AbsPathBuf> {
    let path = url
        .to_file_path()
        .map_err(|()| anyhow!("invalid file path: {}", url))?;

    Ok(AbsPathBuf::try_from(path).unwrap())
}

pub(crate) fn offset(line_index: &LineIndex, position: lsp_types::Position) -> Result<TextSize> {
    let line_col = line_index.to_utf8(LineColUtf16 {
        line: position.line,
        col: position.character,
    });
    let offset = line_index
        .offset(line_col)
        .ok_or_else(|| anyhow!("Invalid offset"))?;

    Ok(offset)
}

pub(crate) fn file_id(snapshot: &GlobalStateSnapshot, url: &lsp_types::Url) -> Result<FileId> {
    snapshot.file_id(url)
}

pub(crate) fn file_position(
    snapshot: &GlobalStateSnapshot,
    position_params: lsp_types::TextDocumentPositionParams,
) -> Result<FilePosition> {
    let file_id = file_id(snapshot, &position_params.text_document.uri)?;

    let index = snapshot.analysis.file_line_index(file_id)?;

    let offset = offset(&index, position_params.position)?;
    Ok(FilePosition { file_id, offset })
}
