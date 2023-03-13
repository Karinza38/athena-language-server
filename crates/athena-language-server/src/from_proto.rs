use crate::Result;
use tower_lsp::lsp_types;
use vfs::{AbsPathBuf, FileId};

pub(crate) fn abs_path(url: &lsp_types::Url) -> Result<AbsPathBuf> {
    let path = url
        .to_file_path()
        .map_err(|()| format!("invalid file path: {}", url))?;

    Ok(AbsPathBuf::try_from(path).unwrap())
}

pub(crate) fn vfs_path(url: &lsp_types::Url) -> Result<vfs::VfsPath> {
    abs_path(url).map(vfs::VfsPath::from)
}

pub(crate) fn url_to_file_id(vfs: &vfs::Vfs, url: &lsp_types::Url) -> Result<FileId> {
    let path = vfs_path(url)?;
    vfs.file_id(&path)
        .ok_or_else(|| format!("file not found: {}", url).into())
}
