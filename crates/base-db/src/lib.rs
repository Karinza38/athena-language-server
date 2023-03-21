pub mod fixture;

pub use paths::{AbsPath, AbsPathBuf};
pub use salsa::{self, Cancelled};
use std::sync::Arc;
use syntax::{ast, Parse, TextRange, TextSize};

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl $crate::salsa::InternKey for $name {
            fn from_intern_id(v: $crate::salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> $crate::salsa::InternId {
                self.0
            }
        }
    };
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[derive(Clone, Copy, Debug)]
pub struct FilePosition {
    pub file_id: FileId,
    pub offset: TextSize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}

pub const DEFAULT_LRU_CAP: usize = 128;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub salsa::InternId);

impl_intern_key!(FileId);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VfsPath {
    Real(AbsPathBuf),
    Virtual(VirtualFilePath),
}

impl VfsPath {
    pub fn as_real_path(&self) -> Option<&AbsPath> {
        match self {
            VfsPath::Real(path) => Some(path.as_path()),
            _ => None,
        }
    }

    pub fn to_real_path(&self) -> Option<AbsPathBuf> {
        match self {
            VfsPath::Real(path) => Some(path.clone()),
            _ => None,
        }
    }
}

impl From<AbsPathBuf> for VfsPath {
    fn from(value: AbsPathBuf) -> Self {
        Self::Real(value)
    }
}

impl From<VirtualFilePath> for VfsPath {
    fn from(value: VirtualFilePath) -> Self {
        Self::Virtual(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VirtualFilePath(String);

impl From<String> for VirtualFilePath {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub trait FileWatcher {
    fn did_change_file(&mut self, file_id: FileId);

    fn in_mem_contents(&self, path: &AbsPath) -> Option<Arc<String>>;

    fn set_in_mem_contents(&mut self, path: AbsPathBuf, contents: Arc<String>);
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: std::fmt::Debug + FileWatcher {
    #[salsa::invoke(parse_query)]
    fn parse(&self, file_id: FileId) -> Parse<ast::SourceFile>;

    #[salsa::interned]
    fn intern_path(&self, path: VfsPath) -> FileId;

    #[salsa::input]
    fn virtual_file_contents(&self, file: VirtualFilePath) -> Arc<String>;

    fn file_contents(&self, file: FileId) -> Arc<String>;
}

fn file_contents(db: &dyn SourceDatabase, file: FileId) -> Arc<String> {
    db.salsa_runtime()
        .report_synthetic_read(salsa::Durability::LOW);

    let path = db.lookup_intern_path(file);

    let path = match path {
        VfsPath::Real(path) => path,
        VfsPath::Virtual(path) => return db.virtual_file_contents(path),
    };

    if let Some(text) = db.in_mem_contents(&path) {
        return text;
    }

    let text = std::fs::read_to_string(path.as_path()).unwrap();
    Arc::new(text)
}

fn parse_query(db: &dyn SourceDatabase, file_id: FileId) -> Parse<ast::SourceFile> {
    let text = db.file_contents(file_id);
    ast::SourceFile::parse(&text)
}
