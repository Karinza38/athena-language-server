mod go_to_definition;
mod helpers;
mod navigation_target;
mod syntax_highlighting;

#[cfg(test)]
mod fixture;

pub use ide_db::line_index::LineIndex;
use ide_db::{
    base_db::{AbsPathBuf, FileId, FileWatcher},
    LineIndexDatabase,
};

use core::fmt;
use std::{panic::UnwindSafe, sync::Arc};

pub use crate::syntax_highlighting::{
    tags::{Highlight, HlPunct, HlTag},
    HighlightConfig, HlRange,
};
pub use ide_db::{
    base_db::{
        salsa::{self, ParallelDatabase},
        Cancelled, FilePosition, FileRange, SourceDatabase,
    },
    RootDatabase,
};
use navigation_target::NavigationTarget;
use syntax::{Parse, SourceFile, TextRange};

pub type Cancellable<T> = Result<T, Cancelled>;

#[derive(Debug, Default)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    pub fn new() -> AnalysisHost {
        AnalysisHost {
            db: RootDatabase::new(),
        }
    }

    pub fn analysis(&self) -> Analysis {
        Analysis {
            db: self.db.snapshot(),
        }
    }

    pub fn raw_db(&self) -> &RootDatabase {
        &self.db
    }

    pub fn raw_db_mut(&mut self) -> &mut RootDatabase {
        &mut self.db
    }

    pub fn request_cancellation(&mut self) {
        self.db.request_cancellation()
    }

    pub fn did_change_file(&mut self, file: FileId) {
        self.request_cancellation();
        self.db.did_change_file(file)
    }
}

pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    pub fn file_text(&self, file_id: FileId) -> Cancellable<Arc<String>> {
        self.with_db(|db| db.file_contents(file_id))
    }

    pub fn parse(&self, file_id: FileId) -> Cancellable<Parse<SourceFile>> {
        self.with_db(|db| db.parse(file_id))
    }

    pub fn file_line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.with_db(|db| db.line_index(file_id))
    }

    pub fn intern_path(&self, path: AbsPathBuf) -> Cancellable<FileId> {
        self.with_db(|db| db.intern_path(path.into()))
    }

    pub fn file_path(&self, file_id: FileId) -> Cancellable<Option<AbsPathBuf>> {
        self.with_db(|db| db.lookup_intern_path(file_id).to_real_path())
    }

    /// Computes syntax highlighting for the given file
    pub fn highlight(&self, config: HighlightConfig, file_id: FileId) -> Cancellable<Vec<HlRange>> {
        self.with_db(|db| syntax_highlighting::highlight(db, config, file_id, None))
    }

    pub fn go_to_definition(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<RangeInfo<Vec<NavigationTarget>>>> {
        self.with_db(|db| go_to_definition::go_to_definition(db, position))
    }

    // wrapper to catch cancellation signals in case a change is applied
    fn with_db<F, T>(&self, f: F) -> Cancellable<T>
    where
        F: FnOnce(&RootDatabase) -> T + UnwindSafe,
    {
        Cancelled::catch(|| f(&self.db))
    }
}

impl fmt::Debug for Analysis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Analysis").finish()
    }
}

#[derive(Debug)]
pub struct RangeInfo<T> {
    pub range: TextRange,
    pub info: T,
}

impl<T> RangeInfo<T> {
    pub fn new(range: TextRange, info: T) -> Self {
        Self { range, info }
    }
}
