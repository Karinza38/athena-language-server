mod navigation_target;
mod helpers;
mod go_to_definition;

pub use ide_db::line_index::LineIndex;

use core::fmt;
use std::{panic::UnwindSafe, sync::Arc};

pub use ide_db::{
    base_db::{
        salsa::{self, ParallelDatabase},
        Cancelled, Change, FileId, FilePosition, SourceDatabase,
    },
    LineIndexDatabase, RootDatabase,
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

    pub fn apply_change(&mut self, change: Change) {
        self.db.apply_change(change)
    }
}

pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    pub fn file_text(&self, file_id: FileId) -> Cancellable<Arc<String>> {
        self.with_db(|db| db.file_text(file_id))
    }

    pub fn parse(&self, file_id: FileId) -> Cancellable<Parse<SourceFile>> {
        self.with_db(|db| db.parse(file_id))
    }

    pub fn file_line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.with_db(|db| db.line_index(file_id))
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
