use std::{fmt, sync::Arc};

use vfs::FileId;

use crate::{SourceDatabase, SourceRoot, SourceRootId};

#[derive(Default)]
pub struct Change {
    pub roots: Option<Vec<SourceRoot>>,
    pub files_changed: Vec<(FileId, Option<Arc<String>>)>,
}

impl fmt::Debug for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = f.debug_struct("Change");
        if let Some(roots) = &self.roots {
            debug.field("roots", roots);
        }
        if !self.files_changed.is_empty() {
            debug.field("files_changed", &self.files_changed.len());
        }
        debug.finish()
    }
}

impl Change {
    pub fn new() -> Change {
        Change::default()
    }

    pub fn set_roots(&mut self, roots: Vec<SourceRoot>) {
        self.roots = Some(roots);
    }

    pub fn change_file(&mut self, file_id: FileId, new_text: Option<Arc<String>>) {
        self.files_changed.push((file_id, new_text));
    }

    pub fn apply(self, db: &mut dyn SourceDatabase) {
        if let Some(roots) = self.roots {
            for (idx, root) in roots.into_iter().enumerate() {
                let root_id = SourceRootId(idx as u32);
                for file_id in root.iter() {
                    db.set_file_source_root(file_id, root_id);
                }
                db.set_source_root(root_id, Arc::new(root));
            }
        }

        for (file_id, text) in self.files_changed {
            let text = text.unwrap_or_default();
            db.set_file_text(file_id, text);
        }
    }
}
