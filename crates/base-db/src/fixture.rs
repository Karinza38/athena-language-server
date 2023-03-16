//! A set of high-level utility fixture methods to use in tests.
use std::{mem, sync::Arc};

use test_utils::{
    extract_range_or_offset, Fixture, RangeOrOffset, CURSOR_MARKER, ESCAPED_CURSOR_MARKER,
};
use vfs::{file_set::FileSet, VfsPath};

use crate::{Change, FileId, FilePosition, FileRange, SourceDatabase, SourceRoot, SourceRootId};

pub const WORKSPACE: SourceRootId = SourceRootId(0);

pub trait WithFixture: Default + SourceDatabase + 'static {
    fn with_single_file(ath_fixture: &str) -> (Self, FileId) {
        let fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.change.apply(&mut db);
        assert_eq!(fixture.files.len(), 1);
        (db, fixture.files[0])
    }

    fn with_many_files(ath_fixture: &str) -> (Self, Vec<FileId>) {
        let fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.change.apply(&mut db);
        assert!(fixture.file_position.is_none());
        (db, fixture.files)
    }

    fn with_files(ath_fixture: &str) -> Self {
        let fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.change.apply(&mut db);
        assert!(fixture.file_position.is_none());
        db
    }

    fn with_position(ath_fixture: &str) -> (Self, FilePosition) {
        let (db, file_id, range_or_offset) = Self::with_range_or_offset(ath_fixture);
        let offset = range_or_offset.expect_offset();
        (db, FilePosition { file_id, offset })
    }

    fn with_range(ath_fixture: &str) -> (Self, FileRange) {
        let (db, file_id, range_or_offset) = Self::with_range_or_offset(ath_fixture);
        let range = range_or_offset.expect_range();
        (db, FileRange { file_id, range })
    }

    fn with_range_or_offset(ath_fixture: &str) -> (Self, FileId, RangeOrOffset) {
        let fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.change.apply(&mut db);
        let (file_id, range_or_offset) = fixture
            .file_position
            .expect("Could not find file position in fixture. Did you forget to add an `$0`?");
        (db, file_id, range_or_offset)
    }
}

impl<DB: SourceDatabase + Default + 'static> WithFixture for DB {}

pub struct ChangeFixture {
    pub file_position: Option<(FileId, RangeOrOffset)>,
    pub files: Vec<FileId>,
    pub change: Change,
}

impl ChangeFixture {
    pub fn parse(ath_fixture: &str) -> ChangeFixture {
        let fixture = Fixture::parse(ath_fixture);
        let mut change = Change::new();

        let mut files = Vec::new();

        let mut file_set = FileSet::default();
        let source_root_prefix = "/".to_string();
        let mut file_id = FileId(0);
        let mut roots = Vec::new();

        let mut file_position = None;

        for entry in fixture {
            let text = if entry.text.contains(CURSOR_MARKER) {
                if entry.text.contains(ESCAPED_CURSOR_MARKER) {
                    entry.text.replace(ESCAPED_CURSOR_MARKER, CURSOR_MARKER)
                } else {
                    let (range_or_offset, text) = extract_range_or_offset(&entry.text);
                    assert!(file_position.is_none());
                    file_position = Some((file_id, range_or_offset));
                    text
                }
            } else {
                entry.text.clone()
            };

            let meta = FileMeta::from(entry);
            assert!(meta.path.starts_with(&source_root_prefix));

            change.change_file(file_id, Some(Arc::new(text)));
            let path = VfsPath::new_virtual_path(meta.path);
            file_set.insert(file_id, path);
            files.push(file_id);
            file_id.0 += 1;
        }

        let root = SourceRoot::new(mem::take(&mut file_set));
        roots.push(root);
        change.set_roots(roots);

        ChangeFixture {
            file_position,
            files,
            change,
        }
    }
}

#[derive(Debug)]
struct FileMeta {
    path: String,
}

impl From<Fixture> for FileMeta {
    fn from(f: Fixture) -> FileMeta {
        FileMeta { path: f.path }
    }
}
