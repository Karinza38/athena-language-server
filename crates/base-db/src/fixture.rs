//! A set of high-level utility fixture methods to use in tests.
use std::sync::Arc;

use test_utils::{
    extract_range_or_offset, Fixture, RangeOrOffset, CURSOR_MARKER, ESCAPED_CURSOR_MARKER,
};

use crate::{FilePathId, FilePosition, FileRange, SourceDatabase, VirtualFilePath};

pub trait WithFixture: Default + SourceDatabase + 'static {
    fn with_single_file(ath_fixture: &str) -> (Self, FilePathId) {
        let mut fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.apply(&mut db);
        assert_eq!(fixture.files.len(), 1);
        (db, fixture.files[0])
    }

    fn with_many_files(ath_fixture: &str) -> (Self, Vec<FilePathId>) {
        let mut fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.apply(&mut db);
        assert!(fixture.file_position.is_none());
        (db, fixture.files)
    }

    fn with_files(ath_fixture: &str) -> Self {
        let mut fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.apply(&mut db);
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

    fn with_range_or_offset(ath_fixture: &str) -> (Self, FilePathId, RangeOrOffset) {
        let mut fixture = ChangeFixture::parse(ath_fixture);
        let mut db = Self::default();
        fixture.apply(&mut db);
        let (file_id, range_or_offset) = fixture
            .file_position
            .expect("Could not find file position in fixture. Did you forget to add an `$0`?");
        (db, file_id, range_or_offset)
    }
}

impl<DB: SourceDatabase + Default + 'static> WithFixture for DB {}

pub struct ChangeFixture {
    pub file_position: Option<(FilePathId, RangeOrOffset)>,
    file_paths: Vec<VirtualFilePath>,
    file_contents: Vec<String>,
    pub files: Vec<FilePathId>,
}

impl ChangeFixture {
    pub fn apply(&mut self, db: &mut dyn SourceDatabase) {
        assert_eq!(self.file_paths.len(), self.file_contents.len());
        for (idx, (path, text)) in self
            .file_paths
            .iter()
            .zip(self.file_contents.iter())
            .enumerate()
        {
            let id = db.intern_path(path.clone().into());
            assert_eq!(id.0.as_usize(), idx);
            db.set_virtual_file_contents(path.clone(), Arc::new(text.clone()));
            self.files.push(id);
        }
    }
    pub fn parse(ath_fixture: &str) -> ChangeFixture {
        let fixture = Fixture::parse(ath_fixture);

        let mut file_paths = Vec::new();
        let mut file_contents = Vec::new();

        let source_root_prefix = "/".to_string();
        let mut file_id = FilePathId(salsa::InternId::from(0u32));

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

            file_paths.push(meta.path.into());
            file_contents.push(text);
            file_id = FilePathId(salsa::InternId::from(file_id.0.as_u32() + 1));
        }

        ChangeFixture {
            file_paths,
            file_position,
            file_contents,
            files: vec![],
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
