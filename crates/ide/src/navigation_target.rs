use ide_db::base_db::FileId;
use syntax::{SmolStr, TextRange};

pub struct NavigationTarget {
    pub file_id: FileId,

    pub full_range: TextRange,

    pub focus_range: Option<TextRange>,

    pub name: SmolStr,
}
