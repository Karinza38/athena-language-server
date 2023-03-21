use ide_db::base_db::FilePathId;
use syntax::{SmolStr, TextRange};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NavigationTarget {
    pub file_id: FilePathId,

    pub full_range: TextRange,

    pub focus_range: Option<TextRange>,

    pub name: SmolStr,
}

impl NavigationTarget {
    pub fn focus_or_full_range(&self) -> TextRange {
        self.focus_range.unwrap_or(self.full_range)
    }
}
