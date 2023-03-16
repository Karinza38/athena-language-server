mod fixture;

pub use dissimilar::diff as __diff;
pub use rustc_hash::FxHashMap;

use text_size::{TextRange, TextSize};

pub use crate::fixture::Fixture;

pub const CURSOR_MARKER: &str = "$0";
pub const ESCAPED_CURSOR_MARKER: &str = "\\$0";

/// Asserts that two strings are equal, otherwise displays a rich diff between them.
///
/// The diff shows changes from the "original" left string to the "actual" right string.
///
/// All arguments starting from and including the 3rd one are passed to
/// `eprintln!()` macro in case of text inequality.
#[macro_export]
macro_rules! assert_eq_text {
    ($left:expr, $right:expr) => {
        assert_eq_text!($left, $right,)
    };
    ($left:expr, $right:expr, $($tt:tt)*) => {{
        let left = $left;
        let right = $right;
        if left != right {
            if left.trim() == right.trim() {
                std::eprintln!("Left:\n{:?}\n\nRight:\n{:?}\n\nWhitespace difference\n", left, right);
            } else {
                let diff = $crate::__diff(left, right);
                std::eprintln!("Left:\n{}\n\nRight:\n{}\n\nDiff:\n{}\n", left, right, $crate::format_diff(diff));
            }
            std::eprintln!($($tt)*);
            panic!("text differs");
        }
    }};
}

/// Infallible version of `try_extract_offset()`.
pub fn extract_offset(text: &str) -> (TextSize, String) {
    match try_extract_offset(text) {
        None => panic!("text should contain cursor marker"),
        Some(result) => result,
    }
}

/// Returns the offset of the first occurrence of `$0` marker and the copy of `text`
/// without the marker.
fn try_extract_offset(text: &str) -> Option<(TextSize, String)> {
    let cursor_pos = text.find(CURSOR_MARKER)?;
    let mut new_text = String::with_capacity(text.len() - CURSOR_MARKER.len());
    new_text.push_str(&text[..cursor_pos]);
    new_text.push_str(&text[cursor_pos + CURSOR_MARKER.len()..]);
    let cursor_pos = TextSize::from(cursor_pos as u32);
    Some((cursor_pos, new_text))
}

/// Infallible version of `try_extract_range()`.
pub fn extract_range(text: &str) -> (TextRange, String) {
    match try_extract_range(text) {
        None => panic!("text should contain cursor marker"),
        Some(result) => result,
    }
}

/// Returns `TextRange` between the first two markers `$0...$0` and the copy
/// of `text` without both of these markers.
fn try_extract_range(text: &str) -> Option<(TextRange, String)> {
    let (start, text) = try_extract_offset(text)?;
    let (end, text) = try_extract_offset(&text)?;
    Some((TextRange::new(start, end), text))
}

#[derive(Clone, Copy)]
pub enum RangeOrOffset {
    Range(TextRange),
    Offset(TextSize),
}

impl RangeOrOffset {
    pub fn expect_offset(self) -> TextSize {
        match self {
            RangeOrOffset::Offset(it) => it,
            RangeOrOffset::Range(_) => panic!("expected an offset but got a range instead"),
        }
    }
    pub fn expect_range(self) -> TextRange {
        match self {
            RangeOrOffset::Range(it) => it,
            RangeOrOffset::Offset(_) => panic!("expected a range but got an offset"),
        }
    }
    pub fn range_or_empty(self) -> TextRange {
        match self {
            RangeOrOffset::Range(range) => range,
            RangeOrOffset::Offset(offset) => TextRange::empty(offset),
        }
    }
}

impl From<RangeOrOffset> for TextRange {
    fn from(selection: RangeOrOffset) -> Self {
        match selection {
            RangeOrOffset::Range(it) => it,
            RangeOrOffset::Offset(it) => TextRange::empty(it),
        }
    }
}

/// Extracts `TextRange` or `TextSize` depending on the amount of `$0` markers
/// found in `text`.
///
/// # Panics
/// Panics if no `$0` marker is present in the `text`.
pub fn extract_range_or_offset(text: &str) -> (RangeOrOffset, String) {
    if let Some((range, text)) = try_extract_range(text) {
        return (RangeOrOffset::Range(range), text);
    }
    let (offset, text) = extract_offset(text);
    (RangeOrOffset::Offset(offset), text)
}
