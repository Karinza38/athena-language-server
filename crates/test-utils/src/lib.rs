// A bunch of utilities for testing
// 99% taken from rust-analyzer

mod fixture;

use std::collections::BTreeMap;

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

/// Extracts `//^^^ some text` annotations.
///
/// A run of `^^^` can be arbitrary long and points to the corresponding range
/// in the line above.
///
/// The `// ^file text` syntax can be used to attach `text` to the entirety of
/// the file.
///
/// Multiline string values are supported:
///
/// // ^^^ first line
/// //   | second line
///
/// Trailing whitespace is sometimes desired but usually stripped by the editor
/// if at the end of a line, or incorrectly sized if followed by another
/// annotation. In those cases the annotation can be explicitly ended with the
/// `$` character.
///
/// // ^^^ trailing-ws-wanted  $
///
/// Annotations point to the last line that actually was long enough for the
/// range, not counting annotations themselves. So overlapping annotations are
/// possible:
/// ```no_run
/// // stuff        other stuff
/// // ^^ 'st'
/// // ^^^^^ 'stuff'
/// //              ^^^^^^^^^^^ 'other stuff'
/// ```
pub fn extract_annotations(text: &str) -> Vec<(TextRange, String)> {
    let mut res = Vec::new();
    // map from line length to beginning of last line that had that length
    let mut line_start_map = BTreeMap::new();
    let mut line_start: TextSize = 0.into();
    let mut prev_line_annotations: Vec<(TextSize, usize)> = Vec::new();
    for line in text.split_inclusive('\n') {
        let mut this_line_annotations = Vec::new();
        let line_length = if let Some((prefix, suffix)) = line.split_once("//") {
            let ss_len = TextSize::of("//");
            let annotation_offset = TextSize::of(prefix) + ss_len;
            for annotation in extract_line_annotations(suffix.trim_end_matches('\n')) {
                match annotation {
                    LineAnnotation::Annotation {
                        mut range,
                        content,
                        file,
                    } => {
                        range += annotation_offset;
                        this_line_annotations.push((range.end(), res.len()));
                        let range = if file {
                            TextRange::up_to(TextSize::of(text))
                        } else {
                            let line_start = line_start_map.range(range.end()..).next().unwrap();

                            range + line_start.1
                        };
                        res.push((range, content));
                    }
                    LineAnnotation::Continuation {
                        mut offset,
                        content,
                    } => {
                        offset += annotation_offset;
                        let &(_, idx) = prev_line_annotations
                            .iter()
                            .find(|&&(off, _idx)| off == offset)
                            .unwrap();
                        res[idx].1.push('\n');
                        res[idx].1.push_str(&content);
                        res[idx].1.push('\n');
                    }
                }
            }
            annotation_offset
        } else {
            TextSize::of(line)
        };

        line_start_map = line_start_map.split_off(&line_length);
        line_start_map.insert(line_length, line_start);

        line_start += TextSize::of(line);

        prev_line_annotations = this_line_annotations;
    }

    res
}

enum LineAnnotation {
    Annotation {
        range: TextRange,
        content: String,
        file: bool,
    },
    Continuation {
        offset: TextSize,
        content: String,
    },
}

fn extract_line_annotations(mut line: &str) -> Vec<LineAnnotation> {
    let mut res = Vec::new();
    let mut offset: TextSize = 0.into();
    let marker: fn(char) -> bool = if line.contains('^') {
        |c| c == '^'
    } else {
        |c| c == '|'
    };
    while let Some(idx) = line.find(marker) {
        offset += TextSize::try_from(idx).unwrap();
        line = &line[idx..];

        let mut len = line.chars().take_while(|&it| it == '^').count();
        let mut continuation = false;
        if len == 0 {
            assert!(line.starts_with('|'));
            continuation = true;
            len = 1;
        }
        let range = TextRange::at(offset, len.try_into().unwrap());
        let line_no_caret = &line[len..];
        let end_marker = line_no_caret.find(|c| c == '$');
        let next = line_no_caret.find(marker).map_or(line.len(), |it| it + len);

        let cond = |end_marker| {
            end_marker < next
                && (line_no_caret[end_marker + 1..].is_empty()
                    || line_no_caret[end_marker + 1..]
                        .strip_prefix(|c: char| c.is_whitespace() || c == '^')
                        .is_some())
        };
        let mut content = match end_marker {
            Some(end_marker) if cond(end_marker) => &line_no_caret[..end_marker],
            _ => line_no_caret[..next - len].trim_end(),
        };

        let mut file = false;
        if !continuation && content.starts_with("file") {
            file = true;
            content = &content["file".len()..];
        }

        let content = content.trim_start().to_string();

        let annotation = if continuation {
            LineAnnotation::Continuation {
                offset: range.end(),
                content,
            }
        } else {
            LineAnnotation::Annotation {
                range,
                content,
                file,
            }
        };
        res.push(annotation);

        line = &line[next..];
        offset += TextSize::try_from(next).unwrap();
    }

    res
}

#[test]
fn test_extract_annotations_1() {
    let text = util::trim_indent(
        r#"
fn main() {
    let (x,     y) = (9, 2);
       //^ def  ^ def
    zoo + 1
} //^^^ type:
  //  | i32

// ^file
    "#,
    );
    let res = extract_annotations(&text)
        .into_iter()
        .map(|(range, ann)| (&text[range], ann))
        .collect::<Vec<_>>();

    assert_eq!(
        res[..3],
        [
            ("x", "def".into()),
            ("y", "def".into()),
            ("zoo", "type:\ni32\n".into())
        ]
    );
    assert_eq!(res[3].0.len(), 115);
}

#[test]
fn test_extract_annotations_2() {
    let text = util::trim_indent(
        r#"
fn main() {
    (x,   y);
   //^ a
      //  ^ b
  //^^^^^^^^ c
}"#,
    );
    let res = extract_annotations(&text)
        .into_iter()
        .map(|(range, ann)| (&text[range], ann))
        .collect::<Vec<_>>();

    assert_eq!(
        res,
        [
            ("x", "a".into()),
            ("y", "b".into()),
            ("(x,   y)", "c".into())
        ]
    );
}
