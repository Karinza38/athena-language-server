// 99% from rust-analyzer

use std::{cmp::Ordering, ops};

use syntax::TextRange;

use crate::{HlRange, HlTag};

pub(super) struct Highlights {
    root: Node,
}

struct Node {
    hl_range: HlRange,
    children: Vec<Node>,
}

impl Highlights {
    pub(super) fn new(range: TextRange) -> Highlights {
        Highlights {
            root: Node::new(HlRange {
                range,
                highlight: HlTag::None.into(),
            }),
        }
    }

    pub(super) fn add(&mut self, range: HlRange) {
        self.root.add(range);
    }

    pub(super) fn into_vec(self) -> Vec<HlRange> {
        let mut res = Vec::new();
        self.root.flatten(&mut res);
        res
    }
}

fn equal_range_by<T, F>(slice: &[T], mut key: F) -> ops::Range<usize>
where
    F: FnMut(&T) -> Ordering,
{
    let start = slice.partition_point(|it| key(it) == Ordering::Less);
    let len = slice[start..].partition_point(|it| key(it) == Ordering::Equal);
    start..start + len
}

impl Node {
    fn new(range: HlRange) -> Node {
        Node {
            hl_range: range,
            children: Vec::new(),
        }
    }

    fn add(&mut self, hl_range: HlRange) {
        assert!(self.hl_range.range.contains_range(hl_range.range));

        if let Some(last) = self.children.last_mut() {
            if last.hl_range.range.contains_range(hl_range.range) {
                return last.add(hl_range);
            }
            if last.hl_range.range.end() <= hl_range.range.start() {
                self.children.push(Node::new(hl_range));
                return;
            }
        }

        let overlapping = equal_range_by(&self.children, |n| {
            TextRange::ordering(n.hl_range.range, hl_range.range)
        });

        if overlapping.len() == 1
            && self.children[overlapping.start]
                .hl_range
                .range
                .contains_range(hl_range.range)
        {
            return self.children[overlapping.start].add(hl_range);
        }

        let children = self
            .children
            .splice(overlapping.clone(), std::iter::once(Node::new(hl_range)))
            .collect::<Vec<_>>();

        self.children[overlapping.start].children = children;
    }

    fn flatten(&self, acc: &mut Vec<HlRange>) {
        let mut start = self.hl_range.range.start();
        let mut children = self.children.iter();
        loop {
            let next = children.next();
            let end = next.map_or(self.hl_range.range.end(), |it| it.hl_range.range.start());
            if start < end {
                acc.push(HlRange {
                    range: TextRange::new(start, end),
                    highlight: self.hl_range.highlight,
                });
            }
            start = match next {
                Some(child) => {
                    child.flatten(acc);
                    child.hl_range.range.end()
                }
                None => break,
            }
        }
    }
}
