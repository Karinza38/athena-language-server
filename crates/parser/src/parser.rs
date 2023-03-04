use crate::{event::Event, input::Input, SyntaxKind, token_set::TokenSet};
use drop_bomb::DropBomb;

pub(crate) struct Parser<'i> {
    input: &'i Input,
    pos: usize,
    events: Vec<Event>,
}

impl<'i> Parser<'i> {
    pub(crate) fn new(input: &'i Input) -> Self {
        Self {
            input,
            pos: 0,
            events: Vec::new(),
        }
    }

    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Finish parsing and return the events.
    pub(crate) fn finish(self) -> Vec<Event> {
        self.events
    }

    fn bump_impl(&mut self, kind: SyntaxKind) {
        self.pos += 1;
        self.push_event(Event::Token { kind });
    }

    /// Bump the current token and assert that it is `kind`
    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind));
    }

    /// Bump the current token if it is `kind`
    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump_impl(kind);
            true
        } else {
            false
        }
    }

    pub(crate) fn bump_any(&mut self) {
        let kind = self.current();
        if kind == SyntaxKind::EOF {
            return;
        }
        self.bump_impl(kind);
    }

    pub(crate) fn error<Msg: Into<String>>(&mut self, msg: Msg) {
        self.push_event(Event::Error { msg: msg.into() });
    }

    /// Bump the current token if it is `kind`, otherwise report an error
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump_impl(kind);
            true
        } else {
            self.error(format!("expected {:?}", kind));
            false
        }
    }

    /// Lookahead `n` tokens
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        self.input.kind(self.pos + n)
    }

    /// Check that the current token is `kind`
    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    /// Check that the `n`th token is `kind`
    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.input.kind(self.pos + n) == kind
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;
        self.push_event(Event::tombstone());
        Marker::new(pos)
    }

    pub(crate) fn at_one_of(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }
}

pub(crate) struct Marker {
    pos: u32,
    bomb: DropBomb,
}

impl Marker {
    fn new(pos: u32) -> Marker {
        Marker {
            pos,
            bomb: DropBomb::new("Marker must be completed or abandoned"),
        }
    }

    /// Finish the node and return a `CompletedMarker` which can be used to
    /// retroactively assign a parent to this node.
    pub(crate) fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();
        let idx = self.pos as usize;
        match &mut p.events[idx] {
            Event::Start { kind: slot, .. } => {
                *slot = kind;
            }
            _ => unreachable!(),
        }
        p.push_event(Event::Finish);

        CompletedMarker::new(self.pos, kind)
    }

    /// Abandon the node. The children are attached to its parent.
    pub(crate) fn abandon(mut self, p: &mut Parser<'_>) {
        self.bomb.defuse();
        let idx = self.pos as usize;
        if idx == p.events.len() - 1 {
            match p.events.pop() {
                Some(Event::Start {
                    kind: SyntaxKind::TOMBSTONE,
                    forward_parent: None,
                }) => {}
                _ => unreachable!(),
            }
        }
    }
}

pub(crate) struct CompletedMarker {
    pos: u32,
    kind: SyntaxKind,
}

impl CompletedMarker {
    fn new(pos: u32, kind: SyntaxKind) -> Self {
        Self { pos, kind }
    }

    /// This method allows to create a new node which starts
    /// *before* the current one. That is, parser could start
    /// node `A`, then complete it, and then after parsing the
    /// whole `A`, decide that it should have started some node
    /// `B` before starting `A`. `precede` allows to do exactly
    /// that. See also docs about
    /// [`Event::Start::forward_parent`](crate::event::Event::Start::forward_parent).
    ///
    /// Given completed events `[START, FINISH]` and its corresponding
    /// `CompletedMarker(pos: 0, _)`.
    /// Append a new `START` events as `[START, FINISH, NEWSTART]`,
    /// then mark `NEWSTART` as `START`'s parent with saving its relative
    /// distance to `NEWSTART` into forward_parent(=2 in this case);
    pub(crate) fn precede(self, p: &mut Parser<'_>) -> Marker {
        let new_pos = p.start();
        let idx = self.pos as usize;
        match &mut p.events[idx] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(new_pos.pos - self.pos);
            }
            _ => unreachable!(),
        }
        new_pos
    }

    /// Extends this completed marker *to the left* up to `m`.
    pub(crate) fn extend_to(self, p: &mut Parser<'_>, mut m: Marker) -> CompletedMarker {
        m.bomb.defuse();
        let idx = m.pos as usize;
        match &mut p.events[idx] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(self.pos - m.pos);
            }
            _ => unreachable!(),
        }
        self
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }
}
