use std::cell::Cell;

use crate::{event::Event, input::Input, token_set::TokenSet, SyntaxKind, T};
use drop_bomb::DropBomb;

pub(crate) struct Parser<'i> {
    input: &'i Input,
    pos: usize,
    events: Vec<Event>,
    steps: Cell<u32>,
}

const STEP_LIMIT: u32 = 100_000;

impl<'i> Parser<'i> {
    pub(crate) fn new(input: &'i Input) -> Self {
        Self {
            input,
            pos: 0,
            events: Vec::new(),
            steps: Cell::new(0),
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

    /// Advances the parser by one token, remapping its kind.
    /// This is useful to create contextual keywords from
    /// identifiers.
    pub(crate) fn bump_remap(&mut self, kind: SyntaxKind) {
        if self.nth(0) == SyntaxKind::EOF {
            // FIXME: panic!?
            return;
        }
        self.bump_impl(kind);
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

    pub(crate) fn at_end(&self) -> bool {
        self.current() == SyntaxKind::EOF
    }

    pub(crate) fn bump_one_of(&mut self, set: TokenSet) {
        let kind = self.current();
        assert!(set.contains(kind));
        self.bump_impl(kind);
    }

    pub(crate) fn bump_any(&mut self) {
        let kind = self.current();
        if kind == SyntaxKind::EOF {
            return;
        }
        self.bump_impl(kind);
    }

    /// Create an error node and consume the next token.
    pub(crate) fn err_recover(&mut self, message: &str, recovery: TokenSet) {
        match self.current() {
            T!['{'] | T!['}'] => {
                self.error(message);
                return;
            }
            _ => (),
        }

        if self.at_one_of(recovery) {
            self.error(message);
            return;
        }

        let m = self.start();
        self.error(message);
        self.bump_any();
        m.complete(self, SyntaxKind::ERROR);
    }

    pub(crate) fn error<Msg: Into<String>>(&mut self, msg: Msg) {
        self.push_event(Event::Error { msg: msg.into() });
    }

    pub(crate) fn err_and_bump(&mut self, message: &str) {
        self.err_recover(message, TokenSet::EMPTY);
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
        let steps = self.steps.get();
        if steps > STEP_LIMIT {
            panic!("step limit exceeded");
        }
        self.steps.set(steps + 1);

        self.input.kind(self.pos + n)
    }

    /// Check that the current token is `kind`
    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    /// Check that the `n`th token is `kind`
    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.nth(n) == kind
    }

    /// Checks if the current token is contextual keyword `kw`.
    pub(crate) fn at_contextual_kw(&self, kw: SyntaxKind) -> bool {
        self.input.contextual_kind(self.pos) == kw
    }

    #[allow(dead_code)]
    /// Checks if the nth token is contextual keyword `kw`.
    pub(crate) fn nth_at_contextual_kw(&self, n: usize, kw: SyntaxKind) -> bool {
        self.input.contextual_kind(self.pos + n) == kw
    }

    /// Check that the next token is `kind`
    pub(crate) fn peek_at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(1, kind)
    }

    pub(crate) fn nth_at_one_of(&self, n: usize, set: TokenSet) -> bool {
        set.contains(self.nth(n))
    }

    /// Check that the next token is in `set`
    pub(crate) fn peek_at_one_of(&self, set: TokenSet) -> bool {
        self.nth_at_one_of(1, set)
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;
        self.push_event(Event::tombstone());
        Marker::new(pos)
    }

    pub(crate) fn at_one_of(&self, set: TokenSet) -> bool {
        self.nth_at_one_of(0, set)
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
    #[allow(dead_code)]
    pos: u32,
    #[allow(dead_code)]
    kind: SyntaxKind,
}

impl CompletedMarker {
    fn new(pos: u32, kind: SyntaxKind) -> Self {
        Self { pos, kind }
    }

    #[allow(dead_code)]
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

    #[allow(dead_code)]
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

    #[allow(dead_code)]
    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }
}
