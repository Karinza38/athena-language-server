//! Bridge between lexer and parser

use std::mem;

use crate::{lexer::LexedInput, SyntaxKind};

pub enum StrStep<'i> {
    Token {
        kind: crate::SyntaxKind,
        text: &'i str,
    },
    Enter {
        kind: crate::SyntaxKind,
    },
    Exit,
    Error {
        msg: &'i str,
        pos: usize,
    },
}

impl<'i> LexedInput<'i> {
    pub fn to_parser_input(&self) -> crate::input::Input {
        let mut res = crate::input::Input::new();
        for (i, kind) in self.kinds().enumerate() {
            if kind.is_trivia() {
                continue;
            } else if kind == SyntaxKind::IDENT {
                let token_text = self.text(i);
                let contextual_kw =
                    SyntaxKind::from_contextual_keyword(token_text).unwrap_or(SyntaxKind::IDENT);
                res.push_ident(contextual_kw);
            } else {
                res.push(kind);
            }
        }
        res
    }

    pub fn intersperse_trivia(
        &self,
        output: &crate::output::Output,
        sink: &mut dyn FnMut(StrStep<'_>),
    ) -> bool {
        let mut builder = Builder::new(self, sink);

        for event in output.steps() {
            match event {
                crate::output::Step::Token { kind } => builder.token(*kind),
                crate::output::Step::Enter { kind } => builder.enter(*kind),
                crate::output::Step::Exit => builder.exit(),
                crate::output::Step::Error { msg } => builder.error(msg),
            }
        }

        match mem::replace(&mut builder.state, State::Normal) {
            State::PendingExit => {
                builder.eat_trivia();
                (builder.sink)(StrStep::Exit);
            }
            State::PendingEnter | State::Normal => unreachable!(),
        }

        builder.pos == self.len() // at EOF
    }
}

enum State {
    PendingEnter,
    Normal,
    PendingExit,
}

struct Builder<'i, 'o> {
    lexed: &'i LexedInput<'i>,
    pos: usize,
    state: State,
    sink: &'o mut dyn FnMut(StrStep<'i>),
}

impl<'i, 'o> Builder<'i, 'o> {
    fn new(lexed: &'i LexedInput<'i>, sink: &'o mut dyn FnMut(StrStep<'i>)) -> Builder<'i, 'o> {
        Builder {
            lexed,
            pos: 0,
            state: State::PendingEnter,
            sink,
        }
    }
    fn token(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingEnter => unreachable!(),
            State::PendingExit => (self.sink)(StrStep::Exit),
            State::Normal => {}
        }

        self.eat_trivia();
        self.do_token(kind);
        self.eat_trivia();
    }

    fn enter(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingEnter => {
                (self.sink)(StrStep::Enter { kind });
                return;
            }
            State::PendingExit => (self.sink)(StrStep::Exit),
            State::Normal => {}
        }
        self.eat_trivia();
        (self.sink)(StrStep::Enter { kind });
        self.eat_trivia();
    }

    fn exit(&mut self) {
        match mem::replace(&mut self.state, State::PendingExit) {
            State::PendingEnter => unreachable!(),
            State::PendingExit => (self.sink)(StrStep::Exit),
            State::Normal => {}
        }
    }

    fn eat_trivia(&mut self) {
        while self.pos < self.lexed.len() {
            let kind = self.lexed.kind(self.pos);
            if !kind.is_trivia() {
                break;
            }
            self.do_token(kind);
        }
    }

    fn do_token(&mut self, kind: SyntaxKind) {
        let text = self.lexed.text_for_range(self.pos..self.pos + 1);
        self.pos += 1;
        (self.sink)(StrStep::Token { kind, text });
    }

    fn error(&mut self, msg: &'i str) {
        let text_pos = self.lexed.text_start(self.pos);
        (self.sink)(StrStep::Error { msg, pos: text_pos })
    }
}
