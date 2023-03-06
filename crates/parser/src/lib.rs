mod bridge;
mod event;
mod grammar;
mod input;
mod lexer;
mod output;
mod parser;
mod syntax_kind;
mod token_set;

#[cfg(test)]
mod tests;

pub use crate::{
    bridge::StrStep,
    input::Input,
    lexer::LexedInput,
    output::{Output, Step},
    syntax_kind::SyntaxKind,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum EntryPoint {
    SourceFile,
    Expr,
    Pat,
    Ded,
    Phrase,
    Dir,
}

impl EntryPoint {
    pub fn parse(&self, input: &Input) -> Output {
        let parse: fn(&mut parser::Parser<'_>) = match self {
            EntryPoint::Expr => grammar::entry::expr,
            EntryPoint::Phrase => grammar::entry::phrase,
            EntryPoint::Pat => grammar::entry::pat,
            EntryPoint::Ded => grammar::entry::ded,
            EntryPoint::Dir => grammar::entry::dir,
            _ => todo!("{self:?}"),
        };
        let mut p = parser::Parser::new(input);
        parse(&mut p);
        let events = p.finish();

        let res = event::process(events);
        res
    }
}
