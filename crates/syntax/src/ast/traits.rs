use crate::ast::{self, support, AstNode};

pub trait HasIdentifier: AstNode {
    fn identifier(&self) -> Option<ast::Identifier> {
        support::child(self.syntax())
    }
}

pub trait HasDefineName: AstNode {
    fn define_name(&self) -> Option<ast::DefineName> {
        support::child(self.syntax())
    }
}

pub trait HasDefineBody: AstNode {
    fn define_body(&self) -> Option<ast::Phrase> {
        support::child(self.syntax())
    }
}
