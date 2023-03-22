use crate::ast::{self, support, AstNode};

pub trait HasName: AstNode {
    fn name(&self) -> Option<ast::Name> {
        support::child(self.syntax())
    }
}

pub trait HasNameRef: AstNode {
    fn name_ref(&self) -> Option<ast::NameRef> {
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
