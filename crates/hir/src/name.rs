use smol_str::SmolStr;
use syntax::{ast, AstToken};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name(SmolStr);

pub trait AsName {
    fn as_name(&self) -> Name;
}

impl AsName for ast::Ident {
    fn as_name(&self) -> Name {
        Name(self.text().into())
    }
}

impl AsName for ast::Identifier {
    fn as_name(&self) -> Name {
        Name(self.ident_token().unwrap().text().into())
    }
}
