use core::fmt;

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

impl AsName for ast::Name {
    fn as_name(&self) -> Name {
        Name(self.text())
    }
}

impl AsName for ast::NameRef {
    fn as_name(&self) -> Name {
        Name(self.text())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Name {
    pub fn as_smol_str(&self) -> SmolStr {
        self.0.clone()
    }
}
