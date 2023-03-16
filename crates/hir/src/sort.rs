use la_arena::Idx;

use crate::{name::Name, scope::ScopeId};

#[derive(PartialEq, Eq, Debug)]
pub struct Sort {
    pub kind: SortKind,
}

#[derive(PartialEq, Eq, Debug)]
pub enum SortKind {
    Var(Name),
    Ident(Name),
    Compound(Vec<Sort>),
}

pub type SortId = Idx<Sort>;
