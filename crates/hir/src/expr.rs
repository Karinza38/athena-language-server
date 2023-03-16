use la_arena::Idx;

use crate::{name::Name, scope::ScopeId, sort::Sort};

pub type ExprId = Idx<Expr>;

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    String(Box<str>),
    Char(u8),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub scope: ScopeId,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Missing,
    Ident(Name),
    Literal(Literal),
    Unit,
    TermVar(Name, Sort),
}
