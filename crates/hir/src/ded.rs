use la_arena::Idx;

use crate::scope::ScopeId;

#[derive(PartialEq, Eq, Debug)]
pub struct Ded {
    // pub kind: DedKind,
    // pub scope: ScopeId,
}

#[derive(PartialEq, Eq, Debug)]
pub enum DedKind {}

pub type DedId = Idx<Ded>;
