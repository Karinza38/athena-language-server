use la_arena::Idx;

use crate::{name::Name, sort::SortId};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Pat {
    pub kind: PatKind,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum PatKind {
    Ident(Name),
    List(Vec<PatId>),
    Var(Name, Option<SortId>),
}

pub type PatId = Idx<Pat>;

impl Pat {
    pub fn walk_child_pats(&self, mut f: impl FnMut(PatId)) {
        match &self.kind {
            PatKind::Ident(_) => {}
            PatKind::List(pats) => {
                for pat in pats {
                    f(*pat);
                }
            }
            PatKind::Var(_, _) => {}
        }
    }
}
