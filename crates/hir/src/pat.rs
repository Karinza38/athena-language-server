use crate::name::Name;

pub struct Pat {
    pub kind: PatKind,
}

pub enum PatKind {
    IdentPat(Name),
}
