use crate::{ded::DedId, expr::ExprId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhraseId {
    ExprId(ExprId),
    DedId(DedId),
}

util::impl_from!(ExprId, DedId for PhraseId);
