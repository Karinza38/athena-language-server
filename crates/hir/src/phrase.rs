use crate::{ded::DedId, expr::ExprId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhraseId {
    Expr(ExprId),
    Ded(DedId),
}
