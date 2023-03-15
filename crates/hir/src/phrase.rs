use crate::expr::{Expr, ExprId};

pub enum PhraseId {
    Expr(ExprId),
}

pub enum Phrase {
    Expr(Expr),
}

pub struct PhraseBody {
    
}

pub struct PhraseSourceMap {

}