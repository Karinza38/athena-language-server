use la_arena::{Idx, RawIdx};

pub type ExprId = Idx<Expr>;

pub enum Literal {
    String(Box<str>),
    Char(u8),
}

pub enum Expr {
    Missing,
}
