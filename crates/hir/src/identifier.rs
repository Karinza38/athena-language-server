use la_arena::Idx;

use crate::name::Name;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Identifier {
    pub name: Name,
}

pub type IdentifierId = Idx<Identifier>;
