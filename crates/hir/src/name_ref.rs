use la_arena::Idx;

use crate::name::Name;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct NameRef {
    pub name: Name,
}

pub type NameRefId = Idx<NameRef>;
