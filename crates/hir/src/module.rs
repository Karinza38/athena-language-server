use std::collections::HashMap;

use la_arena::Idx;

use crate::name::Name;

pub struct ModuleScope {}

pub type ModuleId = Idx<Module>;

pub struct Module {
    parent: Option<ModuleId>,
    children: HashMap<Name, ModuleId>,
    scope: ModuleScope,
}
