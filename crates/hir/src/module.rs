use std::collections::HashMap;

use la_arena::Idx;

use crate::name::Name;

pub struct ModuleScope {
    definitions: Vec<DefinitionId>,
}

pub enum ModuleItem {
    Module(ModuleId),
    Definition(DefinitionId),
    DataType(DataTypeId),
}

pub enum Visibility {
    Public,
    Private,
}

pub struct Definition {
    name: Name,
    parent: ModuleId,
    visibility: Visibility,
}

pub struct DataType {
    name: Name,
    parent: ModuleId,
}

pub struct DataTypeConstructor {
    parent: DataTypeId,
    name: Name,
}

pub struct Structure {
    name: Name,
    parent: ModuleId,
}

pub type DataTypeId = Idx<DataType>;

pub type DefinitionId = Idx<Definition>;

pub type ModuleId = Idx<Module>;

pub enum ModuleKind {
    Root,
    Child { parent: ModuleId, name: Name },
}

pub struct Module {
    kind: ModuleKind,
    children: HashMap<Name, ModuleId>,
    scope: ModuleScope,
}
