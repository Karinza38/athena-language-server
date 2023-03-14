use std::sync::Arc;

use base_db::{salsa, FileId, SourceDatabase};
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use syntax::{ast, match_ast, AstNode, SyntaxNode, SyntaxNodePtr};

use crate::InFile;

#[salsa::query_group(AstDatabaseStorage)]
pub trait AstDatabase: SourceDatabase {
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;
}

pub struct FileAstId<N: AstNode> {
    raw: ErasedAstId,
    _type: std::marker::PhantomData<fn() -> N>,
}

type ErasedAstId = Idx<SyntaxNodePtr>;

pub type AstId<N> = InFile<FileAstId<N>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    ptr_to_id: FxHashMap<SyntaxNodePtr, ErasedAstId>,
}

impl AstIdMap {
    pub(crate) fn from_source(node: &SyntaxNode) -> Self {
        let mut arena = Arena::default();

        bdfs(node, |node| {
            let kind = node.kind();
            if ast::DefineDir::can_cast(kind)
                || ast::DeclareDir::can_cast(kind)
                || ast::ConstantDeclareDir::can_cast(kind)
                || ast::AssertClosedDir::can_cast(kind)
                || ast::AssertDir::can_cast(kind)
                || ast::DomainDir::can_cast(kind)
                || ast::DomainsDir::can_cast(kind)
                || ast::DatatypesStmt::can_cast(kind)
                || ast::DatatypeStmt::can_cast(kind)
                || ast::StructuresStmt::can_cast(kind)
                || ast::StructureStmt::can_cast(kind)
            {
                arena.alloc(SyntaxNodePtr::new(&node));
                false
            } else if ast::ModuleDir::can_cast(kind) || ast::ExtendModuleDir::can_cast(kind) {
                arena.alloc(SyntaxNodePtr::new(&node));
                true
            } else {
                false
            }
        });

        let ptr_to_id = arena
            .iter()
            .map(|(erased, ptr)| (ptr.clone(), erased))
            .collect();

        AstIdMap { arena, ptr_to_id }
    }
}

fn bdfs(node: &SyntaxNode, mut f: impl FnMut(SyntaxNode) -> bool) {
    let mut current = vec![node.clone()];
    let mut next = Vec::new();

    while !current.is_empty() {
        for node in current.drain(..) {
            let mut preorder = node.preorder();

            while let Some(event) = preorder.next() {
                match event {
                    syntax::WalkEvent::Enter(node) => {
                        if f(node.clone()) {
                            next.extend(node.children());
                            preorder.skip_subtree();
                        }
                    }
                    syntax::WalkEvent::Leave(_) => {}
                }
            }
        }
    }
}

fn ast_id_map(db: &dyn AstDatabase, file_id: FileId) -> Arc<AstIdMap> {
    // let _p = profile::span("ast_id_map");
    let source_file = db.parse(file_id);
    Arc::new(AstIdMap::from_source(&source_file.tree().syntax()))
}
