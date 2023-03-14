use std::sync::Arc;

use base_db::{salsa, FileId, SourceDatabase};
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use syntax::{AstNode, SyntaxNode, SyntaxNodePtr};

#[salsa::query_group(AstDatabaseStorage)]
pub trait AstDatabase: SourceDatabase {
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;
}

pub struct FileAstId<N: AstNode> {
    raw: ErasedAstId,
    _type: std::marker::PhantomData<fn() -> N>,
}

type ErasedAstId = Idx<SyntaxNodePtr>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    ptr_to_id: FxHashMap<SyntaxNodePtr, ErasedAstId>,
}

impl AstIdMap {
    pub(crate) fn from_source(node: &SyntaxNode) -> Self {
        let mut arena = Arena::default();

        bdfs(node, |node| {
            arena.alloc(SyntaxNodePtr::new(&node));
            true
        });

        let ptr_to_id = arena
            .iter()
            .map(|(erased, ptr)| (ptr.clone(), erased))
            .collect();
        AstIdMap { arena, ptr_to_id }
    }
}

fn bdfs(node: &SyntaxNode, f: impl FnMut(SyntaxNode) -> bool) {
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
    // let source_file = db.parse(file_id);
    // let mut arena = ArenaMap::default();
    // let mut ast_id_map = AstIdMap { arena };
    // ast_id_map.collect(&source_file.syntax());
    // Arc::new(ast_id_map)
    todo!()
}
