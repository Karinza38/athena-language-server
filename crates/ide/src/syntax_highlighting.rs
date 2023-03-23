pub(crate) mod tags;

mod highlights;

use hir::{InFile, Semantics};
use ide_db::{base_db::FileId, RootDatabase, SymbolKind};
use syntax::{
    ast, match_ast, AstNode, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken, TextRange,
    WalkEvent, T,
};

use crate::{HlPunct, HlTag};

use self::{highlights::Highlights, tags::Highlight};

#[derive(Debug, Clone)]
pub struct HlRange {
    pub range: TextRange,
    pub highlight: Highlight,
}

pub(crate) fn highlight(
    db: &RootDatabase,
    file_id: FileId,
    range_to_highlight: Option<TextRange>,
) -> Vec<HlRange> {
    let sema = Semantics::new(db);

    let (node, range_to_highlight) = {
        let file = sema.parse(file_id);
        let file = file.syntax();
        match range_to_highlight {
            Some(range) => {
                let node = match file.covering_element(range) {
                    NodeOrToken::Node(it) => it,
                    NodeOrToken::Token(it) => it.parent().unwrap_or_else(|| file.clone()),
                };
                (node, range)
            }
            None => (file.clone(), file.text_range()),
        }
    };

    let mut hl = Highlights::new(node.text_range());

    traverse(&mut hl, &sema, &node, file_id, range_to_highlight);

    hl.to_vec()
}

fn traverse(
    hl: &mut Highlights,
    sema: &Semantics<'_, RootDatabase>,
    node: &SyntaxNode,
    file_id: FileId,
    range_to_highlight: TextRange,
) {
    for event in node.preorder_with_tokens() {
        use WalkEvent::{Enter, Leave};

        let range = match &event {
            Enter(it) | Leave(it) => it.text_range(),
        };

        if range.intersect(range_to_highlight).is_none() {
            continue;
        }

        let element = match event {
            Enter(NodeOrToken::Token(tok)) if tok.kind() == SyntaxKind::WHITESPACE => continue,
            Enter(n) => n,
            Leave(_) => continue,
        };

        let h = match element {
            NodeOrToken::Node(n) => {
                if let Some(id) = ast::NameOrNameRef::cast(n.clone()) {
                    name_or_name_ref(sema, file_id, id)
                } else {
                    if let Some(_meta) = ast::MetaIdent::cast(n) {
                        Some(HlTag::IdentLiteral.into())
                    } else {
                        continue;
                    }
                }
            }
            NodeOrToken::Token(tok) => token(tok),
        };

        if let Some(h) = h {
            hl.add(HlRange {
                range,
                highlight: h,
            })
        }
    }
}

fn punctuation(tok: SyntaxToken) -> Highlight {
    match tok.kind() {
        T!['{'] | T!['}'] => HlPunct::Brace.into(),
        T!['('] | T![')'] => HlPunct::Parenthesis.into(),
        T!['['] | T![']'] => HlPunct::Bracket.into(),
        T![!] => HlPunct::Bang.into(),
        T![:] => HlPunct::Colon.into(),
        T![;] => HlPunct::Semi.into(),
        T![,] => HlPunct::Comma.into(),
        T![:=] => HlPunct::ColonEq.into(),
        _ => HlPunct::Other.into(),
    }
}

fn keyword(_tok: SyntaxToken) -> Highlight {
    HlTag::Keyword.into()
}

fn literal(tok: SyntaxToken) -> Highlight {
    match tok.kind() {
        SyntaxKind::STRING => HlTag::StringLiteral.into(),
        SyntaxKind::CHAR => HlTag::CharLiteral.into(),
        _ => unreachable!(),
    }
}

fn token(tok: SyntaxToken) -> Option<Highlight> {
    let kind = tok.kind();

    Some(if kind.is_punct() {
        punctuation(tok)
    } else if kind.is_keyword() {
        keyword(tok)
    } else if kind.is_literal() {
        literal(tok)
    } else if kind == SyntaxKind::COMMENT {
        HlTag::Comment.into()
    } else if kind == SyntaxKind::IDENT {
        match tok.parent() {
            Some(node) => match node.kind() {
                SyntaxKind::META_IDENT => HlTag::IdentLiteral.into(),
                SyntaxKind::VAR_SORT => SymbolKind::Sort.into(),
                SyntaxKind::TERM_VAR_EXPR => SymbolKind::Value.into(),
                SyntaxKind::VAR_PAT => SymbolKind::Value.into(),
                _ => {
                    tracing::warn!("unexpected ident parent: {:?}", node);
                    return None;
                }
            },
            None => {
                tracing::warn!("unexpected ident without parent: {:?}", tok);
                return None;
            }
        }
    } else {
        tracing::warn!("unhandled token: {:?}", kind);
        return None;
    })
}

fn name_or_name_ref(
    sema: &Semantics<'_, RootDatabase>,
    file_id: FileId,
    name_like: ast::NameOrNameRef,
) -> Option<Highlight> {
    match name_like {
        ast::NameOrNameRef::Name(it) => name(sema, it),
        ast::NameOrNameRef::NameRef(it) => name_ref(sema, file_id, it),
    }
}

fn name(_sema: &Semantics<'_, RootDatabase>, name: ast::Name) -> Option<Highlight> {
    let parent = name.syntax().parent()?;

    if name.text().chars().all(|c| c.is_numeric()) {
        return Some(HlTag::NumberLiteral.into());
    }

    Some(match_ast! {
        match parent {
            ast::DeclareDir(_d) => SymbolKind::FnSym.into(),
            ast::DeclareAttr(_d) => SymbolKind::FnSym.into(),
            ast::ModuleDir(_d) => SymbolKind::Module.into(),
            ast::IdentSortDecl(_) => SymbolKind::Sort.into(),
            ast::InfixConstantDeclare(_) => SymbolKind::Const.into(),
            ast::DefineSortDir(_) => SymbolKind::Sort.into(),
            ast::DefineProc(_) => SymbolKind::Value.into(),
            ast::DefineName(_) => SymbolKind::Value.into(),
            ast::AssertDir(_) => SymbolKind::Value.into(),
            ast::RuleDir(_) => SymbolKind::Func.into(),
            ast::PickWitnessDed(_) => SymbolKind::Value.into(),
            ast::PickWitnessesDed(_) => SymbolKind::Value.into(),
            ast::AssumePart(_) => SymbolKind::Value.into(),
            ast::PrefixNamedAssumeDed(_) => SymbolKind::Value.into(),
            ast::OpAnnotatedParam(_) => SymbolKind::Value.into(),
            ast::TypedParam(_) => SymbolKind::Value.into(),
            ast::ConstantConstructor(_) => SymbolKind::Const.into(),
            ast::CompoundConstructor(_) => SymbolKind::Func.into(),
            ast::MaybeTaggedFieldSort(_) => SymbolKind::Value.into(),
            ast::ConcludeDed(_) => SymbolKind::Value.into(),
            ast::MaybeWildcardOpAnnotatedParam(_) => SymbolKind::Value.into(),
            ast::NamedPat(_) => SymbolKind::Value.into(),
            _ => return None,
        }
    })
}

fn name_ref(
    sema: &Semantics<'_, RootDatabase>,
    file_id: FileId,
    name_ref: ast::NameRef,
) -> Option<Highlight> {
    Some(
        match sema.resolve_name_ref(InFile::new(file_id, name_ref.clone())) {
            Some(name_res) => match name_res {
                hir::semantics::NameRefResolution::ModuleItem(mi) => match mi {
                    hir::file_hir::ModuleItem::ModuleId(_) => SymbolKind::Module.into(),
                    hir::file_hir::ModuleItem::DefinitionId(def) => {
                        let def: hir::file_hir::Definition = sema.hir(InFile::new(file_id, def));
                        match def.kind {
                            hir::file_hir::DefKind::FunctionSym => SymbolKind::FnSym.into(),
                            hir::file_hir::DefKind::Proc => SymbolKind::Func.into(),
                            hir::file_hir::DefKind::Value => SymbolKind::Value.into(),
                            hir::file_hir::DefKind::Sort => SymbolKind::Sort.into(),
                        }
                    }
                    hir::file_hir::ModuleItem::DataTypeId(_) => todo!(),
                    hir::file_hir::ModuleItem::StructureId(_) => todo!(),
                    hir::file_hir::ModuleItem::PhraseId(_) => todo!(),
                },
                hir::semantics::NameRefResolution::Local(_) => SymbolKind::Value.into(),
            },
            None => {
                tracing::warn!("failed to resolve name ref: {name_ref:?}");
                SymbolKind::Value.into()
            }
        },
    )
}
