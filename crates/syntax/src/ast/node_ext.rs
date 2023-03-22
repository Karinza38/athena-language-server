use crate::{ast, AstNode, SyntaxKind, SyntaxNode};
use smol_str::SmolStr;

impl ast::Name {
    pub fn text(&self) -> SmolStr {
        text_of_first_token(self.syntax())
    }
}

impl ast::NameRef {
    pub fn text(&self) -> SmolStr {
        text_of_first_token(self.syntax())
    }
}

fn text_of_first_token(node: &SyntaxNode) -> SmolStr {
    node.green()
        .children()
        .next()
        .and_then(|it| it.into_token())
        .unwrap()
        .text()
        .into()
}

#[derive(Debug, Clone)]
pub enum NameOrNameRef {
    Name(ast::Name),
    NameRef(ast::NameRef),
}

impl ast::AstNode for NameOrNameRef {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::NAME | SyntaxKind::NAME_REF)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            SyntaxKind::NAME => NameOrNameRef::Name(ast::Name { syntax }),
            SyntaxKind::NAME_REF => NameOrNameRef::NameRef(ast::NameRef { syntax }),
            _ => return None,
        };

        Some(res)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            NameOrNameRef::Name(it) => it.syntax(),
            NameOrNameRef::NameRef(it) => it.syntax(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SortLike {
    Sort(ast::Sort),
    SortDecl(ast::SortDecl),
    LimitedSort(ast::LimitedSort),
}

impl ast::AstNode for SortLike {
    fn can_cast(kind: SyntaxKind) -> bool {
        ast::Sort::can_cast(kind)
            || ast::SortDecl::can_cast(kind)
            || ast::LimitedSort::can_cast(kind)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if let Some(sort) = ast::Sort::cast(syntax.clone()) {
            return Some(SortLike::Sort(sort));
        }
        if let Some(decl) = ast::SortDecl::cast(syntax.clone()) {
            return Some(SortLike::SortDecl(decl));
        }
        if let Some(limited) = ast::LimitedSort::cast(syntax) {
            return Some(SortLike::LimitedSort(limited));
        }

        None
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            SortLike::Sort(it) => it.syntax(),
            SortLike::SortDecl(it) => it.syntax(),
            SortLike::LimitedSort(it) => it.syntax(),
        }
    }
}
