pub mod ast;
mod parsing;
mod ptr;
mod syntax_error;
mod syntax_node;
mod validation;

#[cfg(test)]
mod tests;

use std::{marker::PhantomData, sync::Arc};

pub use parser::{SyntaxKind, T};

pub use crate::{
    ast::{AstNode, AstToken},
    ptr::{AstPtr, SyntaxNodePtr},
    syntax_error::SyntaxError,
    syntax_node::{
        AthenaLanguage, PreorderWithTokens, SyntaxElement, SyntaxElementChildren, SyntaxNode,
        SyntaxNodeChildren, SyntaxToken, SyntaxTreeBuilder,
    },
};
pub use rowan::{
    api::Preorder, Direction, GreenNode, NodeOrToken, SyntaxText, TextRange, TextSize,
    TokenAtOffset, WalkEvent,
};
pub use smol_str::SmolStr;

use util::format_to;

/// `Parse` is the result of the parsing: a syntax tree and a collection of
/// errors.
///
/// Note that we always produce a syntax tree, even for completely invalid
/// files.
#[derive(Debug, PartialEq, Eq)]
pub struct Parse<T> {
    green: GreenNode,
    errors: Arc<Vec<SyntaxError>>,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Parse<T> {
    fn clone(&self) -> Parse<T> {
        Parse {
            green: self.green.clone(),
            errors: self.errors.clone(),
            _ty: PhantomData,
        }
    }
}

impl<T> Parse<T> {
    fn new(green: GreenNode, errors: Vec<SyntaxError>) -> Parse<T> {
        Parse {
            green,
            errors: Arc::new(errors),
            _ty: PhantomData,
        }
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }
    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }
}

impl<T: AstNode> Parse<T> {
    pub fn to_syntax(self) -> Parse<SyntaxNode> {
        Parse {
            green: self.green,
            errors: self.errors,
            _ty: PhantomData,
        }
    }

    pub fn tree(&self) -> T {
        T::cast(self.syntax_node()).unwrap()
    }

    pub fn ok(self) -> Result<T, Arc<Vec<SyntaxError>>> {
        if self.errors.is_empty() {
            Ok(self.tree())
        } else {
            Err(self.errors)
        }
    }
}

impl Parse<SyntaxNode> {
    pub fn cast<N: AstNode>(self) -> Option<Parse<N>> {
        if N::cast(self.syntax_node()).is_some() {
            Some(Parse {
                green: self.green,
                errors: self.errors,
                _ty: PhantomData,
            })
        } else {
            None
        }
    }
}

impl Parse<SourceFile> {
    pub fn debug_dump(&self) -> String {
        let mut buf = format!("{:#?}", self.tree().syntax());
        for err in self.errors.iter() {
            format_to!(buf, "error {:?}: {}\n", err.range(), err);
        }
        buf
    }

    // pub fn reparse(&self, indel: &Indel) -> Parse<SourceFile> {
    //     self.incremental_reparse(indel)
    //         .unwrap_or_else(|| self.full_reparse(indel))
    // }

    // fn incremental_reparse(&self, indel: &Indel) -> Option<Parse<SourceFile>> {
    //     // FIXME: validation errors are not handled here
    //     parsing::incremental_reparse(self.tree().syntax(), indel, self.errors.to_vec()).map(
    //         |(green_node, errors, _reparsed_range)| Parse {
    //             green: green_node,
    //             errors: Arc::new(errors),
    //             _ty: PhantomData,
    //         },
    //     )
    // }

    // fn full_reparse(&self, indel: &Indel) -> Parse<SourceFile> {
    //     let mut text = self.tree().syntax().text().to_string();
    //     indel.apply(&mut text);
    //     SourceFile::parse(&text)
    // }
}

/// `SourceFile` represents a parse tree for a single Rust file.
pub use crate::ast::SourceFile;

impl SourceFile {
    pub fn parse(text: &str) -> Parse<SourceFile> {
        let (green, mut errors) = parsing::parse_text(text);
        let root = SyntaxNode::new_root(green.clone());

        errors.extend(validation::validate(&root));

        assert_eq!(root.kind(), SyntaxKind::SOURCE_FILE);
        Parse {
            green,
            errors: Arc::new(errors),
            _ty: PhantomData,
        }
    }
}

/// Matches a `SyntaxNode` against an `ast` type.
///
/// # Example:
///
/// ```ignore
/// match_ast! {
///     match node {
///         ast::CallExpr(it) => { ... },
///         ast::MethodCallExpr(it) => { ... },
///         ast::MacroCall(it) => { ... },
///         _ => None,
///     }
/// }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { $crate::match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( $( $path:ident )::+ ($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = $($path::)+cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

#[test]
fn api_walkthrough() {
    // Modeled after rust-analyzer's lovely api_walkthrough test
    use ast::HasName;

    let source = "
        module foo {
            declare bar, baz: [Boolean] -> Int [100]

            define z := bar
        }
    ";

    // `SourceFile` is the main entry point.
    //
    // The `parse` method returns a `Parse` -- a pair of syntax tree and a list
    // of errors. That is, syntax tree is constructed even in presence of errors.
    let parse = SourceFile::parse(source);
    assert!(parse.errors().is_empty());

    // The `tree` method returns the actual syntax tree.
    // Nodes are cheaply clonable, as they are just references to the underlying
    // data
    let file: SourceFile = parse.tree();

    // We can iterate through the file's statements.
    // In our case, there is only one statement: the module declaration.
    for stmt in file.stmts() {
        eprintln!("{:?}", stmt);
    }
    assert_eq!(file.stmts().count(), 1);
    let module = file.stmts().next().unwrap();

    // `module` is an `ast::Stmt`, but we can destructure it to get to the module directive
    let ast::Stmt::Dir(ast::Dir::ModuleDir(module)) = module else {
        panic!("expected module declaration");
    };

    // Each AST node has a bunch of getters for children. All getters return
    // `Option`s, to account for incomplete code. Some getters are common
    // for several kinds of node. In this case, a trait like `ast::HasName`
    // usually exists. By convention, all ast types should be used with `ast::`
    // qualifier.
    let name: Option<ast::Name> = module.name();
    let name = name.unwrap();
    assert_eq!(name.text(), "foo");

    // Let's get the declare directive
    let ast::Stmt::Dir(ast::Dir::DeclareDir(declare)) = module.stmts().next().unwrap() else {
        panic!("expected declare directive");
    };

    // Athena has both infix and prefix forms of most directives, with different syntax (and therefore different children).
    let ast::DeclareDir::InfixDeclareDir(declare) = declare else {
        panic!("expected infix declare directive");
    };

    // Now let's get the return sort
    let return_sort: ast::Sort = declare.return_sort().unwrap();

    // Enums are used to group related ast nodes together, and can be used for
    // matching
    let ident_sort: &ast::IdentSort = match &return_sort {
        ast::Sort::IdentSort(i) => i,
        _ => unreachable!(),
    };

    // Aside from the typed AST API, there is an untyped CST (Concrete Syntax Tree) API.
    // To go from AST to CST, call `.syntax()` on the AST node.
    let return_sort_syntax: &SyntaxNode = return_sort.syntax();

    // Note that `return_sort` and `ident_sort` are the same underlying node:
    assert_eq!(return_sort_syntax, ident_sort.syntax());

    // To go from CST to AST, use `ast::AstNode::cast`:
    let _return_sort: ast::Sort = ast::Sort::cast(return_sort_syntax.clone()).unwrap();

    // Each syntax node has a `SyntaxKind`:
    assert_eq!(return_sort_syntax.kind(), SyntaxKind::IDENT_SORT);

    // And a `TextRange`:
    assert_eq!(
        return_sort_syntax.text_range(),
        TextRange::new(65.into(), 68.into())
    );

    // You can get a node's text as a `SyntaxText` object, which traverses the
    // tree collecting the tokens' text.
    let text: SyntaxText = return_sort_syntax.text();
    // No copy of the original text is made until you call the `Display` impl:
    assert_eq!(text.to_string(), "Int");

    // There are a bunch of traversal methods on `SyntaxNode`:
    assert_eq!(return_sort_syntax.parent().as_ref(), Some(declare.syntax()));
    assert_eq!(
        return_sort_syntax
            .first_child_or_token()
            .map(|it| it.kind()),
        Some(SyntaxKind::NAME_REF)
    );
    assert_eq!(
        return_sort_syntax
            .next_sibling_or_token()
            .map(|it| it.kind()),
        Some(SyntaxKind::WHITESPACE)
    );

    // As well as some iterator helpers:
    let m = return_sort_syntax
        .ancestors()
        .find_map(ast::ModuleDir::cast);
    assert_eq!(m, Some(module));

    // To recursively process the tree, there are three approaches:
    // 1. explicitly call getter methods on AST nodes.
    // 2. use descendants and `AstNode::cast`.
    // 3. use descendants and `match_ast!`.

    // Here's what the second approach looks like:
    let sorts_cast: Vec<String> = file
        .syntax()
        .descendants()
        .filter_map(ast::Sort::cast)
        .map(|sort| sort.syntax().text().to_string())
        .collect();

    // And here is the third approach, using the `match_ast!` macro:
    let mut sorts_visit = Vec::new();
    for node in file.syntax().descendants() {
        match_ast! {
            match node {
                ast::Sort(it) => {
                    let res = it.syntax().text().to_string();
                    sorts_visit.push(res);
                },
                _ => (),
            }
        }
    }
    assert_eq!(sorts_cast, sorts_visit);
}
