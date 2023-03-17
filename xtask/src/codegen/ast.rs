mod ast_src;

use fs_err as fs;
use ungrammar::{Grammar, Rule};
use itertools::Itertools;
use proc_macro2::{Ident, Punct, Spacing};
use quote::{format_ident, quote};
use std::{
    collections::{BTreeSet, HashSet},
    fmt::Write,
};

use ast_src::{
    AstEnumSrc, AstNodeSrc, AstSrc, AstTokenDef, AstTokenDefinition, Cardinality, Field, KindsSrc,
};

use crate::Error;

pub(super) fn sourcegen_ast() -> Result<(), Error> {
    let src_string = fs::read_to_string(sourcegen::project_root().join("crates/syntax/ast.ron"))?;
    let kinds_src = ron::from_str(&src_string)?;

    let syntax_kinds = generate_syntax_kinds(&kinds_src);
    let syntax_kinds_file =
        sourcegen::project_root().join("crates/parser/src/syntax_kind/generated.rs");

    sourcegen::ensure_file_contents(syntax_kinds_file.as_path(), &syntax_kinds);

    let grammar = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../crates/syntax/athena.ungram"
    ))
    .parse()
    .unwrap();

    let ast = lower(&grammar, &kinds_src);

    let ast_tokens = generate_tokens(&ast);
    let ast_tokens_file =
        sourcegen::project_root().join("crates/syntax/src/ast/generated/tokens.rs");
    sourcegen::ensure_file_contents(ast_tokens_file.as_path(), &ast_tokens);

    let ast_nodes = generate_nodes(&kinds_src, &ast);
    let ast_nodes_file = sourcegen::project_root().join("crates/syntax/src/ast/generated/nodes.rs");
    sourcegen::ensure_file_contents(ast_nodes_file.as_path(), &ast_nodes);

    let lexer = generate_lexer(&ast);
    let lexer_file = sourcegen::project_root().join("crates/parser/src/lexer/generated.rs");
    sourcegen::ensure_file_contents(lexer_file.as_path(), &lexer);

    Ok(())
}

fn generate_tokens(grammar: &AstSrc) -> String {
    let tokens = grammar.tokens.iter().map(|token| {
        let name = format_ident!("{}", token);
        let kind = format_ident!("{}", to_upper_snake_case(token));
        quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #name {
                pub(crate) syntax: SyntaxToken,
            }
            impl std::fmt::Display for #name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    std::fmt::Display::fmt(&self.syntax, f)
                }
            }
            impl AstToken for #name {
                fn can_cast(kind: SyntaxKind) -> bool { kind == #kind }
                fn cast(syntax: SyntaxToken) -> Option<Self> {
                    if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                }
                fn syntax(&self) -> &SyntaxToken { &self.syntax }
            }
        }
    });

    sourcegen::add_preamble(
        "sourcegen_ast",
        sourcegen::reformat(
            quote! {
                use crate::{SyntaxKind::{self, *}, SyntaxToken, ast::AstToken};
                #(#tokens)*
            }
            .to_string(),
        ),
    )
    .replace("#[derive", "\n#[derive")
}

impl AstSrc {
    fn get_enum_node(&self, name: &str) -> Option<&AstEnumSrc> {
        self.enums.iter().find(|node| node.name == name)
    }

    fn get_node(&self, name: &str) -> Option<&AstNodeSrc> {
        self.nodes.iter().find(|node| node.name == name)
    }

    #[allow(dead_code)]
    fn get_traits(&self, name: &str) -> Option<BTreeSet<String>> {
        match self.get_node(name) {
            Some(node) => Some(node.traits()),
            None => self.get_enum_node(name).map(|enm| enm.traits()),
        }
    }
}

impl AstNodeSrc {
    fn name(&self) -> Ident {
        format_ident!("{}", &self.name)
    }

    fn kind(&self) -> Ident {
        format_ident!("{}", to_upper_snake_case(&self.name))
    }

    fn traits(&self) -> BTreeSet<String> {
        self.traits.iter().cloned().collect()
    }
}

impl AstEnumSrc {
    fn variants(&self) -> Vec<Ident> {
        self.variants
            .iter()
            .map(|variant| format_ident!("{}", variant))
            .collect()
    }

    fn partitioned_variants(&self, grammar: &AstSrc) -> (Vec<Ident>, Vec<Ident>) {
        let mut enums = Vec::new();
        let mut non_enums = Vec::new();
        for variant in self.variants() {
            if grammar.get_enum_node(&variant.to_string()).is_some() {
                enums.push(variant);
            } else {
                non_enums.push(variant);
            }
        }
        (enums, non_enums)
    }

    fn name(&self) -> Ident {
        format_ident!("{}", &self.name)
    }

    fn traits(&self) -> BTreeSet<String> {
        self.traits.iter().cloned().collect()
    }
}

fn generate_nodes(kinds: &KindsSrc, grammar: &AstSrc) -> String {
    let (node_defs, node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .map(|node| {
            let name = node.name();
            let kind = node.kind();
            let traits = node
                .traits
                .iter()
                .filter(|trait_name| {
                    // Loops have two expressions so this might collide, therefore manual impl it
                    node.name != "ForExpr" && node.name != "WhileExpr"
                        || trait_name.as_str() != "HasLoopBody"
                })
                .map(|trait_name| {
                    let trait_name = format_ident!("{}", trait_name);
                    quote!(impl ast::#trait_name for #name {})
                });

            let methods = node.fields.iter().map(|field| {
                let method_name = field.method_name();
                let ty = field.ty();

                if field.is_many() {
                    quote! {
                        pub fn #method_name(&self) -> AstChildren<#ty> {
                            support::children(&self.syntax)
                        }
                    }
                } else if let Some(token_kind) = field.token_kind() {
                    quote! {
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::token(&self.syntax, #token_kind)
                        }
                    }
                } else {
                    quote! {
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::child(&self.syntax)
                        }
                    }
                }
            });
            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }

                    #(#traits)*

                    impl #name {
                        #(#methods)*
                    }
                },
                quote! {
                    impl AstNode for #name {
                        fn can_cast(kind: SyntaxKind) -> bool {
                            kind == #kind
                        }
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                        }
                        fn syntax(&self) -> &SyntaxNode { &self.syntax }
                    }
                },
            )
        })
        .unzip();

    let (enum_defs, enum_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .enums
        .iter()
        .map(|en| {
            let variants = en.variants();
            let name = en.name();

            let traits = en.traits.iter().map(|trait_name| {
                let trait_name = format_ident!("{}", trait_name);
                quote!(impl ast::#trait_name for #name {})
            });

            let (enum_variants, non_enum_variants) = en.partitioned_variants(grammar);

            let non_enum_kinds = non_enum_variants
                .iter()
                .map(|variant| {
                    let kind = format_ident!("{}", to_upper_snake_case(&variant.to_string()));
                    quote!(#kind)
                })
                .collect::<Vec<_>>();

            assert!(enum_variants.len() + non_enum_variants.len() > 0);

            let can_cast = if non_enum_variants.is_empty() {
                quote! {
                    fn can_cast(kind: SyntaxKind) -> bool {
                        #(#enum_variants::can_cast(kind))||*
                    }
                }
            } else {
                quote! {
                    fn can_cast(kind: SyntaxKind) -> bool {
                        matches!(kind, #(#non_enum_kinds)|*) #(|| #enum_variants::can_cast(kind))*
                    }
                }
            };

            let cast = {
                let other = if enum_variants.is_empty() {
                    quote! { _ => return None, }
                } else {
                    quote! {
                        other => {
                            if false  {
                                return None;
                            } #(else if #enum_variants::can_cast(other) {
                                return #enum_variants::cast(syntax).map(Self::#enum_variants);
                            })* else {
                                return None;
                            }
                        }
                    }
                };

                let body = if non_enum_variants.is_empty() {
                    quote! { 
                        match syntax.kind() {
                            #other
                        }
                    }
                } else {
                    quote! {
                        let res = match syntax.kind() {
                            #(#non_enum_kinds => Self::#non_enum_variants(#non_enum_variants { syntax }),)*
                            #other
                        };
                        Some(res)
                    }
                };
                quote! {
                    fn cast(syntax: SyntaxNode) -> Option<Self> {
                        #body
                    }
                }
            };

            let ast_node = {
                quote! {
                    impl AstNode for #name {
                        #can_cast
                        #cast

                        #[inline]
                        fn syntax(&self) -> &SyntaxNode {
                            match self {
                                #(
                                #name::#variants(it) => it.syntax(),
                                )*
                            }
                        }
                    }
                }
            };


            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub enum #name {
                        #(#variants(#variants),)*
                    }
                    #(#traits)*
                },
                quote! {
                    #(
                        impl From<#variants> for #name {
                            fn from(node: #variants) -> #name {
                                #name::#variants(node)
                            }
                        }
                    )*
                    #ast_node
                },
            )
        })
        .unzip();

    let (any_node_defs, any_node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .flat_map(|node| node.traits.iter().map(move |t| (t, node)))
        .into_group_map()
        .into_iter()
        .sorted_by_key(|(k, _)| *k)
        .map(|(trait_name, nodes)| {
            let name = format_ident!("Any{}", trait_name);
            let trait_name = format_ident!("{}", trait_name);
            let kinds: Vec<_> = nodes
                .iter()
                .map(|name| format_ident!("{}", to_upper_snake_case(&name.name.to_string())))
                .collect();

            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }
                    impl ast::#trait_name for #name {}
                },
                quote! {
                    impl #name {
                        #[inline]
                        pub fn new<T: ast::#trait_name>(node: T) -> #name {
                            #name {
                                syntax: node.syntax().clone()
                            }
                        }
                    }
                    impl AstNode for #name {
                        fn can_cast(kind: SyntaxKind) -> bool {
                            matches!(kind, #(#kinds)|*)
                        }
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            Self::can_cast(syntax.kind()).then_some(#name { syntax })
                        }
                        fn syntax(&self) -> &SyntaxNode {
                            &self.syntax
                        }
                    }
                },
            )
        })
        .unzip();

    let enum_names = grammar.enums.iter().map(|it| &it.name);
    let node_names = grammar.nodes.iter().map(|it| &it.name);

    let display_impls = enum_names
        .chain(node_names.clone())
        .map(|it| format_ident!("{}", it))
        .map(|name| {
            quote! {
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(self.syntax(), f)
                    }
                }
            }
        });

    let defined_nodes: HashSet<_> = node_names.collect();

    for node in kinds
        .nodes
        .iter()
        .map(|kind| to_pascal_case(kind))
        .filter(|name| !defined_nodes.contains(name))
    {
        drop(node)
        // FIXME: restore this
        // eprintln!("Warning: node {} not defined in ast source", node);
    }

    let ast = quote! {
        #![allow(non_snake_case, dead_code, unused_imports)]
        use crate::{
            SyntaxNode, SyntaxToken, SyntaxKind::{self, *},
            ast::{self, AstNode, AstChildren, support},
            T,
        };

        #(#node_defs)*
        #(#enum_defs)*
        #(#any_node_defs)*
        #(#node_boilerplate_impls)*
        #(#enum_boilerplate_impls)*
        #(#any_node_boilerplate_impls)*
        #(#display_impls)*
    };

    let ast = ast.to_string().replace("T ! [", "T![");

    let mut res = String::with_capacity(ast.len() * 2);

    let mut docs = grammar
        .nodes
        .iter()
        .map(|it| &it.doc)
        .chain(grammar.enums.iter().map(|it| &it.doc));

    for chunk in ast.split("# [pretty_doc_comment_placeholder_workaround] ") {
        res.push_str(chunk);
        if let Some(doc) = docs.next() {
            write_doc_comment(doc, &mut res);
        }
    }

    let res = sourcegen::add_preamble("sourcegen_ast", sourcegen::reformat(res));
    res.replace("#[derive", "\n#[derive")
}

fn write_doc_comment(contents: &[String], dest: &mut String) {
    for line in contents {
        writeln!(dest, "///{line}").unwrap();
    }
}

fn generate_syntax_kinds(grammar: &KindsSrc) -> String {
    let (single_byte_tokens_values, single_byte_tokens): (Vec<_>, Vec<_>) = grammar
        .punct
        .iter()
        .filter(|(token, _name)| token.len() == 1)
        .map(|(token, name)| (token.chars().next().unwrap(), format_ident!("{}", name)))
        .unzip();

    let punctuation_values = grammar.punct.iter().map(|(token, _name)| {
        if "{}[]()'".contains(token) {
            let c = token.chars().next().unwrap();
            quote! { #c }
        } else if "|{}|".contains(token) {
            quote! { #token }
        } else {
            let cs = token.chars().map(|c| Punct::new(c, Spacing::Joint));
            quote! { #(#cs)* }
        }
    });
    let punctuation = grammar
        .punct
        .iter()
        .map(|(_token, name)| format_ident!("{}", name))
        .collect::<Vec<_>>();

    let x = |name: &String| match name.as_str() {
        "Self" => format_ident!("SELF_TYPE_KW"),
        "assert*" => format_ident!("ASSERT_STAR_KW"),
        "|" => format_ident!("PIPE"),
        name => format_ident!("{}_KW", to_upper_snake_case(name)),
    };
    let full_keywords_values = grammar.keywords.clone();
    let full_keywords = full_keywords_values.iter().map(x);

    let contextual_keywords_values = &grammar.contextual_keywords;
    let contextual_keywords = contextual_keywords_values.iter().map(x);

    let all_keywords_values = grammar
        .keywords
        .iter()
        .chain(grammar.contextual_keywords.iter())
        .cloned()
        .collect::<Vec<_>>();
    let all_keywords_idents = all_keywords_values.iter().map(|kw| {
        if kw.as_str() == "|" {
            quote! { | }
        } else {
            kw.parse::<proc_macro2::TokenStream>().unwrap()
        }
    });
    let all_keywords = all_keywords_values.iter().map(x).collect::<Vec<_>>();

    let literals = grammar
        .literals
        .iter()
        .map(|name| format_ident!("{}", name))
        .collect::<Vec<_>>();

    let tokens = grammar
        .tokens
        .iter()
        .map(|name| format_ident!("{}", name))
        .collect::<Vec<_>>();

    let nodes = grammar
        .nodes
        .iter()
        .map(|name| format_ident!("{}", name))
        .collect::<Vec<_>>();

    let last_token = tokens.last().unwrap();

    let ast = quote! {
        #![allow(bad_style, missing_docs, unreachable_pub)]
        /// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        #[repr(u16)]
        pub enum SyntaxKind {
            // Technical SyntaxKinds: they appear temporally during parsing,
            // but never end up in the final tree
            #[doc(hidden)]
            TOMBSTONE,
            #[doc(hidden)]
            EOF,
            #(#punctuation,)*
            #(#all_keywords,)*
            #(#literals,)*
            #(#tokens,)*
            #(#nodes,)*

            // Technical kind so that we can cast from u16 safely
            #[doc(hidden)]
            __LAST,
        }
        use self::SyntaxKind::*;

        impl SyntaxKind {
            pub const LAST_TOKEN: SyntaxKind = SyntaxKind::#last_token;

            pub fn is_keyword(self) -> bool {
                matches!(self, #(#all_keywords)|*)
            }

            pub fn is_punct(self) -> bool {

                matches!(self, #(#punctuation)|*)

            }

            pub fn is_literal(self) -> bool {
                matches!(self, #(#literals)|*)
            }

            pub fn is_node(self) -> bool {
                matches!(self, #(#nodes)|*)
            }

            pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
                let kw = match ident {
                    #(#full_keywords_values => #full_keywords,)*
                    _ => return None,
                };
                Some(kw)
            }

            pub fn from_contextual_keyword(ident: &str) -> Option<SyntaxKind> {
                #[allow(unused_variables, unreachable_code)]
                {
                    let kw = match ident {
                        #(#contextual_keywords_values => #contextual_keywords,)*
                        _ => return None,
                    };
                    Some(kw)
                }
            }

            pub fn from_char(c: char) -> Option<SyntaxKind> {
                let tok = match c {
                    #(#single_byte_tokens_values => #single_byte_tokens,)*
                    _ => return None,
                };
                Some(tok)
            }
        }

        #[macro_export]
        macro_rules! T {
            #([#punctuation_values] => { $crate::SyntaxKind::#punctuation };)*
            #([#all_keywords_idents] => { $crate::SyntaxKind::#all_keywords };)*
            [lifetime_ident] => { $crate::SyntaxKind::LIFETIME_IDENT };
            [ident] => { $crate::SyntaxKind::IDENT };
            [shebang] => { $crate::SyntaxKind::SHEBANG };
        }
        pub use T;
    };

    sourcegen::add_preamble("sourcegen_ast", sourcegen::reformat(ast.to_string()))
}

fn generate_lexer(grammar: &AstSrc) -> String {
    let mut logos_rule = Vec::new();
    let mut variant_names = Vec::new();
    let mut complex_variant_names = Vec::new();
    for token_def in &grammar.token_defs {
        // FIXME: this whole # thing is gross
        let simple = token_def.name.starts_with('#');
        let variant_name =
            format_ident!("{}", to_pascal_case(token_def.name.trim_start_matches('#')));
        let variant = quote! { #variant_name(usize) };
        let logos_annotation = match &token_def.def {
            AstTokenDef::Regex(reg) => {
                if simple {
                    variant_names.push((variant_name.clone(), token_def.raw_token()));
                } else {
                    complex_variant_names.push(variant_name.clone());
                }
                let mut buf = String::from("r###\"");
                buf.push_str(reg);
                buf.push_str("\"###");
                let reg = buf.parse::<proc_macro2::TokenStream>().unwrap();
                quote! { #[regex(#reg, |lex| lex.slice().len())] }
            }
            AstTokenDef::Literal(lit) => {
                variant_names.push((variant_name.clone(), token_def.raw_token()));
                quote! { #[token(#lit, |lex| lex.slice().len())] }
            }
        };
        logos_rule.push(quote! {
            #logos_annotation
            #variant,
        });
    }

    let (variant_names, syntax_kind) = variant_names
        .into_iter()
        .map(|(v, t)| {
            let kind = token_kind_raw(&t);
            (v, kind)
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let (complex_variant_names, complex_syntax_kind) = complex_variant_names
        .into_iter()
        .map(|v| {
            let kind = format_ident!("{}", to_upper_snake_case(&v.to_string()));
            (v, quote! { SyntaxKind::#kind })
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let to_syntax_kind = quote! {
        pub(crate) fn to_syntax_kind(self) -> SyntaxKind {
            match self {
                #(Self::#variant_names(..) => #syntax_kind,)*
                #(Self::#complex_variant_names(..) => #complex_syntax_kind,)*
                Self::Error => SyntaxKind::ERROR,
            }
        }
    };

    let lexer = quote! {
        #![allow(bad_style, missing_docs, unreachable_pub)]
        use logos::Logos;
        use crate::{SyntaxKind, T};

        #[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub(crate) enum LexerToken {
            #( #logos_rule )*
            #[error]
            Error,
        }

        impl LexerToken {
            #to_syntax_kind

            pub(crate) fn len(self) -> usize {
                match self {
                    #(Self::#variant_names(len) => len,)*
                    #(Self::#complex_variant_names(len) => len,)*
                    Self::Error => 0,
                }
            }
        }
    };

    sourcegen::add_preamble("sourcegen_ast", sourcegen::reformat(lexer.to_string()))
}

fn to_upper_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_')
        }
        prev = true;

        if c == '-' {
            buf.push('_');
            continue;
        } else if c == '!' {
            buf.push_str("BANG");
            continue;
        }
        buf.push(c.to_ascii_uppercase());
    }
    buf
}

fn to_lower_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_')
        }
        prev = true;

        buf.push(c.to_ascii_lowercase());
    }
    buf
}

fn to_pascal_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev_is_underscore = true;
    for c in s.chars() {
        if c == '_' {
            prev_is_underscore = true;
        } else if prev_is_underscore {
            buf.push(c.to_ascii_uppercase());
            prev_is_underscore = false;
        } else {
            buf.push(c.to_ascii_lowercase());
        }
    }
    buf
}

fn pluralize(s: &str) -> String {
    format!("{s}s")
}

fn token_name(name: &str) -> String {
    let processed_name = if name.starts_with('-') {
        name.to_owned()
    } else {
        // FIXME: Should really have a smarter way to handle this
        name.replace('-', "_").replace('!', "_bang")
    };
    match name {
        ";" => "semicolon",
        "->" => "thin_arrow",
        "'{'" | "{" => "l_curly",
        "'}'" | "}" => "r_curly",
        "'('" | "(" => "l_paren",
        "')'" | ")" => "r_paren",
        "'['" | "[" => "l_brack",
        "']'" | "]" => "r_brack",
        r#""|{""# | "|{" => "pipe_curly",
        r#"}|""# | "}|" => "curly_pipe",
        "<" => "l_angle",
        ">" => "r_angle",
        "=" => "eq",
        "!" => "bang",
        "*" => "star",
        "&" => "amp",
        "_" => "underscore",
        "." => "dot",
        ".." => "dotdot",
        "..." => "dotdotdot",
        "..=" => "dotdoteq",
        "=>" => "fat_arrow",
        "@" => "at",
        ":" => "colon",
        "::" => "coloncolon",
        "#" => "pound",
        "?" => "question_mark",
        "," => "comma",
        "|" => "pipe",
        "~" => "tilde",
        "'" => "single_quote",
        "&&" => "ampamp",
        "||" => "pipepipe",
        "set!" => "set_bang",
        ":=" => "colon_eq",
        "assert*" => "assert_star",
        _ => processed_name.as_str(),
    }
    .into()
}

fn token_kind_raw(token: &str) -> proc_macro2::TokenStream {
    if token == "'" {
        return quote! { T!['\''] };
    } else if token == "|{" {
        return quote! { T!["|{"] };
    } else if token == "}|" {
        return quote! { T!["}|"] };
    }
    let token = token.parse::<proc_macro2::TokenStream>().unwrap();
    quote! { T![#token] }
}

impl Field {
    fn is_many(&self) -> bool {
        matches!(
            self,
            Field::Node {
                cardinality: Cardinality::Many,
                ..
            }
        )
    }
    fn token_kind(&self) -> Option<proc_macro2::TokenStream> {
        match self {
            Field::Token(token) => Some(token_kind_raw(token)),
            _ => None,
        }
    }
    fn method_name(&self) -> proc_macro2::Ident {
        match self {
            Field::Token(name) => {
                let name = token_name(name);
                format_ident!("{}_token", name)
            }
            Field::Node { name, .. } => {
                if name == "type" {
                    format_ident!("ty")
                } else {
                    format_ident!("{}", name)
                }
            }
        }
    }
    fn ty(&self) -> proc_macro2::Ident {
        match self {
            Field::Token(_) => format_ident!("SyntaxToken"),
            Field::Node { ty, .. } => format_ident!("{}", ty),
        }
    }
}

fn lower_token_defs(res: &mut AstSrc, grammar: &Grammar, kinds: &KindsSrc, token_def_rule: &Rule) {
    let mut ignore_toks = Vec::new();
    let mut ignore_tok_names = Vec::new();
    let Rule::Alt(alts) = token_def_rule else { unreachable!()};

    let mut token_defs = Vec::new();
    for alt in alts {
        // print!("alt = ");
        // print_rule(alt, grammar);
        // println!();
        let tok = match alt {
            Rule::Token(tok) => tok,
            _ => unreachable!(),
        };
        ignore_toks.push(tok);
        let token = &grammar[*tok];

        let Some((name, def)) = token.name.split_once('=') else { panic!("not a valid token def")};
        ignore_tok_names.push(name.trim().trim_start_matches('#')); // FIXME: this whole # thing is gross

        let name = token_name(name.trim());
        // println!("name = {}, def = {}", name, def);
        token_defs.push(AstTokenDefinition::regex(name, def.trim()));
    }

    for token in grammar
        .tokens()
        .filter(|t| !ignore_toks.contains(&t))
        .map(|t| &grammar[t])
        .filter(|d| {
            !ignore_tok_names.contains(&&*d.name) && !kinds.contextual_keywords.contains(&d.name)
        })
    {
        let name = token_name(&token.name);
        token_defs.push(AstTokenDefinition::literal(name, &token.name));
    }

    res.token_defs.extend(token_defs);
}

fn lower(grammar: &Grammar, kinds: &KindsSrc) -> AstSrc {
    let mut res = AstSrc {
        tokens: "Whitespace Comment String Ident"
            .split_ascii_whitespace()
            .map(|it| it.to_string())
            .collect::<Vec<_>>(),
        ..Default::default()
    };

    let nodes = grammar.iter().collect_vec();

    for &node in &nodes {
        let node_data = &grammar[node];
        let name = node_data.name.clone();
        let rule = &node_data.rule;

        if name == "Tokens" {
            lower_token_defs(&mut res, grammar, kinds, rule);
            continue;
        }

        match lower_enum(grammar, rule) {
            Some(variants) => {
                let enum_src = AstEnumSrc {
                    doc: Vec::new(),
                    name,
                    traits: Vec::new(),
                    variants,
                };
                res.enums.push(enum_src);
            }
            None => {
                let mut fields = Vec::new();
                lower_rule(&mut fields, grammar, None, rule);
                res.nodes.push(AstNodeSrc {
                    doc: Vec::new(),
                    name,
                    traits: Vec::new(),
                    fields,
                });
            }
        }
    }

    deduplicate_fields(&mut res);
    extract_enums(&mut res);
    extract_struct_traits(&mut res);
    extract_enum_traits(&mut res);
    res
}

fn lower_enum(grammar: &Grammar, rule: &Rule) -> Option<Vec<String>> {
    let alternatives = match rule {
        Rule::Alt(it) => it,
        _ => return None,
    };
    let mut variants = Vec::new();
    for alternative in alternatives {
        match alternative {
            Rule::Node(it) => variants.push(grammar[*it].name.clone()),
            Rule::Token(it) if grammar[*it].name == ";" => (),
            _ => return None,
        }
    }
    Some(variants)
}

fn print_rule(rule: &Rule, grammar: &Grammar) {
    match rule {
        Rule::Node(node) => {
            print!("{}", grammar[*node].name);
        }
        Rule::Token(token) => {
            print!("{}", grammar[*token].name);
        }
        Rule::Alt(alternatives) => {
            print!("(");
            for (i, alternative) in alternatives.iter().enumerate() {
                if i != 0 {
                    print!(" | ");
                }
                print_rule(alternative, grammar);
            }
            print!(")");
        }
        Rule::Seq(seq) => {
            for (i, rule) in seq.iter().enumerate() {
                if i != 0 {
                    print!(" ");
                }
                print_rule(rule, grammar);
            }
        }
        Rule::Opt(rule) => {
            print!("(");
            print_rule(rule, grammar);
            print!(")?");
        }
        Rule::Rep(rule) => {
            print!("(");
            print_rule(rule, grammar);
            print!(")*");
        }
        Rule::Labeled { label, rule } => {
            print!("{}: ", label);
            print_rule(rule, grammar);
        }
    }
}

fn lower_rule(acc: &mut Vec<Field>, grammar: &Grammar, label: Option<&String>, rule: &Rule) {
    if lower_comma_list(acc, grammar, label, rule) {
        return;
    }

    match rule {
        Rule::Node(node) => {
            let ty = grammar[*node].name.clone();
            let name = label.cloned().unwrap_or_else(|| to_lower_snake_case(&ty));
            let field = Field::Node {
                name,
                ty,
                cardinality: Cardinality::Optional,
            };
            acc.push(field);
        }
        Rule::Token(token) => {
            assert!(label.is_none());
            let mut name = grammar[*token].name.clone();
            if name != "int_number" && name != "string" && name != "char" {
                if "[]{}()".contains(&name) {
                    name = format!("'{name}'");
                }
                let field = Field::Token(name);
                acc.push(field);
            }
        }
        Rule::Rep(inner) => {
            if let Rule::Node(node) = &**inner {
                let ty = grammar[*node].name.clone();
                let name = label
                    .cloned()
                    .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
                let field = Field::Node {
                    name,
                    ty,
                    cardinality: Cardinality::Many,
                };
                acc.push(field);
                return;
            }
            print_rule(rule, grammar);
            panic!("unhandled rule: {rule:?}")
        }
        Rule::Labeled { label: l, rule } => {
            assert!(label.is_none());
            let manually_implemented = matches!(l.as_str(), "");
            if manually_implemented {
                return;
            }
            lower_rule(acc, grammar, Some(l), rule);
        }
        Rule::Seq(rules) | Rule::Alt(rules) => {
            for rule in rules {
                lower_rule(acc, grammar, label, rule)
            }
        }
        Rule::Opt(rule) => lower_rule(acc, grammar, label, rule),
    }
}

// (T (',' T)* ','?)
fn lower_comma_list(
    acc: &mut Vec<Field>,
    grammar: &Grammar,
    label: Option<&String>,
    rule: &Rule,
) -> bool {
    let rule = match rule {
        Rule::Seq(it) => it,
        _ => return false,
    };
    let (node, repeat) = match rule.as_slice() {
        [Rule::Node(node), Rule::Rep(repeat)] => (node, repeat),
        _ => return false,
    };
    let repeat = match &**repeat {
        Rule::Seq(it) => it,
        _ => return false,
    };
    match repeat.as_slice() {
        [_comma, Rule::Node(n)] if n == node => (),
        _ => return false,
    }
    let ty = grammar[*node].name.clone();
    let name = label
        .cloned()
        .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
    let field = Field::Node {
        name,
        ty,
        cardinality: Cardinality::Many,
    };
    acc.push(field);
    true
}

fn deduplicate_fields(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        let mut i = 0;
        'outer: while i < node.fields.len() {
            for j in 0..i {
                let f1 = &node.fields[i];
                let f2 = &node.fields[j];
                if f1 == f2 {
                    node.fields.remove(i);
                    continue 'outer;
                }
            }
            i += 1;
        }
    }
}

fn extract_enums(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        for enm in &ast.enums {
            let mut to_remove = Vec::new();
            for (i, field) in node.fields.iter().enumerate() {
                let ty = field.ty().to_string();
                if enm.variants.iter().any(|it| it == &ty) {
                    to_remove.push(i);
                }
            }
            if to_remove.len() == enm.variants.len() {
                node.remove_field(to_remove);
                let ty = enm.name.clone();
                let name = to_lower_snake_case(&ty);
                node.fields.push(Field::Node {
                    name,
                    ty,
                    cardinality: Cardinality::Optional,
                });
            }
        }
    }
}

fn extract_struct_traits(ast: &mut AstSrc) {
    let traits: &[(&str, &[&str])] = &[
        ("HasIdentifier", &["identifier"]),
        ("HasDefineName", &["define_name"]),
        ("HasDefineBody", &["define_body"]),
    ];

    for node in &mut ast.nodes {
        for (name, methods) in traits {
            extract_struct_trait(node, name, methods);
        }
    }
}

fn extract_struct_trait(node: &mut AstNodeSrc, trait_name: &str, methods: &[&str]) {
    let mut to_remove = Vec::new();
    for (i, field) in node.fields.iter().enumerate() {
        let method_name = field.method_name().to_string();
        if methods.iter().any(|&it| it == method_name) {
            to_remove.push(i);
        }
    }
    if to_remove.len() == methods.len() {
        node.traits.push(trait_name.to_string());
        node.remove_field(to_remove);
    }
}

fn extract_enum_traits(ast: &mut AstSrc) {
    for enm in &mut ast.enums {
        // if enm.name == "Stmt" {
        //     continue;
        // }
        let nodes = &ast.nodes;
        let mut variant_traits = enm
            .variants
            .iter()
            .filter_map(|var| nodes.iter().find(|it| &it.name == var))
            .map(|node| node.traits.iter().cloned().collect::<BTreeSet<_>>());

        let mut enum_traits = match variant_traits.next() {
            Some(it) => it,
            None => continue,
        };
        for traits in variant_traits {
            enum_traits = enum_traits.intersection(&traits).cloned().collect();
        }
        enm.traits = enum_traits.into_iter().collect();
    }
}

impl AstNodeSrc {
    fn remove_field(&mut self, to_remove: Vec<usize>) {
        to_remove.into_iter().rev().for_each(|idx| {
            self.fields.remove(idx);
        });
    }
}
