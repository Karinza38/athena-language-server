/// Defines input for code generation process.
use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub(crate) struct KindsSrc {
    pub(crate) punct: Vec<(String, String)>,
    pub(crate) keywords: Vec<String>,
    pub(crate) contextual_keywords: Vec<String>,
    pub(crate) literals: Vec<String>,
    pub(crate) tokens: Vec<String>,
    pub(crate) nodes: Vec<String>,
}

#[derive(Default, Debug)]
pub(crate) struct AstSrc {
    pub(crate) tokens: Vec<String>,
    pub(crate) nodes: Vec<AstNodeSrc>,
    pub(crate) enums: Vec<AstEnumSrc>,
    pub(crate) token_defs: Vec<AstTokenDefinition>,
}

#[derive(Debug)]
pub(crate) struct AstNodeSrc {
    pub(crate) doc: Vec<String>,
    pub(crate) name: String,
    pub(crate) traits: Vec<String>,
    pub(crate) fields: Vec<Field>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Field {
    Token(String),
    Node {
        name: String,
        ty: String,
        cardinality: Cardinality,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Cardinality {
    Optional,
    Many,
}

#[derive(Debug)]
pub(crate) struct AstEnumSrc {
    pub(crate) doc: Vec<String>,
    pub(crate) name: String,
    pub(crate) traits: Vec<String>,
    pub(crate) variants: Vec<String>,
}

#[derive(Debug)]
pub(crate) struct AstTokenDefinition {
    pub(crate) name: String,
    pub(crate) def: AstTokenDef,
}

impl AstTokenDefinition {
    pub(crate) fn regex(name: impl Into<String>, regex: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            def: AstTokenDef::Regex(regex.into()),
        }
    }

    pub(crate) fn literal(name: impl Into<String>, literal: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            def: AstTokenDef::Literal(literal.into()),
        }
    }

    // this is a terrible name
    pub(crate) fn raw_token(&self) -> String {
        match self {
            AstTokenDefinition {
                def: AstTokenDef::Literal(l),
                ..
            } => match l.as_str() {
                "{" => "'{'",
                "}" => "'}'",
                "(" => "'('",
                ")" => "')'",
                "[" => "'['",
                "]" => "']'",
                _ => l,
            }
            .into(),
            AstTokenDefinition { name, .. } => name.clone(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum AstTokenDef {
    Literal(String),
    Regex(String),
}
