//! Defines input for code generation process.

pub(crate) struct KindsSrc<'a> {
    pub(crate) punct: &'a [(&'a str, &'a str)],
    pub(crate) keywords: &'a [&'a str],
    pub(crate) contextual_keywords: &'a [&'a str],
    pub(crate) literals: &'a [&'a str],
    pub(crate) tokens: &'a [&'a str],
    pub(crate) nodes: &'a [&'a str],
}

pub(crate) const KINDS_SRC: KindsSrc<'_> = KindsSrc {
    punct: &[
        ("(", "L_PAREN"),
        (")", "R_PAREN"),
        ("{", "L_CURLY"),
        ("}", "R_CURLY"),
        ("[", "L_BRACK"),
        ("]", "R_BRACK"),
        ("?", "QUESTION"),
        ("|", "PIPE"),
        ("_", "UNDERSCORE"),
        (":", "COLON"),
        ("=>", "FAT_ARROW"),
        ("!", "BANG"),
        ("&&", "AMP2"),
        ("||", "PIPE2"),
        (":=", "COLON_EQ"),
        ("'", "SINGLE_QUOTE"),
        (";", "SEMI"),
        ("->", "THIN_ARROW"),
        (",", "COMMA"),
    ],
    keywords: &[
        "while",
        "let",
        "letrec",
        "try",
        "check",
        "lambda",
        "method",
        "match",
        "cell",
        "set!",
        "ref",
        "make-vector",
        "vector-sub",
        "vector-set!",
        "seq",
        "apply-method",
        "conclude",
        "assume",
        "suppose-absurd",
        "generalize-over",
        "pick-any",
        "with-witness",
        "pick-witness",
        "pick-witnesses",
        "by-induction",
        "datatype-cases",
        "some-var",
        "some-sent-con",
        "some-quant",
        "some-term",
        "some-atom",
        "some-sentence",
        "some-list",
        "some-cell",
        "some-vector",
        "some-proc",
        "some-method",
        "some-symbol",
        "some-table",
        "some-map",
        "some-sub",
        "some-char",
        "split",
        "where",
        "list-of",
        "val-of",
        "as",
        "bind",
        "for",
        "define",
        "module",
        "declare",
        "domain",
        "domains",
    ],
    contextual_keywords: &[],
    literals: &["INT_NUMBER", "STRING", "CHAR"],
    tokens: &["ERROR", "IDENT", "WHITESPACE", "COMMENT"],
    nodes: &[
        "SOURCE_FILE",
        "IDENTIFIER",
        "LITERAL",
        "META_IDENT",
        "UNIT",
        // Sorts
        "IDENT_SORT",
        "VAR_SORT",
        "COMPOUND_SORT",
        // Phrases
        "EXPR_PHRASE",
        "DED_PHRASE",
        // Exprs
        "IDENT_EXPR",
        "LITERAL_EXPR",
        "UNIT_EXPR",
        "META_IDENT_EXPR",
        "TERM_VAR_EXPR",
        "CHECK_EXPR",
        "LAMBDA_EXPR",
        "APPLICATION_EXPR",
        "LIST_EXPR",
        "METHOD_EXPR",
        "LET_EXPR",
        "LET_REC_EXPR",
        "MATCH_EXPR",
        "TRY_EXPR",
        "CELL_EXPR",
        "SET_EXPR",
        "REF_EXPR",
        "WHILE_EXPR",
        "MAKE_VECTOR_EXPR",
        "VECTOR_SUB_EXPR",
        "VECTOR_SET_EXPR",
        "SEQ_EXPR",
        "AND_EXPR",
        "OR_EXPR",
        // Deductions
        "METHOD_CALL_DED",
        "BANG_METHOD_CALL_DED",
        "ASSUME_DED",
        "NAMED_ASSUME_DED",
        "PROOF_BY_CONTRA_DED",
        "GENERALIZE_OVER_DED",
        "PICK_ANY_DED",
        "EXISTENTIAL_INSTANT_DED",
        "INDUCT_DED",
        "CASES_DED",
        "CHECK_DED",
        "MATCH_DED",
        "LET_DED",
        "LET_REC_DED",
        "TRY_DED",
        "TRY_DED_ARM",
        "MATCH_DED_ARM",
        "CHECK_DED_ARM",
        "RESTRICTED_MATCH_DED",
        "RESTRICTED_NAMED_PAT",
        "RESTRICTED_APPLY_PAT",
        "ASSUME_PART",
        // Patterns
        "IDENT_PAT",
        "ANNOTATED_IDENT_PAT",
        "VAR_PAT",
        "META_IDENT_PAT",
        "LITERAL_PAT",
        "UNIT_PAT",
        "WILDCARD_PAT",
        "NAMED_PAT",
        "VAL_OF_PAT",
        "LIST_OF_PAT",
        "SPLIT_PAT",
        "LIST_PAT",
        "COMPOUND_PAT",
        "WHERE_PAT",
        "SOME_THING_PAT",
        "SOME_THING",
        // Misc
        "MATCH_ARM",
        "TRY_ARM",
        "LET_PART",
        "CHECK_ARM",
        "LET_REC_PART",
        // Directives
        "FUNC_SORTS",
        "SORT_VARS_DECL",
        "COMPOUND_SORT_DECL",
        "DEFINE_DIR",
        "DEFINE_MULTI_DIR",
        "DEFINE_PROC_DIR",
        "DOMAIN_DIR",
        "DOMAINS_DIR",
        "DECLARE_DIR",
        "MODULE_DIR",
        // Statements
        "DIR_STMT",
        "PHRASE_STMT",
    ],
};

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
