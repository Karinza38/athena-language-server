use core::fmt;

use ide_db::SymbolKind;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Highlight {
    pub tag: HlTag,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HlTag {
    Symbol(SymbolKind),

    Comment,

    Keyword,
    StringLiteral,
    CharLiteral,

    IdentLiteral,
    NumberLiteral,

    Punct(HlPunct),

    None,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HlPunct {
    /// []
    Bracket,
    /// {}
    Brace,
    /// ()
    Parenthesis,
    /// ,
    Comma,
    /// .
    Dot,
    /// :
    Colon,
    /// ;
    Semi,
    /// !
    Bang,
    /// :=
    ColonEq,
    ///
    Other,
}

impl Highlight {
    pub(crate) fn new(tag: HlTag) -> Highlight {
        Highlight { tag }
    }
    pub fn is_empty(&self) -> bool {
        self.tag == HlTag::None
    }
}

impl HlTag {
    fn as_str(self) -> &'static str {
        match self {
            HlTag::Symbol(sym) => match sym {
                SymbolKind::FnSym => "fn_sym",
                SymbolKind::Value => "value",
                SymbolKind::Sort => "sort",
                SymbolKind::Func => "func",
                SymbolKind::Module => "module",
                SymbolKind::Const => "const",
            },
            HlTag::Comment => "comment",
            HlTag::Keyword => "keyword",
            HlTag::StringLiteral => "string_literal",
            HlTag::CharLiteral => "char_literal",
            HlTag::IdentLiteral => "ident_literal",
            HlTag::NumberLiteral => "number_literal",
            HlTag::Punct(punct) => match punct {
                HlPunct::Bracket => "bracket",
                HlPunct::Brace => "brace",
                HlPunct::Parenthesis => "parenthesis",
                HlPunct::Comma => "comma",
                HlPunct::Dot => "dot",
                HlPunct::Colon => "colon",
                HlPunct::Semi => "semi",
                HlPunct::Bang => "bang",
                HlPunct::ColonEq => "colon_eq",
                HlPunct::Other => "other",
            },
            HlTag::None => "none",
        }
    }
}

impl fmt::Display for HlTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Display for Highlight {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.tag.fmt(f)
    }
}

impl From<HlTag> for Highlight {
    fn from(tag: HlTag) -> Highlight {
        Highlight::new(tag)
    }
}

impl From<SymbolKind> for Highlight {
    fn from(sym: SymbolKind) -> Highlight {
        Highlight::new(HlTag::Symbol(sym))
    }
}

impl From<HlPunct> for Highlight {
    fn from(punct: HlPunct) -> Highlight {
        Highlight::new(HlTag::Punct(punct))
    }
}
