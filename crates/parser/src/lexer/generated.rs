//! Generated by `sourcegen_ast`, do not edit by hand.

#![allow(bad_style, missing_docs, unreachable_pub)]
use logos::Logos;
use crate::{SyntaxKind, T};
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum LexerToken {
    #[regex(
        r###"(::)|~|([%&*+\--9<->@-Z\\^_a-z|](!|[#-']|[*+]|[\--9]|[<-\\]|\^|[_-z]|\|)*)"###,
        |lex|lex.slice().len()
    )]
    Ident(usize),
    #[regex(
        r###""([ ~#-\[\]-~]|(?:\\\d+)|(?:\\["\\abnrftv])|(?:\\\^[A-Z@\[\]/^]))*""###,
        |lex|lex.slice().len()
    )]
    String(usize),
    #[regex(
        r###"`([ ~#-\[\]-~]|(?:\\\d+)|(?:\\["\\abnrftv])|(?:\\\^[A-Z@\[\]/^]))"###,
        |lex|lex.slice().len()
    )]
    Char(usize),
    #[regex(r###"[ \t\n\r]+"###, |lex|lex.slice().len())]
    Whitespace(usize),
    #[regex(r###"#.*"###, |lex|lex.slice().len())]
    Comment(usize),
    #[token("'", |lex|lex.slice().len())]
    SingleQuote(usize),
    #[token("(", |lex|lex.slice().len())]
    LParen(usize),
    #[token(")", |lex|lex.slice().len())]
    RParen(usize),
    #[token(":", |lex|lex.slice().len())]
    Colon(usize),
    #[token("_", |lex|lex.slice().len())]
    Underscore(usize),
    #[token("OP", |lex|lex.slice().len())]
    Op(usize),
    #[token(":=", |lex|lex.slice().len())]
    ColonEq(usize),
    #[token("datatype", |lex|lex.slice().len())]
    Datatype(usize),
    #[token("structure", |lex|lex.slice().len())]
    Structure(usize),
    #[token("datatypes", |lex|lex.slice().len())]
    Datatypes(usize),
    #[token("&&", |lex|lex.slice().len())]
    Ampamp(usize),
    #[token("structures", |lex|lex.slice().len())]
    Structures(usize),
    #[token("module", |lex|lex.slice().len())]
    Module(usize),
    #[token("{", |lex|lex.slice().len())]
    LCurly(usize),
    #[token("}", |lex|lex.slice().len())]
    RCurly(usize),
    #[token("extend-module", |lex|lex.slice().len())]
    ExtendModule(usize),
    #[token("domain", |lex|lex.slice().len())]
    Domain(usize),
    #[token("domains", |lex|lex.slice().len())]
    Domains(usize),
    #[token(",", |lex|lex.slice().len())]
    Comma(usize),
    #[token("declare", |lex|lex.slice().len())]
    Declare(usize),
    #[token("->", |lex|lex.slice().len())]
    ThinArrow(usize),
    #[token("[", |lex|lex.slice().len())]
    LBrack(usize),
    #[token("]", |lex|lex.slice().len())]
    RBrack(usize),
    #[token("left-assoc", |lex|lex.slice().len())]
    LeftAssoc(usize),
    #[token("right-assoc", |lex|lex.slice().len())]
    RightAssoc(usize),
    #[token("define", |lex|lex.slice().len())]
    Define(usize),
    #[token("private", |lex|lex.slice().len())]
    Private(usize),
    #[token("as", |lex|lex.slice().len())]
    As(usize),
    #[token("load", |lex|lex.slice().len())]
    Load(usize),
    #[token("assert", |lex|lex.slice().len())]
    Assert(usize),
    #[token("assert*", |lex|lex.slice().len())]
    AssertStar(usize),
    #[token("open", |lex|lex.slice().len())]
    Open(usize),
    #[token("primitive-method", |lex|lex.slice().len())]
    PrimitiveMethod(usize),
    #[token("?", |lex|lex.slice().len())]
    QuestionMark(usize),
    #[token("check", |lex|lex.slice().len())]
    Check(usize),
    #[token("=>", |lex|lex.slice().len())]
    FatArrow(usize),
    #[token("lambda", |lex|lex.slice().len())]
    Lambda(usize),
    #[token("method", |lex|lex.slice().len())]
    Method(usize),
    #[token("let", |lex|lex.slice().len())]
    Let(usize),
    #[token("letrec", |lex|lex.slice().len())]
    Letrec(usize),
    #[token("match", |lex|lex.slice().len())]
    Match(usize),
    #[token("try", |lex|lex.slice().len())]
    Try(usize),
    #[token("cell", |lex|lex.slice().len())]
    Cell(usize),
    #[token("set!", |lex|lex.slice().len())]
    SetBang(usize),
    #[token("ref", |lex|lex.slice().len())]
    Ref(usize),
    #[token("while", |lex|lex.slice().len())]
    While(usize),
    #[token("make-vector", |lex|lex.slice().len())]
    MakeVector(usize),
    #[token("vector-sub", |lex|lex.slice().len())]
    VectorSub(usize),
    #[token("vector-set!", |lex|lex.slice().len())]
    VectorSetBang(usize),
    #[token("seq", |lex|lex.slice().len())]
    Seq(usize),
    #[token("||", |lex|lex.slice().len())]
    Pipepipe(usize),
    #[token("|{", |lex|lex.slice().len())]
    PipeCurly(usize),
    #[token("}|", |lex|lex.slice().len())]
    CurlyPipe(usize),
    #[token("conclude", |lex|lex.slice().len())]
    Conclude(usize),
    #[token("apply-method", |lex|lex.slice().len())]
    ApplyMethod(usize),
    #[token("!", |lex|lex.slice().len())]
    Bang(usize),
    #[token("assume", |lex|lex.slice().len())]
    Assume(usize),
    #[token(";", |lex|lex.slice().len())]
    Semicolon(usize),
    #[token("suppose-absurd", |lex|lex.slice().len())]
    SupposeAbsurd(usize),
    #[token("generalize-over", |lex|lex.slice().len())]
    GeneralizeOver(usize),
    #[token("pick-any", |lex|lex.slice().len())]
    PickAny(usize),
    #[token("with-witness", |lex|lex.slice().len())]
    WithWitness(usize),
    #[token("pick-witness", |lex|lex.slice().len())]
    PickWitness(usize),
    #[token("for", |lex|lex.slice().len())]
    For(usize),
    #[token("pick-witnesses", |lex|lex.slice().len())]
    PickWitnesses(usize),
    #[token("by-induction", |lex|lex.slice().len())]
    ByInduction(usize),
    #[token("datatype-cases", |lex|lex.slice().len())]
    DatatypeCases(usize),
    #[token("on", |lex|lex.slice().len())]
    On(usize),
    #[token("dmatch", |lex|lex.slice().len())]
    Dmatch(usize),
    #[token("begin", |lex|lex.slice().len())]
    Begin(usize),
    #[token("end", |lex|lex.slice().len())]
    End(usize),
    #[token("from", |lex|lex.slice().len())]
    From(usize),
    #[token("by", |lex|lex.slice().len())]
    By(usize),
    #[token("bind", |lex|lex.slice().len())]
    Bind(usize),
    #[token("val-of", |lex|lex.slice().len())]
    ValOf(usize),
    #[token("list-of", |lex|lex.slice().len())]
    ListOf(usize),
    #[token("split", |lex|lex.slice().len())]
    Split(usize),
    #[token("where", |lex|lex.slice().len())]
    Where(usize),
    #[token("some-var", |lex|lex.slice().len())]
    SomeVar(usize),
    #[token("some-sent-con", |lex|lex.slice().len())]
    SomeSentCon(usize),
    #[token("some-quant", |lex|lex.slice().len())]
    SomeQuant(usize),
    #[token("some-term", |lex|lex.slice().len())]
    SomeTerm(usize),
    #[token("some-atom", |lex|lex.slice().len())]
    SomeAtom(usize),
    #[token("some-sentence", |lex|lex.slice().len())]
    SomeSentence(usize),
    #[token("some-list", |lex|lex.slice().len())]
    SomeList(usize),
    #[token("some-cell", |lex|lex.slice().len())]
    SomeCell(usize),
    #[token("some-vector", |lex|lex.slice().len())]
    SomeVector(usize),
    #[token("some-proc", |lex|lex.slice().len())]
    SomeProc(usize),
    #[token("some-method", |lex|lex.slice().len())]
    SomeMethod(usize),
    #[token("some-symbol", |lex|lex.slice().len())]
    SomeSymbol(usize),
    #[token("some-table", |lex|lex.slice().len())]
    SomeTable(usize),
    #[token("some-map", |lex|lex.slice().len())]
    SomeMap(usize),
    #[token("some-sub", |lex|lex.slice().len())]
    SomeSub(usize),
    #[token("some-char", |lex|lex.slice().len())]
    SomeChar(usize),
    #[error]
    Error,
}
impl LexerToken {
    pub(crate) fn to_syntax_kind(self) -> SyntaxKind {
        match self {
            Self::SingleQuote(..) => T!['\''],
            Self::LParen(..) => T!['('],
            Self::RParen(..) => T![')'],
            Self::Colon(..) => T![:],
            Self::Underscore(..) => T![_],
            Self::Op(..) => T![OP],
            Self::ColonEq(..) => T![:=],
            Self::Datatype(..) => T![datatype],
            Self::Structure(..) => T![structure],
            Self::Datatypes(..) => T![datatypes],
            Self::Ampamp(..) => T![&&],
            Self::Structures(..) => T![structures],
            Self::Module(..) => T![module],
            Self::LCurly(..) => T!['{'],
            Self::RCurly(..) => T!['}'],
            Self::ExtendModule(..) => T![extend - module],
            Self::Domain(..) => T![domain],
            Self::Domains(..) => T![domains],
            Self::Comma(..) => T![,],
            Self::Declare(..) => T![declare],
            Self::ThinArrow(..) => T![->],
            Self::LBrack(..) => T!['['],
            Self::RBrack(..) => T![']'],
            Self::LeftAssoc(..) => T![left - assoc],
            Self::RightAssoc(..) => T![right - assoc],
            Self::Define(..) => T![define],
            Self::Private(..) => T![private],
            Self::As(..) => T![as],
            Self::Load(..) => T![load],
            Self::Assert(..) => T![assert],
            Self::AssertStar(..) => T![assert *],
            Self::Open(..) => T![open],
            Self::PrimitiveMethod(..) => T![primitive - method],
            Self::QuestionMark(..) => T![?],
            Self::Check(..) => T![check],
            Self::FatArrow(..) => T![=>],
            Self::Lambda(..) => T![lambda],
            Self::Method(..) => T![method],
            Self::Let(..) => T![let],
            Self::Letrec(..) => T![letrec],
            Self::Match(..) => T![match],
            Self::Try(..) => T![try],
            Self::Cell(..) => T![cell],
            Self::SetBang(..) => T![set!],
            Self::Ref(..) => T![ref],
            Self::While(..) => T![while],
            Self::MakeVector(..) => T![make - vector],
            Self::VectorSub(..) => T![vector - sub],
            Self::VectorSetBang(..) => T![vector - set!],
            Self::Seq(..) => T![seq],
            Self::Pipepipe(..) => T![||],
            Self::PipeCurly(..) => T!["|{"],
            Self::CurlyPipe(..) => T!["}|"],
            Self::Conclude(..) => T![conclude],
            Self::ApplyMethod(..) => T![apply - method],
            Self::Bang(..) => T![!],
            Self::Assume(..) => T![assume],
            Self::Semicolon(..) => T![;],
            Self::SupposeAbsurd(..) => T![suppose - absurd],
            Self::GeneralizeOver(..) => T![generalize - over],
            Self::PickAny(..) => T![pick - any],
            Self::WithWitness(..) => T![with - witness],
            Self::PickWitness(..) => T![pick - witness],
            Self::For(..) => T![for],
            Self::PickWitnesses(..) => T![pick - witnesses],
            Self::ByInduction(..) => T![by - induction],
            Self::DatatypeCases(..) => T![datatype - cases],
            Self::On(..) => T![on],
            Self::Dmatch(..) => T![dmatch],
            Self::Begin(..) => T![begin],
            Self::End(..) => T![end],
            Self::From(..) => T![from],
            Self::By(..) => T![by],
            Self::Bind(..) => T![bind],
            Self::ValOf(..) => T![val - of],
            Self::ListOf(..) => T![list - of],
            Self::Split(..) => T![split],
            Self::Where(..) => T![where],
            Self::SomeVar(..) => T![some - var],
            Self::SomeSentCon(..) => T![some - sent - con],
            Self::SomeQuant(..) => T![some - quant],
            Self::SomeTerm(..) => T![some - term],
            Self::SomeAtom(..) => T![some - atom],
            Self::SomeSentence(..) => T![some - sentence],
            Self::SomeList(..) => T![some - list],
            Self::SomeCell(..) => T![some - cell],
            Self::SomeVector(..) => T![some - vector],
            Self::SomeProc(..) => T![some - proc],
            Self::SomeMethod(..) => T![some - method],
            Self::SomeSymbol(..) => T![some - symbol],
            Self::SomeTable(..) => T![some - table],
            Self::SomeMap(..) => T![some - map],
            Self::SomeSub(..) => T![some - sub],
            Self::SomeChar(..) => T![some - char],
            Self::Ident(..) => SyntaxKind::IDENT,
            Self::String(..) => SyntaxKind::STRING,
            Self::Char(..) => SyntaxKind::CHAR,
            Self::Whitespace(..) => SyntaxKind::WHITESPACE,
            Self::Comment(..) => SyntaxKind::COMMENT,
            Self::Error => SyntaxKind::ERROR,
        }
    }
    pub(crate) fn len(self) -> usize {
        match self {
            Self::SingleQuote(len) => len,
            Self::LParen(len) => len,
            Self::RParen(len) => len,
            Self::Colon(len) => len,
            Self::Underscore(len) => len,
            Self::Op(len) => len,
            Self::ColonEq(len) => len,
            Self::Datatype(len) => len,
            Self::Structure(len) => len,
            Self::Datatypes(len) => len,
            Self::Ampamp(len) => len,
            Self::Structures(len) => len,
            Self::Module(len) => len,
            Self::LCurly(len) => len,
            Self::RCurly(len) => len,
            Self::ExtendModule(len) => len,
            Self::Domain(len) => len,
            Self::Domains(len) => len,
            Self::Comma(len) => len,
            Self::Declare(len) => len,
            Self::ThinArrow(len) => len,
            Self::LBrack(len) => len,
            Self::RBrack(len) => len,
            Self::LeftAssoc(len) => len,
            Self::RightAssoc(len) => len,
            Self::Define(len) => len,
            Self::Private(len) => len,
            Self::As(len) => len,
            Self::Load(len) => len,
            Self::Assert(len) => len,
            Self::AssertStar(len) => len,
            Self::Open(len) => len,
            Self::PrimitiveMethod(len) => len,
            Self::QuestionMark(len) => len,
            Self::Check(len) => len,
            Self::FatArrow(len) => len,
            Self::Lambda(len) => len,
            Self::Method(len) => len,
            Self::Let(len) => len,
            Self::Letrec(len) => len,
            Self::Match(len) => len,
            Self::Try(len) => len,
            Self::Cell(len) => len,
            Self::SetBang(len) => len,
            Self::Ref(len) => len,
            Self::While(len) => len,
            Self::MakeVector(len) => len,
            Self::VectorSub(len) => len,
            Self::VectorSetBang(len) => len,
            Self::Seq(len) => len,
            Self::Pipepipe(len) => len,
            Self::PipeCurly(len) => len,
            Self::CurlyPipe(len) => len,
            Self::Conclude(len) => len,
            Self::ApplyMethod(len) => len,
            Self::Bang(len) => len,
            Self::Assume(len) => len,
            Self::Semicolon(len) => len,
            Self::SupposeAbsurd(len) => len,
            Self::GeneralizeOver(len) => len,
            Self::PickAny(len) => len,
            Self::WithWitness(len) => len,
            Self::PickWitness(len) => len,
            Self::For(len) => len,
            Self::PickWitnesses(len) => len,
            Self::ByInduction(len) => len,
            Self::DatatypeCases(len) => len,
            Self::On(len) => len,
            Self::Dmatch(len) => len,
            Self::Begin(len) => len,
            Self::End(len) => len,
            Self::From(len) => len,
            Self::By(len) => len,
            Self::Bind(len) => len,
            Self::ValOf(len) => len,
            Self::ListOf(len) => len,
            Self::Split(len) => len,
            Self::Where(len) => len,
            Self::SomeVar(len) => len,
            Self::SomeSentCon(len) => len,
            Self::SomeQuant(len) => len,
            Self::SomeTerm(len) => len,
            Self::SomeAtom(len) => len,
            Self::SomeSentence(len) => len,
            Self::SomeList(len) => len,
            Self::SomeCell(len) => len,
            Self::SomeVector(len) => len,
            Self::SomeProc(len) => len,
            Self::SomeMethod(len) => len,
            Self::SomeSymbol(len) => len,
            Self::SomeTable(len) => len,
            Self::SomeMap(len) => len,
            Self::SomeSub(len) => len,
            Self::SomeChar(len) => len,
            Self::Ident(len) => len,
            Self::String(len) => len,
            Self::Char(len) => len,
            Self::Whitespace(len) => len,
            Self::Comment(len) => len,
            Self::Error => 0,
        }
    }
}
