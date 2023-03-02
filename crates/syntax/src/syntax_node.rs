use rowan::{GreenNodeBuilder, Language};

use crate::{Parse, SyntaxError, SyntaxKind, TextSize};

pub(crate) use rowan::{GreenNode, GreenToken, NodeOrToken};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AthenaLanguage {}
impl Language for AthenaLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<AthenaLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<AthenaLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<AthenaLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<AthenaLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<AthenaLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<AthenaLanguage>;

#[derive(Default)]
pub struct SyntaxTreeBuilder {
    errors: Vec<SyntaxError>,
    inner: GreenNodeBuilder<'static>,
}

impl SyntaxTreeBuilder {
    pub(crate) fn finish_raw(self) -> (GreenNode, Vec<SyntaxError>) {
        let green = self.inner.finish();
        (green, self.errors)
    }

    pub fn finish(self) -> Parse<SyntaxNode> {
        let (green, errors) = self.finish_raw();
        // Disable block validation, see https://github.com/rust-lang/rust-analyzer/pull/10357
        // #[allow(clippy::overly_complex_bool_expr)]
        // if cfg!(debug_assertions) && false {
        //     let node = SyntaxNode::new_root(green.clone());
        //     crate::validation::validate_block_structure(&node);
        // }
        Parse::new(green, errors)
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        let kind = AthenaLanguage::kind_to_raw(kind);
        self.inner.token(kind, text);
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        let kind = AthenaLanguage::kind_to_raw(kind);
        self.inner.start_node(kind);
    }

    pub fn finish_node(&mut self) {
        self.inner.finish_node();
    }

    pub fn error(&mut self, error: String, text_pos: TextSize) {
        self.errors
            .push(SyntaxError::new_at_offset(error, text_pos));
    }
}
