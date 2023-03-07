use crate::SyntaxKind;

#[derive(Default)]
pub struct Input {
    kind: Vec<SyntaxKind>,
    contextual_kind: Vec<SyntaxKind>,
}

impl Input {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self, kind: SyntaxKind) {
        self.push_impl(kind, SyntaxKind::EOF);
    }

    pub fn push_ident(&mut self, contextual_kind: SyntaxKind) {
        self.push_impl(SyntaxKind::IDENT, contextual_kind);
    }

    pub(crate) fn kind(&self, idx: usize) -> SyntaxKind {
        self.kind.get(idx).copied().unwrap_or(SyntaxKind::EOF)
    }

    fn push_impl(&mut self, kind: SyntaxKind, contextual_kind: SyntaxKind) {
        self.kind.push(kind);
        self.contextual_kind.push(contextual_kind);
    }

    pub(crate) fn contextual_kind(&self, idx: usize) -> SyntaxKind {
        self.contextual_kind
            .get(idx)
            .copied()
            .unwrap_or(SyntaxKind::EOF)
    }
}
