use crate::SyntaxKind;

#[derive(Default)]
pub struct Input {
    kind: Vec<SyntaxKind>,
}

impl Input {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self, kind: SyntaxKind) {
        self.kind.push(kind);
    }

    pub(crate) fn kind(&self, idx: usize) -> SyntaxKind {
        self.kind.get(idx).copied().unwrap_or(SyntaxKind::EOF)
    }
}
