use static_assertions::const_assert;

use crate::SyntaxKind;

type Repr = u128;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct TokenSet(Repr);

impl TokenSet {
    pub(crate) const fn new(kinds: &[SyntaxKind]) -> Self {
        let mut res = 0;
        let mut i = 0;
        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1;
        }
        Self(res)
    }

    #[allow(dead_code)]
    pub(crate) const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub(crate) const fn contains(self, kind: SyntaxKind) -> bool {
        self.0 & mask(kind) != 0
    }
}

const fn mask(kind: SyntaxKind) -> Repr {
    1 << (kind as usize)
}

const_assert!((SyntaxKind::LAST_TOKEN as usize) < (Repr::BITS as usize));
