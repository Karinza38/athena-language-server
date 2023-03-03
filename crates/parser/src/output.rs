use crate::SyntaxKind;

pub enum Step {
    Token { kind: SyntaxKind },
    Enter { kind: SyntaxKind },
    Exit,
    Error { msg: String },
}

#[derive(Default)]
pub struct Output {
    steps: Vec<Step>,
}

impl Output {
    pub fn new() -> Self {
        Self { steps: Vec::new() }
    }

    pub fn token(&mut self, kind: SyntaxKind) {
        self.steps.push(Step::Token { kind });
    }

    pub fn enter_node(&mut self, kind: SyntaxKind) {
        self.steps.push(Step::Enter { kind });
    }

    pub fn leave_node(&mut self) {
        self.steps.push(Step::Exit);
    }

    pub fn error(&mut self, msg: String) {
        self.steps.push(Step::Error { msg });
    }

    pub fn steps(&self) -> &[Step] {
        &self.steps
    }
}
