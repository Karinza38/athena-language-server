mod ast;

pub(crate) fn ast() -> Result<(), crate::Error> {
    ast::sourcegen_ast()
}
