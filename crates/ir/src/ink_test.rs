//! ink! test IR.

use ra_ap_syntax::ast;

use crate::traits::IsInkFn;

/// An ink! test.
#[ink_analyzer_macro::entity(macro_kind = Test)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InkTest {
    // ASTNode type.
    ast: ast::Fn,
}

impl IsInkFn for InkTest {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ast.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node = parse_first_syntax_node(quote_as_str! {
            #[ink::test]
            fn it_works() {
            }
        });

        let ink_test = InkTest::cast(node).unwrap();

        // `fn` item exists.
        assert!(ink_test.fn_item().is_some());
    }
}
