//! ink! e2e test IR.

use ra_ap_syntax::ast;

use crate::traits::IsInkFn;

/// An ink! e2e test.
#[ink_analyzer_macro::entity(macro_kind = E2ETest)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InkE2ETest {
    // ASTNode type.
    ast: ast::Fn,
}

impl IsInkFn for InkE2ETest {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ast.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
    use ra_ap_syntax::AstNode;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node: ast::Fn = parse_first_ast_node_of_type(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
            }
        });

        let ink_e2e_test = InkE2ETest::cast(node.syntax().clone()).unwrap();

        // `fn` item exists.
        assert!(ink_e2e_test.fn_item().is_some());
    }
}
