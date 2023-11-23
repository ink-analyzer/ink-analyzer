//! ink! e2e test IR.

use ra_ap_syntax::ast;

/// An ink! e2e test.
#[ink_analyzer_macro::entity(macro_kind = E2ETest)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InkE2ETest {
    // ASTNode type.
    ast: ast::Fn,
}

impl_ast_type_trait!(InkE2ETest, IsInkFn);

impl InkE2ETest {
    impl_pub_ink_arg_getter!(
        additional_contracts_arg,
        AdditionalContracts,
        additional_contracts
    );

    impl_pub_ink_arg_getter!(environment_arg, Environment, environment);

    impl_pub_ink_arg_getter!(keep_attr_arg, KeepAttr, keep_attr);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{InkEntity, IsInkFn};
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
