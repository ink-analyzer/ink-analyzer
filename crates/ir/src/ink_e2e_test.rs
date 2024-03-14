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

impl_has_ink_environment!(InkE2ETest, Environment);

impl InkE2ETest {
    impl_pub_ink_arg_getter!(
        additional_contracts_arg,
        AdditionalContracts,
        additional_contracts
    );

    impl_pub_ink_arg_getter!(backend_arg, Backend, backend);

    impl_pub_ink_arg_getter!(environment_arg, Environment, environment);

    impl_pub_ink_arg_getter!(keep_attr_arg, KeepAttr, keep_attr);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{InkEntity, IsInkFn};
    use quote::quote;
    use ra_ap_syntax::AstNode;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (args, has_additional_contracts, has_backend, has_environment, has_keep_attr) in [
            (quote! {}, false, false, false, false),
            (
                quote! { (additional_contracts = "adder/Cargo.toml flipper/Cargo.toml") },
                true,
                false,
                false,
                false,
            ),
            (quote! { (backend(node)) }, false, true, false, false),
            (
                quote! { (backend(node(url = "ws://127.0.0.1:8000"))) },
                false,
                true,
                false,
                false,
            ),
            (
                quote! { (backend(runtime_only)) },
                false,
                true,
                false,
                false,
            ),
            (
                quote! { (backend(runtime_only(sandbox = ink_e2e::MinimalSandbox))) },
                false,
                true,
                false,
                false,
            ),
            (
                quote! { (environment = ink::env::DefaultEnvironment) },
                false,
                false,
                true,
                false,
            ),
            (
                quote! { (keep_attr = "foo, bar") },
                false,
                false,
                false,
                true,
            ),
        ] {
            let node: ast::Fn = parse_first_ast_node_of_type(quote_as_str! {
                type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                #[ink_e2e::test #args]
                async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
                }
            });

            let ink_e2e_test = InkE2ETest::cast(node.syntax().clone()).unwrap();

            // `fn` item exists.
            assert!(ink_e2e_test.fn_item().is_some());

            // `additional_contracts` argument exists.
            assert_eq!(
                ink_e2e_test.additional_contracts_arg().is_some(),
                has_additional_contracts
            );

            // `backend` argument exists.
            assert_eq!(ink_e2e_test.backend_arg().is_some(), has_backend);

            // `environment` argument exists.
            assert_eq!(ink_e2e_test.environment_arg().is_some(), has_environment);

            // `keep_attr` argument exists.
            assert_eq!(ink_e2e_test.keep_attr_arg().is_some(), has_keep_attr);
        }
    }
}
