//! ink! function IR.

use ra_ap_syntax::ast;

/// An ink! function.
#[ink_analyzer_macro::entity(arg_kind = Function)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    // ASTNode type.
    ast: ast::Fn,
}

impl_ast_type_trait!(Function, IsInkFn);

impl_is_chain_extension_fn!(Function, Function);

impl Function {
    /// Returns the function id (if any).
    pub fn id(&self) -> Option<u16> {
        self.function_arg()?.value()?.as_u16()
    }

    impl_pub_ink_arg_getter!(function_arg, Function, function);

    impl_pub_ink_arg_getter!(handle_status_arg, HandleStatus, handle_status);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{InkEntity, IsInkFn};
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, has_handle_status) in [
            (
                quote_as_str! {
                    #[ink(function=1)]
                    fn my_function();
                },
                false,
            ),
            (
                quote_as_str! {
                    #[ink(function=0x1)]
                    fn my_function();
                },
                false,
            ),
            (
                quote_as_str! {
                    #[ink(function=1, handle_status=false)]
                    fn my_function();
                },
                true,
            ),
            (
                quote_as_str! {
                    #[ink(function=1)]
                    #[ink(handle_status=true)]
                    fn my_function();
                },
                true,
            ),
        ] {
            let node = parse_first_syntax_node(code);

            let function = Function::cast(node).unwrap();

            // `function_arg` argument exists.
            assert!(function.function_arg().is_some());

            // `handle_status` argument exists.
            assert_eq!(function.handle_status_arg().is_some(), has_handle_status);

            // `fn` item exists.
            assert!(function.fn_item().is_some());
        }
    }
}
