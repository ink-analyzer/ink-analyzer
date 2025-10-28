//! ink! constructor IR.

use ra_ap_syntax::ast;

use crate::traits::IsInkCallable;

/// An ink! constructor.
#[ink_analyzer_macro::entity(arg_kind = Constructor)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constructor {
    // ASTNode type.
    ast: ast::Fn,
}

impl_ast_type_trait!(Constructor, IsInkFn);

impl IsInkCallable for Constructor {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{InkEntity, IsInkFn};
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, is_payable, has_selector, is_default, has_name) in [
            (
                quote_as_str! {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {}
                },
                false,
                false,
                false,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(constructor, payable, default, selector=1, name="myConstructor")]
                    pub fn my_constructor() -> Self {}
                },
                true,
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink(constructor)]
                    #[ink(payable, default, selector=1, name="myConstructor")]
                    pub fn my_constructor() -> Self {}
                },
                true,
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink(constructor)]
                    #[ink(payable)]
                    #[ink(default)]
                    #[ink(selector=1)]
                    #[ink(name="myConstructor")]
                    pub fn my_constructor() -> Self {}
                },
                true,
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink(constructor, selector=0xA)]
                    pub fn my_constructor() -> Self {}
                },
                false,
                true,
                false,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(constructor, selector=_)]
                    pub fn my_constructor() -> Self {}
                },
                false,
                true,
                false,
                false,
            ),
        ] {
            let node = parse_first_syntax_node(code);

            let constructor = Constructor::cast(node).unwrap();

            // `payable` argument exists.
            assert_eq!(constructor.payable_arg().is_some(), is_payable);

            // `selector` argument exists.
            assert_eq!(constructor.selector_arg().is_some(), has_selector);

            // `default` argument exists.
            assert_eq!(constructor.default_arg().is_some(), is_default);

            // `name` argument exists.
            assert_eq!(constructor.name_arg().is_some(), has_name);

            // composed selector exists.
            assert!(constructor.composed_selector().is_some());

            // `fn` item exists.
            assert!(constructor.fn_item().is_some());
        }
    }
}
