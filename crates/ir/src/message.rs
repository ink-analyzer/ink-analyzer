//! ink! message IR.

use ra_ap_syntax::ast;

use crate::traits::{IsInkCallable, IsInkFn};

/// An ink! message.
#[ink_analyzer_macro::entity(arg_kind = Message)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Message {
    // ASTNode type.
    ast: ast::Fn,
}

impl IsInkFn for Message {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ast.as_ref()
    }
}

impl IsInkCallable for Message {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, is_payable, has_selector, is_default) in [
            (
                quote_as_str! {
                    #[ink(message)]
                    pub fn my_message(&self) {}
                },
                false,
                false,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(message, payable, default, selector=1)]
                    pub fn my_message(&self) {}
                },
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink(message)]
                    #[ink(payable, default, selector=1)]
                    pub fn my_message(&self) {}
                },
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink(message)]
                    #[ink(payable)]
                    #[ink(default)]
                    #[ink(selector=1)]
                    pub fn my_message(&self) {}
                },
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink(message, selector=0xA)]
                    pub fn my_message(&self) {}
                },
                false,
                true,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(message, selector=_)]
                    pub fn my_message(&self) {}
                },
                false,
                true,
                false,
            ),
        ] {
            let node = parse_first_syntax_node(code);

            let message = Message::cast(node).unwrap();

            // `payable` argument exists.
            assert_eq!(message.payable_arg().is_some(), is_payable);

            // `selector` argument exists.
            assert_eq!(message.selector_arg().is_some(), has_selector);

            // `default` argument exists.
            assert_eq!(message.default_arg().is_some(), is_default);

            // composed selector exists.
            assert!(message.composed_selector().is_some());

            // `fn` item exists.
            assert!(message.fn_item().is_some());
        }
    }
}
