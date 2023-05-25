//! ink! message IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, InkCallable, InkFn};

/// An ink! message.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Message {
    /// ink! attribute IR data.
    #[arg_kind(Message)]
    ink_attr: InkAttrData<ast::Fn>,
}

impl InkFn for Message {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ink_attr.parent_ast()
    }
}

impl InkCallable for Message {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
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
            let ink_attr = parse_first_ink_attribute(code);

            let message = Message::cast(ink_attr).unwrap();

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
