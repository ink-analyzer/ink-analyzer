//! ink! constructor IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::traits::{FromInkAttribute, FromSyntax, IsInkCallable, IsInkFn};
use crate::{InkAttrData, InkAttribute};

/// An ink! constructor.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Constructor {
    /// ink! attribute IR data.
    #[arg_kind(Constructor)]
    ink_attr: InkAttrData<ast::Fn>,
}

impl IsInkFn for Constructor {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ink_attr.parent_ast()
    }
}

impl IsInkCallable for Constructor {}

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
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {}
                },
                false,
                false,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(constructor, payable, default, selector=1)]
                    pub fn my_constructor() -> Self {}
                },
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink(constructor)]
                    #[ink(payable, default, selector=1)]
                    pub fn my_constructor() -> Self {}
                },
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
                    pub fn my_constructor() -> Self {}
                },
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
            ),
            (
                quote_as_str! {
                    #[ink(constructor, selector=_)]
                    pub fn my_constructor() -> Self {}
                },
                false,
                true,
                false,
            ),
        ] {
            let ink_attr = parse_first_ink_attribute(code);

            let constructor = Constructor::cast(ink_attr).unwrap();

            // `payable` argument exists.
            assert_eq!(constructor.payable_arg().is_some(), is_payable);

            // `selector` argument exists.
            assert_eq!(constructor.selector_arg().is_some(), has_selector);

            // `default` argument exists.
            assert_eq!(constructor.default_arg().is_some(), is_default);

            // composed selector exists.
            assert!(constructor.composed_selector().is_some());

            // `fn` item exists.
            assert!(constructor.fn_item().is_some());
        }
    }
}
