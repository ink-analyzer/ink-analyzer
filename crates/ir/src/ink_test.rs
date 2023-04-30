//! ink! test IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, InkFn};

/// An ink! test.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct InkTest {
    /// ink! attribute IR data.
    #[macro_kind(Test)]
    ink_attr: InkAttrData<ast::Fn>,
}

impl InkFn for InkTest {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ink_attr.parent_ast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quote_as_str;
    use crate::test_utils::*;

    #[test]
    fn cast_works() {
        let ink_attr = parse_first_ink_attribute(quote_as_str! {
            #[ink::test]
            fn it_works() {
            }
        });

        let ink_test = InkTest::cast(ink_attr).unwrap();

        // `fn` item exists.
        assert!(ink_test.fn_item().is_some());
    }
}
