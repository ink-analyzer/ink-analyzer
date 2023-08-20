//! ink! e2e test IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::traits::{FromInkAttribute, FromSyntax, IsInkFn};
use crate::{InkAttrData, InkAttribute};

/// An ink! e2e test.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct InkE2ETest {
    /// ink! attribute IR data.
    #[macro_kind(E2ETest)]
    ink_attr: InkAttrData<ast::Fn>,
}

impl IsInkFn for InkE2ETest {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ink_attr.parent_ast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let ink_attr = parse_first_ink_attribute(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
            }
        });

        let ink_e2e_test = InkE2ETest::cast(ink_attr).unwrap();

        // `fn` item exists.
        assert!(ink_e2e_test.fn_item().is_some());
    }
}
