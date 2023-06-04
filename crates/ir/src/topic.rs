//! ink! topic IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::traits::{FromInkAttribute, FromSyntax};
use crate::{InkAttrData, InkAttribute};

/// An ink! topic.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Topic {
    /// ink! attribute IR data.
    #[arg_kind(Topic)]
    ink_attr: InkAttrData<ast::RecordField>,
}

impl Topic {
    /// Returns the `field` item (if any) for the ink! topic.
    pub fn field(&self) -> Option<&ast::RecordField> {
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
            pub struct MyEvent {
                #[ink(topic)]
                value: i32,
            }
        });

        let topic = Topic::cast(ink_attr).unwrap();

        // `field` item exists.
        assert!(topic.field().is_some());
    }
}
