//! ink! event IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::{
    utils, FromInkAttribute, FromSyntax, InkArg, InkArgKind, InkAttrData, InkAttribute, InkStruct,
    Topic,
};

/// An ink! event.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Event {
    /// ink! attribute IR data.
    #[arg_kind(Event)]
    ink_attr: InkAttrData<ast::Struct>,
    /// ink! topics.
    #[arg_kind(Topic)]
    topics: Vec<Topic>,
}

impl InkStruct for Event {
    fn struct_item(&self) -> Option<&ast::Struct> {
        self.ink_attr.parent_ast()
    }
}

impl Event {
    /// Returns the ink! anonymous argument (if any) for the ink! event.
    pub fn anonymous_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Anonymous)
    }

    /// Returns the ink! topic fields for the ink! event.
    pub fn topics(&self) -> &[Topic] {
        &self.topics
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, is_anonymous, expected_n_topics) in [
            (
                quote_as_str! {
                    #[ink(event)]
                    pub struct MyEvent {}
                },
                false,
                0,
            ),
            (
                quote_as_str! {
                    #[ink(event, anonymous)]
                    pub struct MyEvent {}
                },
                true,
                0,
            ),
            (
                quote_as_str! {
                    #[ink(event)]
                    #[ink(anonymous)]
                    pub struct MyEvent {}
                },
                true,
                0,
            ),
            (
                quote_as_str! {
                    #[ink(event)]
                    pub struct MyEvent {
                        #[ink(topic)]
                        value: i32,
                    }
                },
                false,
                1,
            ),
            (
                quote_as_str! {
                    #[ink(event)]
                    pub struct MyEvent {
                        #[ink(topic)]
                        value: i32,
                        #[ink(topic)]
                        value2: bool,
                    }
                },
                false,
                2,
            ),
        ] {
            let ink_attr = parse_first_ink_attribute(code);

            let event = Event::cast(ink_attr).unwrap();

            // `anonymous` argument exists.
            assert_eq!(event.anonymous_arg().is_some(), is_anonymous);

            // Checks the expected number of topics.
            assert_eq!(event.topics().len(), expected_n_topics);

            // `struct` item exists.
            assert!(event.struct_item().is_some());
        }
    }
}
