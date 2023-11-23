//! ink! event IR.

use ra_ap_syntax::ast;

use crate::Topic;

/// An ink! event.
#[ink_analyzer_macro::entity(arg_kind = Event)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Event {
    // ASTNode type.
    ast: ast::Struct,
    // ink! topics.
    topics: Vec<Topic>,
}

impl_ast_type_trait!(Event, IsInkStruct);

impl Event {
    impl_pub_ink_arg_getter!(anonymous_arg, Anonymous, anonymous);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{InkEntity, IsInkStruct};
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
            let node = parse_first_syntax_node(code);

            let event = Event::cast(node).unwrap();

            // `anonymous` argument exists.
            assert_eq!(event.anonymous_arg().is_some(), is_anonymous);

            // Checks the expected number of topics.
            assert_eq!(event.topics().len(), expected_n_topics);

            // `struct` item exists.
            assert!(event.struct_item().is_some());
        }
    }
}
