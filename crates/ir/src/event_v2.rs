//! ink! event 2.0 IR.

use ra_ap_syntax::ast;

use crate::Topic;

/// An ink! event 2.0.
///
/// Ref: <https://github.com/paritytech/ink/pull/1827>
#[ink_analyzer_macro::entity(macro_kind = Event)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EventV2 {
    // ASTNode type.
    ast: ast::Struct,
    // ink! topics.
    topics: Vec<Topic>,
}

impl_ast_type_trait!(EventV2, IsInkStruct);

impl_is_ink_event!(EventV2);

impl EventV2 {
    impl_pub_ink_arg_getter!(anonymous_arg, Anonymous, anonymous);

    impl_pub_ink_arg_getter!(signature_arg, SignatureTopic, signature_topic);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{InkEntity, IsInkStruct};
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, is_anonymous, signature, expected_n_topics) in [
            (
                quote_as_str! {
                    #[ink::event]
                    pub struct MyEvent {}
                },
                false,
                None,
                0,
            ),
            (
                quote_as_str! {
                    #[ink::event(anonymous)]
                    pub struct MyEvent {}
                },
                true,
                None,
                0,
            ),
            (
                quote_as_str! {
                    #[ink::event(signature_topic = "1111111111111111111111111111111111111111111111111111111111111111")]
                    pub struct MyEvent {}
                },
                false,
                Some("1111111111111111111111111111111111111111111111111111111111111111"),
                0,
            ),
            (
                quote_as_str! {
                    #[ink::event]
                    pub struct MyEvent {
                        #[ink(topic)]
                        value: i32,
                    }
                },
                false,
                None,
                1,
            ),
            (
                quote_as_str! {
                    #[ink::event]
                    pub struct MyEvent {
                        #[ink(topic)]
                        value: i32,
                        #[ink(topic)]
                        value2: bool,
                    }
                },
                false,
                None,
                2,
            ),
        ] {
            let node = parse_first_syntax_node(code);

            let event = EventV2::cast(node).unwrap();

            // `anonymous` argument exists.
            assert_eq!(event.anonymous_arg().is_some(), is_anonymous);

            // `signature_topic` argument exists.
            assert_eq!(event.signature_arg().is_some(), signature.is_some());

            // `signature_topic` argument value.
            assert_eq!(
                event
                    .signature_arg()
                    .and_then(|arg| arg.value()?.as_string())
                    .as_deref(),
                signature
            );

            // Checks the expected number of topics.
            assert_eq!(event.topics().len(), expected_n_topics);

            // `struct` item exists.
            assert!(event.struct_item().is_some());
        }
    }
}
