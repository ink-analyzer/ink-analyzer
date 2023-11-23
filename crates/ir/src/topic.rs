//! ink! topic IR.

use ra_ap_syntax::ast;

/// An ink! topic.
#[ink_analyzer_macro::entity(arg_kind = Topic)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Topic {
    // ASTNode type.
    ast: ast::RecordField,
}

impl Topic {
    impl_pub_ast_type_getter!(field, RecordField);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
    use ra_ap_syntax::AstNode;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node: ast::RecordField = parse_first_ast_node_of_type(quote_as_str! {
            pub struct MyEvent {
                #[ink(topic)]
                value: i32,
            }
        });

        let topic = Topic::cast(node.syntax().clone()).unwrap();

        // `field` item exists.
        assert!(topic.field().is_some());
    }
}
