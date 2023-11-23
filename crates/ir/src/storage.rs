//! ink! storage IR.

use ra_ap_syntax::ast;

/// An ink! storage definition.
#[ink_analyzer_macro::entity(arg_kind = Storage)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Storage {
    // ASTNode type.
    ast: ast::Struct,
}

impl_ast_type_trait!(Storage, IsInkStruct);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{InkEntity, IsInkStruct};
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node = parse_first_syntax_node(quote_as_str! {
            #[ink(storage)]
            pub struct MyContract {}
        });

        let storage = Storage::cast(node).unwrap();

        // `struct` item exists.
        assert!(storage.struct_item().is_some());
    }
}
