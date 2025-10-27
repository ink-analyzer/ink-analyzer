//! ink! error IR.

use ra_ap_syntax::ast;

/// An ink! error.
#[ink_analyzer_macro::entity(macro_kind = Error)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    // ASTNode type.
    ast: ast::Adt,
}

impl Error {
    impl_pub_ast_type_getter!(adt, Adt);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
    use quote::quote;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for code in [
            quote! {
                struct Error {
                }
            },
            quote! {
                enum Error {
                }
            },
            quote! {
                union Error {
                }
            },
        ] {
            let node = parse_first_syntax_node(quote_as_str! {
                #[ink::error]
                #code
            });

            let error = Error::cast(node).unwrap();

            // ADT item exists.
            assert!(error.adt().is_some());
        }
    }
}
