//! ink! chain extension IR.

use ra_ap_syntax::ast;
use ra_ap_syntax::ast::HasName;

use crate::traits::IsInkTrait;
use crate::{Extension, Function};

/// An ink! chain extension.
#[ink_analyzer_macro::entity(macro_kind = ChainExtension)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChainExtension {
    // ASTNode type.
    ast: ast::Trait,
    // ink! extensions.
    extensions: Vec<Extension>,
    // ink! v5 functions.
    functions: Vec<Function>,
}

impl_ast_type_trait!(ChainExtension, IsInkTrait);

impl ChainExtension {
    /// Returns the ink! v5 extension id (if any).
    pub fn id(&self) -> Option<u32> {
        self.extension_arg()?.value()?.as_u32()
    }

    impl_pub_ink_arg_getter!(extension_arg, Extension, extension);

    /// Returns the `ErrorCode` associated types for the ink! chain extension.
    pub fn error_code(&self) -> Option<ast::TypeAlias> {
        self.trait_item()?
            .assoc_item_list()
            .map(|assoc_item_list| {
                assoc_item_list
                    .assoc_items()
                    .find_map(|assoc_item| match assoc_item {
                        ast::AssocItem::TypeAlias(type_alias) => {
                            let name = type_alias.name()?;
                            (name.to_string() == "ErrorCode").then_some(type_alias)
                        }
                        _ => None,
                    })
            })?
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node = parse_first_syntax_node(quote_as_str! {
            #[ink::chain_extension]
            pub trait MyChainExtension {
                type ErrorCode = ();

                #[ink(extension=1)]
                fn my_extension();

                #[ink(extension=2)]
                fn my_extension2();
            }
        });

        let chain_extension = ChainExtension::cast(node).unwrap();

        // 1 error code.
        assert!(chain_extension.error_code().is_some());

        // 2 extensions.
        assert_eq!(chain_extension.extensions().len(), 2);

        // `trait` item exists.
        assert!(chain_extension.trait_item().is_some());
    }

    #[test]
    fn cast_v5_works() {
        let node = parse_first_syntax_node(quote_as_str! {
            #[ink::chain_extension(extension=1)]
            pub trait MyChainExtension {
                type ErrorCode = ();

                #[ink(function=1)]
                fn my_function();

                #[ink(function=2)]
                fn my_function2();
            }
        });

        let chain_extension = ChainExtension::cast(node).unwrap();

        // 1 error code.
        assert!(chain_extension.error_code().is_some());

        // `extension` argument exists.
        assert!(chain_extension.extension_arg().is_some());

        // ink! v5 extension id is set to 1.
        assert_eq!(chain_extension.id(), Some(1));

        // 2 functions.
        assert_eq!(chain_extension.functions().len(), 2);

        // `trait` item exists.
        assert!(chain_extension.trait_item().is_some());
    }
}
