//! ink! chain extension IR.

use ra_ap_syntax::ast;
use ra_ap_syntax::ast::HasName;

use crate::traits::IsInkTrait;
use crate::Extension;

/// An ink! chain extension.
#[ink_analyzer_macro::entity(macro_kind = ChainExtension)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChainExtension {
    // ASTNode type.
    ast: ast::Trait,
    // ink! extensions.
    extensions: Vec<Extension>,
}

impl IsInkTrait for ChainExtension {
    fn trait_item(&self) -> Option<&ast::Trait> {
        self.ast.as_ref()
    }
}

impl ChainExtension {
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
}
