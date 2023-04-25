//! ink! chain extension IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::{AssocItem, HasName, Trait, TypeAlias};

use crate::{AsInkTrait, Extension, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! chain extension.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct ChainExtension {
    /// ink! attribute IR data.
    #[macro_kind(ChainExtension)]
    ink_attr: InkAttrData<Trait>,
    /// ink! extensions.
    #[arg_kind(Extension)]
    extensions: Vec<Extension>,
}

impl AsInkTrait for ChainExtension {
    fn trait_item(&self) -> Option<&Trait> {
        self.ink_attr.parent_ast()
    }
}

impl ChainExtension {
    /// Returns the ink! extensions for the ink! chain extension.
    pub fn extensions(&self) -> &[Extension] {
        &self.extensions
    }

    /// Returns the ink! extensions for the ink! chain extension.
    pub fn error_codes(&self) -> Vec<TypeAlias> {
        if let Some(trait_item) = self.trait_item() {
            if let Some(assoc_item_list) = trait_item.assoc_item_list() {
                return assoc_item_list
                    .assoc_items()
                    .filter_map(|assoc_item| {
                        if let AssocItem::TypeAlias(type_alias) = assoc_item {
                            if let Some(name) = type_alias.name() {
                                return (name.to_string() == "ErrorCode").then_some(type_alias);
                            }
                        }
                        None
                    })
                    .collect();
            }
        }
        Vec::new()
    }
}
