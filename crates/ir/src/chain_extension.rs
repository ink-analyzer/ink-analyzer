//! ink! chain extension IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Trait;

use crate::{Extension, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

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

impl ChainExtension {
    /// Returns the ink! `trait` item (if any) for the ink! chain extension.
    pub fn trait_item(&self) -> Option<&Trait> {
        self.ink_attr.parent_ast()
    }

    /// Returns the ink! extensions for the ink! chain extension.
    pub fn extensions(&self) -> &[Extension] {
        &self.extensions
    }
}
