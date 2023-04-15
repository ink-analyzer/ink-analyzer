//! ink! contract storage IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;
use ra_ap_syntax::SyntaxNode;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! contract's storage.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Storage {
    /// ink! attribute IR data.
    #[arg_kind(Storage)]
    ink_attr: InkAttrData<Struct>,
}

impl Storage {
    /// Returns the struct item (if any) for the ink! contract storage item.
    pub fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}
