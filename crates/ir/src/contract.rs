//! ink! contract IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Module;
use ra_ap_syntax::SyntaxNode;

use crate::{
    FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, InkAttributeKind, InkPathKind,
};

/// An ink! contract.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Contract {
    /// ink! attribute IR data.
    ink_attr: InkAttrData<Module>,
}

impl Contract {
    pub fn cast(attr: InkAttribute) -> Option<Self> {
        if let InkAttributeKind::Path(InkPathKind::Contract) = attr.kind() {
            return Some(Self {
                ink_attr: InkAttrData::from(attr),
            });
        }
        None
    }

    pub fn module(&self) -> Option<&Module> {
        self.ink_attr.parent_ast()
    }
}
