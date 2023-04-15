//! ink! contract impl IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Impl as ASTImpl;
use ra_ap_syntax::SyntaxNode;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! contract implementation.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Impl {
    /// ink! attribute IR data.
    #[arg_kind(Impl)]
    ink_attr: InkAttrData<ASTImpl>,
}

impl Impl {
    /// Returns the impl item (if any) for the ink! contract implementation.
    pub fn impl_item(&self) -> Option<&ASTImpl> {
        self.ink_attr.parent_ast()
    }
}
