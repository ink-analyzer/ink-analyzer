//! ink! contract constructor IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Fn;
use ra_ap_syntax::SyntaxNode;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! contract constructor.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Constructor {
    /// ink! attribute IR data.
    #[arg_kind(Constructor)]
    ink_attr: InkAttrData<Fn>,
}

impl Constructor {
    /// Returns the fn item (if any) for the ink! contract constructor.
    pub fn fn_item(&self) -> Option<&Fn> {
        self.ink_attr.parent_ast()
    }
}
