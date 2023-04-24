//! ink! extension IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Fn;

use crate::{AsInkFn, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! extension.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Extension {
    /// ink! attribute IR data.
    #[arg_kind(Extension)]
    ink_attr: InkAttrData<Fn>,
}

impl AsInkFn for Extension {
    fn fn_item(&self) -> Option<&Fn> {
        self.ink_attr.parent_ast()
    }
}
