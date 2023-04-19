//! ink! contract `constructor` IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Fn;

use crate::{AsInkFn, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! contract `constructor`.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Constructor {
    /// ink! attribute IR data.
    #[arg_kind(Constructor)]
    ink_attr: InkAttrData<Fn>,
}

impl AsInkFn for Constructor {
    fn fn_item(&self) -> Option<&Fn> {
        self.ink_attr.parent_ast()
    }
}
