//! ink! constructor IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::{AsInkFn, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! constructor.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Constructor {
    /// ink! attribute IR data.
    #[arg_kind(Constructor)]
    ink_attr: InkAttrData<ast::Fn>,
}

impl AsInkFn for Constructor {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ink_attr.parent_ast()
    }
}
