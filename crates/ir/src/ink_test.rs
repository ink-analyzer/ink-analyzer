//! ink! test IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, InkFn};

/// An ink! test.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct InkTest {
    /// ink! attribute IR data.
    #[macro_kind(Test)]
    ink_attr: InkAttrData<ast::Fn>,
}

impl InkFn for InkTest {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ink_attr.parent_ast()
    }
}
