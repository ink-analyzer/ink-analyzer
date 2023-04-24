//! ink! test IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Fn;

use crate::{AsInkFn, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! test.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct InkTest {
    /// ink! attribute IR data.
    #[macro_kind(Test)]
    ink_attr: InkAttrData<Fn>,
}

impl AsInkFn for InkTest {
    fn fn_item(&self) -> Option<&Fn> {
        self.ink_attr.parent_ast()
    }
}
