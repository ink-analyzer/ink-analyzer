//! ink! extension IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::{
    utils, FromInkAttribute, FromSyntax, InkArg, InkArgKind, InkAttrData, InkAttribute, InkFn,
};

/// An ink! extension.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Extension {
    /// ink! attribute IR data.
    #[arg_kind(Extension)]
    ink_attr: InkAttrData<ast::Fn>,
}

impl InkFn for Extension {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ink_attr.parent_ast()
    }
}

impl Extension {
    /// Returns the ink! extension argument (if any) for the ink! extension.
    pub fn extension_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Extension)
    }

    /// Returns the ink! handle_status argument (if any) for the ink! extension.
    pub fn handle_status_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::HandleStatus)
    }
}
