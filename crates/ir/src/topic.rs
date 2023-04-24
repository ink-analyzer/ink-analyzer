//! ink! topic IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::RecordField;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! topic.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Topic {
    /// ink! attribute IR data.
    #[arg_kind(Topic)]
    ink_attr: InkAttrData<RecordField>,
}

impl Topic {
    /// Returns the `field` item (if any) for the ink! topic.
    pub fn field(&self) -> Option<&RecordField> {
        self.ink_attr.parent_ast()
    }
}
