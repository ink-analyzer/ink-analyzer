//! ink! contract event IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;
use ra_ap_syntax::SyntaxNode;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Topic};

/// An ink! contract event.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Event {
    /// ink! attribute IR data.
    #[arg_kind(Event)]
    ink_attr: InkAttrData<Struct>,
    /// List of top level ink! message items.
    #[arg_kind(Topic)]
    topics: Vec<Topic>,
}

impl Event {
    /// Returns the struct item (if any) for the ink! contract event.
    pub fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}
