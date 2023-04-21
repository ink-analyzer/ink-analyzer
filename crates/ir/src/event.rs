//! ink! contract `event` IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;

use crate::{AsInkStruct, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Topic};

/// An ink! contract `event`.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Event {
    /// ink! attribute IR data.
    #[arg_kind(Event)]
    ink_attr: InkAttrData<Struct>,
    /// List of top level ink! message items.
    #[arg_kind(Topic)]
    topics: Vec<Topic>,
}

impl AsInkStruct for Event {
    fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}

impl Event {
    /// Returns the `topic` fields for the ink! contract `event`.
    pub fn topics(&self) -> &[Topic] {
        &self.topics
    }
}
