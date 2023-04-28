//! ink! event IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;

use crate::{
    utils, FromInkAttribute, FromSyntax, InkArg, InkArgKind, InkAttrData, InkAttribute, InkStruct,
    Topic,
};

/// An ink! event.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Event {
    /// ink! attribute IR data.
    #[arg_kind(Event)]
    ink_attr: InkAttrData<Struct>,
    /// ink! topics.
    #[arg_kind(Topic)]
    topics: Vec<Topic>,
}

impl InkStruct for Event {
    fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}

impl Event {
    /// Returns the ink! anonymous argument (if any) for the ink! event.
    pub fn anonymous_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Anonymous)
    }

    /// Returns the ink! topic fields for the ink! event.
    pub fn topics(&self) -> &[Topic] {
        &self.topics
    }
}
