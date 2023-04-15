//! ink! contract IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Module;
use ra_ap_syntax::SyntaxNode;

use crate::{
    Constructor, Event, FromInkAttribute, FromSyntax, Impl, InkAttrData, InkAttribute, Message,
    Storage,
};

/// An ink! contract.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Contract {
    /// ink! attribute IR data.
    #[path_kind(Contract)]
    ink_attr: InkAttrData<Module>,
    /// List of top level ink! storage items.
    #[arg_kind(Storage)]
    storage: Vec<Storage>,
    /// List of top level ink! event items.
    #[arg_kind(Event)]
    events: Vec<Event>,
    /// List of top level ink! impl items.
    #[arg_kind(Impl)]
    impls: Vec<Impl>,
    /// List of top level ink! constructor items.
    #[arg_kind(Constructor)]
    constructors: Vec<Constructor>,
    /// List of top level ink! message items.
    #[arg_kind(Message)]
    messages: Vec<Message>,
}

impl Contract {
    /// Returns the module item (if any) for the ink! contract.
    pub fn module(&self) -> Option<&Module> {
        self.ink_attr.parent_ast()
    }
}
