//! ink! contract IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Module;

use crate::{
    Constructor, Event, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, InkImpl, InkTest,
    Message, Storage,
};

/// An ink! contract.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Contract {
    /// ink! attribute IR data.
    #[macro_kind(Contract)]
    ink_attr: InkAttrData<Module>,
    /// ink! storage items.
    #[arg_kind(Storage)]
    storage: Vec<Storage>,
    /// ink! events.
    #[arg_kind(Event)]
    events: Vec<Event>,
    /// ink! impl items.
    #[arg_kind(Impl)]
    impls: Vec<InkImpl>,
    /// ink! constructors.
    #[arg_kind(Constructor)]
    constructors: Vec<Constructor>,
    /// ink! messages.
    #[arg_kind(Message)]
    messages: Vec<Message>,
    /// ink! tests.
    #[macro_kind(Test)]
    tests: Vec<InkTest>,
}

impl Contract {
    /// Returns the `mod` item (if any) for the ink! contract.
    pub fn module(&self) -> Option<&Module> {
        self.ink_attr.parent_ast()
    }

    /// Returns the ink! storage items for the ink! contract.
    pub fn storage(&self) -> &[Storage] {
        &self.storage
    }

    /// Returns the ink! events for the ink! contract.
    pub fn events(&self) -> &[Event] {
        &self.events
    }

    /// Returns the ink! impl blocks for the ink! contract.
    pub fn impls(&self) -> &[InkImpl] {
        &self.impls
    }

    /// Returns the ink! constructors for the ink! contract.
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    /// Returns the ink! messages for the ink! contract.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }

    /// Returns the ink! tests for the ink! contract.
    pub fn tests(&self) -> &[InkTest] {
        &self.tests
    }
}
