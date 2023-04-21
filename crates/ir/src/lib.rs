//! [ink!](https://use.ink/) intermediate representations (IRs) and abstractions
//! for [ink! analyzer](https://docs.rs/ink-analyzer/latest/ink_analyzer/).
//!
//! # Example
//! Generate an IR of ink! smart contract code.
//!
//! ```
//! use ink_analyzer_ir::{InkFile, quote_as_str};
//!
//! fn generate_ir() {
//!         let file = InkFile::parse(quote_as_str! {
//!             #[ink::contract]
//!             mod flipper {
//!
//!                 #[ink(storage)]
//!                 pub struct Flipper {
//!                     value: bool,
//!                 }
//!
//!                 #[ink(event)]
//!                 pub struct Flip {
//!                     #[ink(topic)]
//!                     flipped: bool,
//!                 }
//!
//!                 // --snip--
//!             }
//!         });
//!         dbg!(&file);
//!
//!         let contracts = file.contracts();
//!         dbg!(&contracts);
//!
//!         if let Some(contract) = contracts.first() {
//!             let events = contract.events();
//!             dbg!(&events);
//!         }
//!     }
//! ```

pub use self::{
    attrs::{meta, InkArg, InkArgKind, InkAttrData, InkAttribute, InkAttributeKind, InkMacroKind},
    constructor::Constructor,
    contract::Contract,
    event::Event,
    file::InkFile,
    impl_item::Impl,
    message::Message,
    storage::Storage,
    topic::Topic,
    traits::{AsInkFn, AsInkStruct, FromAST, FromInkAttribute, FromSyntax, IRItem},
    utils::{
        ink_attrs, ink_attrs_ancestors, ink_attrs_closest_ancestors, ink_attrs_closest_descendants,
        ink_attrs_descendants, parent_ast_item,
    },
};

// Re-export ra_ap_syntax as syntax.
pub use ra_ap_syntax as syntax;

/// Re-export ra_ap_syntax::ast as ast.
pub use ra_ap_syntax::ast;

mod attrs;
mod constructor;
mod contract;
mod event;
mod file;
mod impl_item;
mod message;
mod storage;
mod topic;

mod traits;
#[macro_use]
mod utils;
