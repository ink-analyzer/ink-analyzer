//! [ink!](https://use.ink/) intermediate representations (IRs) and abstractions
//! for [ink! analyzer](https://docs.rs/ink-analyzer/latest/ink_analyzer/).
//!
//! # Example
//! Generate an IR of ink! smart contract code.
//!
//! ```
//! use ink_analyzer_ir::InkFile;
//!
//! fn generate_ir() {
//!         let file = InkFile::parse(r#"
//!             #[ink::contract]
//!             mod my_contract {
//!
//!                 #[ink(storage)]
//!                 pub struct MyContract {
//!                     value: bool,
//!                 }
//!
//!                 #[ink(event)]
//!                 pub struct MyEvent {
//!                     #[ink(topic)]
//!                     value: bool,
//!                 }
//!
//!                 // --snip--
//!             }
//!         "#);
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
    attrs::{
        meta, InkArg, InkArgKind, InkArgValueKind, InkArgValuePathKind, InkArgValueStringKind,
        InkAttribute, InkAttributeKind, InkMacroKind,
    },
    chain_extension::ChainExtension,
    constructor::Constructor,
    contract::Contract,
    environment::{EnvArg, Environment},
    event::Event,
    extension::Extension,
    file::InkFile,
    ink_e2e_test::InkE2ETest,
    ink_impl::InkImpl,
    ink_test::InkTest,
    message::Message,
    selector::{Selector, SelectorArg, SelectorArgKind},
    storage::Storage,
    storage_item::StorageItem,
    topic::Topic,
    trait_definition::TraitDefinition,
    traits::{
        HasInkEnvironment, HasInkImplParent, InkEntity, IsInkCallable, IsInkFn, IsInkStruct,
        IsInkTrait, IsSyntax,
    },
    tree::ast_ext::{
        closest_ancestor_ast_type, closest_item_which, closest_non_trivia_token, parent_ast_item,
    },
    tree::utils::{
        attrs, ink_ancestors, ink_arg_by_kind, ink_args, ink_args_by_kind, ink_attr_to_entity,
        ink_attrs, ink_attrs_ancestors, ink_attrs_closest_ancestors, ink_attrs_closest_descendants,
        ink_attrs_descendants, ink_attrs_in_scope, ink_callable_closest_descendants,
        ink_closest_ancestors, ink_closest_descendants, ink_descendants,
        ink_impl_closest_descendants, ink_parent, ink_peekable_quasi_closest_descendants,
    },
    tree::{InkTree, ItemAtOffset},
};

// Re-export ra_ap_syntax as syntax.
pub use ra_ap_syntax as syntax;

/// Re-export `ra_ap_syntax::ast` as `ast`.
pub use ra_ap_syntax::ast;

#[macro_use]
mod macros;

mod attrs;
mod chain_extension;
mod constructor;
mod contract;
mod event;
mod extension;
mod file;
mod ink_e2e_test;
mod ink_impl;
mod ink_test;
mod message;
mod storage;
mod storage_item;
mod topic;
mod trait_definition;

mod environment;
mod iter;
mod selector;
mod traits;
mod tree;

mod test_utils;
