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
//!                 // --snip--
//!             }
//!         "#);
//!         dbg!(&file);
//!
//!         let contracts = file.contracts();
//!         dbg!(&contracts);
//!
//!         if let Some(contract) = contracts.first() {
//!             let storage = contract.storage();
//!             dbg!(&storage);
//!         }
//!     }
//! ```

#[macro_use]
mod macros;

mod attrs;
mod chain_extension;
mod constructor;
mod contract;
mod event;
mod event_v2;
mod extension;
mod file;
mod function;
mod ink_e2e_test;
mod ink_impl;
mod ink_test;
mod message;
mod scale_derive;
mod storage;
mod storage_item;
mod topic;
mod trait_definition;

mod environment;
mod selector;

mod iter;
mod traits;
mod tree;

mod test_utils;

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
    event_v2::EventV2,
    extension::Extension,
    file::InkFile,
    function::Function,
    ink_e2e_test::InkE2ETest,
    ink_impl::InkImpl,
    ink_test::InkTest,
    message::Message,
    scale_derive::ScaleDerive,
    selector::{Selector, SelectorArg, SelectorArgKind},
    storage::Storage,
    storage_item::StorageItem,
    topic::Topic,
    trait_definition::TraitDefinition,
    traits::{
        HasInkEnvironment, HasInkImplParent, InkEntity, IsChainExtensionFn, IsInkCallable,
        IsInkEvent, IsInkFn, IsInkStruct, IsInkTrait, IsIntId, IsSyntax,
    },
    tree::ast_ext::{
        closest_ancestor_ast_type, closest_item_which, closest_non_trivia_token, parent_ast_item,
        path_from_str, path_from_type, path_to_string, resolve_current_module, resolve_item,
        resolve_qualifier, simple_use_paths_and_aliases_in_scope,
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

/// Re-export `ra_ap_syntax` as syntax.
pub use ra_ap_syntax as syntax;

/// Re-export `ra_ap_syntax::ast` as `ast`.
pub use ra_ap_syntax::ast;

/// ink! language version.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Version {
    /// version == 4.x.x
    V4,
    /// version >= 5.x.x
    V5(MinorVersion),
}

/// ink! language minor version.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MinorVersion {
    /// Latest minor version.
    Latest,
    /// version == 5.0.x
    V5_0,
}

impl Version {
    /// Returns true if `version == 4.x.x`
    pub fn is_v4(&self) -> bool {
        *self == Version::V4
    }

    /// Returns true if `version >= 5.x.x`
    pub fn is_v5(&self) -> bool {
        matches!(self, Version::V5(..))
    }

    /// Returns true if `version == 5.0.x`
    pub fn is_v5_0_x(&self) -> bool {
        *self == Version::V5(MinorVersion::V5_0)
    }

    /// Returns true if `version >= 5.1.x`
    pub fn is_gte_v5_1(&self) -> bool {
        *self == Version::V5(MinorVersion::Latest)
    }
}
