//! [ink!](https://use.ink/) intermediate representations (IRs) and abstractions
//! for [ink! analyzer](https://docs.rs/ink-analyzer/latest/ink_analyzer/).

pub use self::{
    attrs::{
        InkArgKind, InkAttrData, InkAttribute, InkAttributeKind, InkPathKind, MetaArg, MetaOption,
        MetaSeparator, MetaValue,
    },
    constructor::Constructor,
    contract::Contract,
    event::Event,
    file::InkFile,
    impl_item::Impl,
    message::Message,
    storage::Storage,
    topic::Topic,
    traits::{FromAST, FromInkAttribute, FromSyntax, IRItem},
    utils::{
        ink_ancestors, ink_attrs, ink_attrs_closest_descendants, ink_attrs_descendants,
        ink_closest_ancestors,
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
mod utils;
