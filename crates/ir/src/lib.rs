//! [ink!](https://use.ink/) intermediate representations (IRs) and abstractions
//! for [ink! analyzer](https://docs.rs/ink-analyzer/latest/ink_analyzer/).

pub use self::{
    attrs::{
        InkArgKind, InkAttribute, InkAttributeKind, InkPathKind, MetaArg, MetaOption,
        MetaSeparator, MetaValue,
    },
    contract::Contract,
    file::InkFile,
    traits::{FromAST, FromInkAttribute, FromSyntax, IRItem},
};

// Re-export ra_ap_syntax as syntax.
pub use ra_ap_syntax as syntax;

/// Re-export ra_ap_syntax::ast as ast.
pub use ra_ap_syntax::ast;

mod attrs;
mod contract;
mod file;

mod traits;
mod utils;
