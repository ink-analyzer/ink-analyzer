//! ink! IR traits.

pub use ast::{InkFn, InkImplItem, InkStruct, InkTrait};
pub use callable::InkCallable;
pub use entity::InkEntity;
pub use from::{FromAST, FromInkAttribute, FromSyntax};

mod ast;
mod callable;
mod entity;
mod from;
