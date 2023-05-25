//! ink! IR traits.

pub use ast::{InkFn, InkImplItem, InkStruct, InkTrait};
pub use ast_ext::HasParent;
pub use callable::InkCallable;
pub use entity::InkEntity;
pub use from::{FromAST, FromInkAttribute, FromSyntax};

mod ast;
mod ast_ext;
mod callable;
mod entity;
mod from;
