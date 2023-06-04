//! ink! IR traits.

pub use ast_ext::HasParent;
pub use ast_type::{IsInkFn, IsInkImplItem, IsInkStruct, IsInkTrait};
pub use callable::IsInkCallable;
pub use entity::IsInkEntity;
pub use from::{FromAST, FromInkAttribute, FromSyntax};

mod ast_ext;
mod ast_type;
mod callable;
mod entity;
mod from;
