//! ink! IR traits.

pub use ast_ext::IsSyntax;
pub use ast_type::{IsInkFn, IsInkImplItem, IsInkStruct, IsInkTrait};
pub use callable::IsInkCallable;
pub use entity::InkEntity;

mod ast_ext;
mod ast_type;
mod callable;
mod entity;
