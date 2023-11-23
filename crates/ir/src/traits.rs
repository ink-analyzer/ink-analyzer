//! ink! IR traits.

pub use ast_ext::IsSyntax;
pub use ast_type::{HasInkImplParent, IsInkFn, IsInkStruct, IsInkTrait};
pub use callable::IsInkCallable;
pub use entity::InkEntity;

mod ast_ext;
mod ast_type;
mod callable;
mod entity;
