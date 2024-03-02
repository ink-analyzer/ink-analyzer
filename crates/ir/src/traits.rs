//! ink! IR traits.

mod ast_ext;
mod ast_type;
mod entity;
mod shared;

pub use ast_ext::IsSyntax;
pub use ast_type::{HasInkImplParent, IsInkFn, IsInkStruct, IsInkTrait};
pub use entity::InkEntity;
pub use shared::{HasInkEnvironment, IsChainExtensionFn, IsInkCallable, IsInkEvent, IsIntId};
