//! ink! entity traits for accessing the wrapped AST type.

use ra_ap_syntax::{ast, AstNode};

use crate::tree::ast_ext;
use crate::InkEntity;

/// Implemented by ink! entities whose valid AST node is a `struct` item.
pub trait IsInkStruct: InkEntity {
    /// Returns the `struct` item (if any).
    fn struct_item(&self) -> Option<&ast::Struct>;
}

/// Implemented by ink! entities whose valid AST node is an `fn` item.
pub trait IsInkFn: InkEntity {
    /// Returns the `fn` item (if any).
    fn fn_item(&self) -> Option<&ast::Fn>;
}

/// Implemented by ink! entities whose valid AST node is a `trait` item.
pub trait IsInkTrait: InkEntity {
    /// Returns the `trait` item (if any).
    fn trait_item(&self) -> Option<&ast::Trait>;
}

/// Implemented by ink! entities whose valid AST parent item node is an `impl` item.
pub trait HasInkImplParent: InkEntity {
    /// Returns the `impl` item (if any) for the ink! entity's parent item node.
    fn parent_impl_item(&self) -> Option<ast::Impl>;
}

/// Blanket implementation of `InkImplItem` for ink! entities that implement `InkFn`.
impl<T> HasInkImplParent for T
where
    T: IsInkFn,
{
    fn parent_impl_item(&self) -> Option<ast::Impl> {
        match ast_ext::parent_ast_item(self.fn_item()?.syntax())? {
            ast::Item::Impl(item) => Some(item),
            _ => None,
        }
    }
}
