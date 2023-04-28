//! ink! IR traits.

use ra_ap_syntax::ast::{Item, Struct, Trait};
use ra_ap_syntax::{ast, AstNode, SyntaxKind, SyntaxNode};

use crate::{utils, InkArg, InkArgKind, InkAttribute, Selector, SelectorArg};

/// Implemented by IR items that wrap a syntax node.
pub trait FromSyntax {
    /// Returns the syntax node for the IR item.
    fn syntax(&self) -> &SyntaxNode;
}

/// Implemented by IR items that wrap an AST node.
pub trait FromAST {
    /// Associated AST node type.
    type AST: AstNode;

    /// Returns the AST node for the IR item.
    fn ast(&self) -> &Self::AST;
}

/// Blanket implementation of FromSyntax for ASTNode wrappers.
impl<T: FromAST> FromSyntax for T {
    fn syntax(&self) -> &SyntaxNode {
        self.ast().syntax()
    }
}

/// Implemented by IR items derived from an ink! attribute item.
pub trait FromInkAttribute {
    /// Returns true if the IR item can be derived for the ink! attribute.
    fn can_cast(attr: &InkAttribute) -> bool;

    /// Returns an IR item if one can be derived for the ink! attribute.
    fn cast(attr: InkAttribute) -> Option<Self>
    where
        Self: Sized;

    /// Returns the ink! attribute the IR item was derived from.
    fn ink_attr(&self) -> &InkAttribute;
}

/// Implemented by IR items whose valid AST node is a `struct` item.
pub trait InkStruct {
    /// Returns the `struct` item (if any) for the ink! entity.
    fn struct_item(&self) -> Option<&Struct>;
}

/// Implemented by IR items whose valid AST node is an `fn` item.
pub trait InkFn {
    /// Returns the `fn` item (if any) for the ink! entity.
    fn fn_item(&self) -> Option<&ast::Fn>;
}

/// Implemented by IR items whose valid AST node is a `trait` item.
pub trait InkTrait {
    /// Returns the `trait` item (if any) for the ink! entity.
    fn trait_item(&self) -> Option<&Trait>;
}

/// Implemented by IR items whose valid AST parent item node is an `impl` item.
pub trait InkImplItem {
    /// Returns the `impl` item (if any) for the ink! entity's parent item node.
    fn impl_item(&self) -> Option<ast::Impl>;
}

/// Blanket implementation of AsInkImplItem for IRItems that implement AsInkFn.
impl<T> InkImplItem for T
where
    T: InkFn,
{
    fn impl_item(&self) -> Option<ast::Impl> {
        match utils::parent_ast_item(self.fn_item()?.syntax())? {
            Item::Impl(item) => Some(item),
            _ => None,
        }
    }
}

/// Implemented by IR items that represent an ink! callable entity (i.e an ink! constructor or ink! message).
pub trait InkCallable: FromSyntax + InkFn {
    /// Returns the ink! payable argument (if any) for the ink! callable entity.
    fn payable_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Payable)
    }

    /// Returns the ink! selector argument (if any) for the ink! callable entity.
    fn selector_arg(&self) -> Option<SelectorArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Selector).and_then(SelectorArg::cast)
    }

    /// Returns the ink! default argument (if any) for the ink! callable entity.
    fn default_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Default)
    }

    /// Returns the composed selector for the ink! callable entity.
    fn composed_selector(&self) -> Option<Selector>
    where
        Self: Sized,
    {
        Selector::compose(self)
    }
}

/// Convenience methods for navigating the IR that are implemented by all IR items.
pub trait InkItem {
    /// Returns the syntax kind for the IR item.
    fn syntax_kind(&self) -> SyntaxKind;

    /// Returns the syntax tree parent for the IR item.
    fn syntax_parent(&self) -> Option<SyntaxNode>;

    /// Returns the kind of the syntax tree parent for the IR item.
    fn syntax_parent_kind(&self) -> Option<SyntaxKind>;

    /// Returns ink! attributes for the IR item.
    fn ink_attrs(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's descendants.
    fn ink_attrs_descendants(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes in the IR item's scope.
    /// This includes both the nodes own ink! attributes and those of all of it's descendants.
    fn ink_attrs_in_scope(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's descendants
    /// that don't have any ink! ancestors between them and the item.
    fn ink_attrs_closest_descendants(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's ancestors.
    fn ink_attrs_ancestors(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's ancestors
    /// that don't have any ink! ancestors between them and the item.
    fn ink_attrs_closest_ancestors(&self) -> Vec<InkAttribute>;

    /// Returns ink! arguments of the IR item.
    fn ink_args(&self) -> Vec<InkArg>;

    /// Returns ink! arguments of a specific kind (if any) for the IR item.
    fn ink_args_by_kind(&self, kind: InkArgKind) -> Vec<InkArg>;

    /// Returns ink! argument of a specific kind (if any) for the IR item.
    fn ink_arg_by_kind(&self, kind: InkArgKind) -> Option<InkArg>;
}

/// Blanket implementation of IRItem for syntax node wrappers.
impl<T> InkItem for T
where
    T: FromSyntax,
{
    fn syntax_kind(&self) -> SyntaxKind {
        self.syntax().kind()
    }

    fn syntax_parent(&self) -> Option<SyntaxNode> {
        self.syntax().parent()
    }

    fn syntax_parent_kind(&self) -> Option<SyntaxKind> {
        Some(self.syntax_parent()?.kind())
    }

    fn ink_attrs(&self) -> Vec<InkAttribute> {
        utils::ink_attrs(self.syntax())
    }

    fn ink_attrs_descendants(&self) -> Vec<InkAttribute> {
        utils::ink_attrs_descendants(self.syntax())
    }

    fn ink_attrs_in_scope(&self) -> Vec<InkAttribute> {
        utils::ink_attrs_in_scope(self.syntax())
    }

    fn ink_attrs_closest_descendants(&self) -> Vec<InkAttribute> {
        utils::ink_attrs_closest_descendants(self.syntax())
    }

    fn ink_attrs_ancestors(&self) -> Vec<InkAttribute> {
        utils::ink_attrs_ancestors(self.syntax())
    }

    fn ink_attrs_closest_ancestors(&self) -> Vec<InkAttribute> {
        utils::ink_attrs_closest_ancestors(self.syntax())
    }

    fn ink_args(&self) -> Vec<InkArg> {
        utils::ink_args(self.syntax())
    }

    fn ink_args_by_kind(&self, kind: InkArgKind) -> Vec<InkArg> {
        utils::ink_args_by_kind(self.syntax(), kind)
    }

    fn ink_arg_by_kind(&self, kind: InkArgKind) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), kind)
    }
}
