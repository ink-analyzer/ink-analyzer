//! ink! impl IR.

use ink_analyzer_macro::FromSyntax;
use ra_ap_syntax::{ast, AstNode, SyntaxNode};

use crate::{
    utils, Constructor, FromSyntax, InkArg, InkArgKind, InkAttribute, InkAttributeKind, Message,
};

/// An ink! impl block.
#[derive(Debug, Clone, PartialEq, Eq, FromSyntax)]
pub struct InkImpl {
    /// ink! constructors.
    constructors: Vec<Constructor>,
    /// ink! messages.
    messages: Vec<Message>,
    /// Syntax node for ink! impl.
    syntax: SyntaxNode,
}

impl InkImpl {
    /// Returns true if the syntax node can be converted into an ink! impl item.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L118-L216>.
    pub fn can_cast(node: &SyntaxNode) -> bool {
        // Has ink! impl attribute.
        utils::ink_attrs(node)
            .iter()
            .any(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Impl))
            // Is an `impl` item and has any ink! constructor or ink! message annotated descendants.
            || (ast::Impl::can_cast(node.kind())
                && utils::ink_attrs_closest_descendants(node)
                    .iter()
                    .any(|attr| {
                        matches!(
                            attr.kind(),
                            InkAttributeKind::Arg(InkArgKind::Constructor)
                                | InkAttributeKind::Arg(InkArgKind::Message)
                        )
                    }))
    }

    /// Converts a syntax node into an ink! impl item (if possible).
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(&node).then_some(Self {
            constructors: utils::ink_closest_descendants(&node),
            messages: utils::ink_closest_descendants(&node),
            syntax: node,
        })
    }

    /// Returns the `impl` item (if any) for the ink! impl.
    pub fn impl_item(&self) -> Option<ast::Impl> {
        ast::Impl::cast(self.syntax.clone())
    }

    /// Returns the ink! impl attribute (if any).
    pub fn impl_attr(&self) -> Option<InkAttribute> {
        utils::ink_attrs(&self.syntax)
            .into_iter()
            .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Impl))
    }

    /// Returns the ink! impl namespace argument (if any).
    pub fn namespace_arg(&self) -> Option<InkArg> {
        utils::ink_attrs(&self.syntax)
            .iter()
            .find_map(|attr| {
                attr.args()
                    .iter()
                    .find(|arg| *arg.kind() == InkArgKind::Namespace)
            })
            .cloned()
    }

    /// Returns the ink! constructors for the ink! impl.
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    /// Returns the ink! messages for the ink! impl.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }
}
