//! ink! contract IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::{AstNode, Module};
use ra_ap_syntax::SyntaxNode;

use crate::{FromInkAttribute, FromSyntax, IRItem, InkAttribute, InkAttributeKind, InkPathKind};

/// An ink! contract.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Contract {
    /// ink! contract attributes.
    ink_attr: InkAttribute,
    /// Annotated module (if any).
    ast: Option<Module>,
    /// Syntax node for ink! contract.
    syntax: SyntaxNode,
}

impl Contract {
    pub fn cast(attr: InkAttribute) -> Option<Self> {
        if let InkAttributeKind::Path(kind) = attr.kind() {
            return matches!(kind, InkPathKind::Contract).then_some(Self {
                ast: attr.syntax_parent().and_then(Module::cast),
                syntax: attr.syntax_parent()?,
                ink_attr: attr,
            });
        }
        None
    }

    pub fn module(&self) -> Option<&Module> {
        Option::from(&self.ast)
    }
}
