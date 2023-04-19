//! ink! attribute IR.

use ink_analyzer_macro::FromAST;
use ra_ap_syntax::ast::Attr;
use ra_ap_syntax::{AstNode, SyntaxNode};

use crate::{FromAST, IRItem};

pub use arg::{InkArg, InkArgKind};

mod arg;
pub mod meta;
mod utils;

/// An ink! specific attribute.
#[derive(Debug, Clone, PartialEq, Eq, FromAST)]
pub struct InkAttribute {
    /// The kind of the ink! attribute e.g path attribute like `#[ink::contract]`
    /// or argument attribute like `#[ink(storage)]`.
    kind: InkAttributeKind,
    /// ink! attribute arguments e.g message, payable, selector = 1
    /// for `#[ink(message, payable, selector = 1)]`
    args: Vec<InkArg>,
    /// AST Node for ink! attribute.
    ast: Attr,
}

impl InkAttribute {
    /// Convert an AST attribute (`Attr`) into an `InkAttribute` IR type.
    pub fn cast(attr: Attr) -> Option<Self> {
        // Get attribute path segments.
        let mut path_segments = attr.path()?.segments();

        let first_segment = path_segments.next()?;
        if first_segment.to_string() == "ink" {
            let args = utils::parse_ink_args(&attr);
            let ink_attr_kind = if let Some(second_segment) = path_segments.next() {
                // More than one path segment means an ink! path-based attribute e.g `#[ink::contract]`.
                if path_segments.next().is_some() {
                    // Any more path segments means an unknown path/macro e.g `#[ink::abc::xyz]`.
                    InkAttributeKind::Path(InkPathKind::Unknown)
                } else {
                    InkAttributeKind::Path(InkPathKind::from(&second_segment.to_string()))
                }
            } else {
                // No additional path segments means an ink! argument-based attribute e.g ``#[ink(storage)]`.
                let ink_arg_kind = if args.is_empty() {
                    InkArgKind::Unknown
                } else {
                    // Prioritize arguments so that we choose the best `InkArgKind` for the attribute.
                    // See `utils::sort_ink_args_by_kind` doc.
                    // Returns a new list so we don't change the original order for later analysis.
                    let sorted_args = utils::sort_ink_args_by_kind(&args);
                    *sorted_args[0].kind()
                };
                InkAttributeKind::Arg(ink_arg_kind)
            };

            return Some(Self {
                ast: attr,
                kind: ink_attr_kind,
                args,
            });
        }

        None
    }

    /// Returns the ink! attribute kind.
    ///
    /// Differentiates path-based attributes (e.g `#[ink::contract]`)
    /// from argument/meta-based attributes (e.g `#[ink(storage)]`).
    pub fn kind(&self) -> &InkAttributeKind {
        &self.kind
    }

    /// Returns the ink! attribute arguments.
    pub fn args(&self) -> &Vec<InkArg> {
        &self.args
    }
}

/// The kind of the ink! attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InkAttributeKind {
    /// ink! path-based attributes e.g `#[ink::contract]`.
    Path(InkPathKind),
    /// ink! argument/meta-based attributes e.g `#[ink(storage)]`.
    Arg(InkArgKind),
}

/// An ink! attribute path kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InkPathKind {
    /// `#[ink::chain_extension]`
    ChainExtension,
    /// `#[ink::contract]`
    Contract,
    /// `#[ink::storage_item]`
    StorageItem,
    /// `#[ink::test]`
    Test,
    /// `#[ink::trait_definition]`
    TraitDefinition,
    /// Unknown ink! attribute path (i.e unknown ink! attribute macro).
    Unknown,
}

impl InkPathKind {
    /// Convert a string slice representing an attribute path segment into an ink! attribute path kind.
    pub fn from(path_segment: &str) -> Self {
        match path_segment {
            // `#[ink::chain_extension]`
            "chain_extension" => InkPathKind::ChainExtension,
            // `#[ink::contract]`
            "contract" => InkPathKind::Contract,
            // `#[ink::storage_item]`
            "storage_item" => InkPathKind::StorageItem,
            // `#[ink::test]`
            "test" => InkPathKind::Test,
            // `#[ink::trait_definition]`
            "trait_definition" => InkPathKind::TraitDefinition,
            // unknown ink! attribute path (i.e unknown ink! attribute macro).
            _ => InkPathKind::Unknown,
        }
    }
}

/// Standard data for an IR item derived from an ink! attribute.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InkAttrData<T: AstNode> {
    /// ink! contract attributes.
    attr: InkAttribute,
    /// Annotated module (if any).
    ast: Option<T>,
    /// Syntax node for ink! contract.
    syntax: SyntaxNode,
}

impl<T: AstNode> From<InkAttribute> for InkAttrData<T> {
    fn from(attr: InkAttribute) -> Self {
        Self {
            ast: attr.syntax_parent().and_then(T::cast),
            syntax: attr
                .syntax_parent()
                .expect("An attribute should always have a parent."),
            attr,
        }
    }
}

impl<T: AstNode> InkAttrData<T> {
    /// Returns the ink! attribute.
    pub fn attr(&self) -> &InkAttribute {
        &self.attr
    }

    /// Returns the ink! attribute's parent `ASTNode`.
    pub fn parent_ast(&self) -> Option<&T> {
        Option::from(&self.ast)
    }

    /// Returns the ink! attribute's parent `SyntaxNode`.
    pub fn parent_syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
