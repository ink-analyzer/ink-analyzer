//! ink! attribute IR.

use ink_analyzer_macro::FromAST;
use ra_ap_syntax::ast::{Attr, PathSegment};
use ra_ap_syntax::{AstNode, SyntaxNode};
use std::fmt;

use crate::{FromAST, InkItem};

use crate::meta::MetaName;
pub use arg::{InkArg, InkArgKind};

mod arg;
pub mod meta;
mod utils;

/// An ink! specific attribute.
#[derive(Debug, Clone, PartialEq, Eq, FromAST)]
pub struct InkAttribute {
    /// The kind of the ink! attribute e.g attribute macro like `#[ink::contract]`
    /// or attribute argument like `#[ink(storage)]`.
    kind: InkAttributeKind,
    /// ink! attribute arguments e.g message, payable, selector = 1
    /// for `#[ink(message, payable, selector = 1)]`
    args: Vec<InkArg>,
    /// AST Node for ink! attribute.
    ast: Attr,
    /// ink! path segment node.
    ink: PathSegment,
    /// ink! macro path segment node (if any) from which the attribute macro kind is derived.
    ink_macro: Option<PathSegment>,
    /// ink! argument name (if any) from which the attribute argument kind is derived.
    ink_arg_name: Option<MetaName>,
}

impl InkAttribute {
    /// Convert an AST attribute (`Attr`) into an `InkAttribute` IR type.
    pub fn cast(attr: Attr) -> Option<Self> {
        // Get attribute path segments.
        let mut path_segments = attr.path()?.segments();

        let ink_segment = path_segments.next()?;
        if ink_segment.to_string() == "ink" {
            let args = utils::parse_ink_args(&attr);
            let possible_ink_macro_segment = path_segments.next();
            let mut possible_ink_arg_name: Option<MetaName> = None;
            let ink_attr_kind = match &possible_ink_macro_segment {
                Some(ink_macro_segment) => {
                    // More than one path segment means an ink! attribute macro e.g `#[ink::contract]`.
                    if path_segments.next().is_some() {
                        // Any more path segments means an unknown attribute macro e.g `#[ink::abc::xyz]`.
                        InkAttributeKind::Macro(InkMacroKind::Unknown)
                    } else {
                        InkAttributeKind::Macro(InkMacroKind::from(
                            ink_macro_segment.to_string().as_str(),
                        ))
                    }
                }
                None => {
                    // No additional path segments means an ink! attribute argument e.g ``#[ink(storage)]`.
                    let ink_arg_kind = if args.is_empty() {
                        InkArgKind::Unknown
                    } else {
                        // Prioritize arguments so that we choose the best `InkArgKind` for the attribute.
                        // See `utils::sort_ink_args_by_kind` doc.
                        // Returns a new list so we don't change the original order for later analysis.
                        let sorted_args = utils::sort_ink_args_by_kind(&args);
                        let primary_arg = &sorted_args[0];
                        possible_ink_arg_name = primary_arg.name().map(|name| name.to_owned());
                        *primary_arg.kind()
                    };
                    InkAttributeKind::Arg(ink_arg_kind)
                }
            };

            return Some(Self {
                ast: attr,
                kind: ink_attr_kind,
                args,
                ink: ink_segment,
                ink_macro: possible_ink_macro_segment,
                ink_arg_name: possible_ink_arg_name,
            });
        }

        None
    }

    /// Returns the ink! attribute kind.
    ///
    /// Differentiates ink! attribute macros (e.g `#[ink::contract]`)
    /// from ink! attribute arguments (e.g `#[ink(storage)]`).
    pub fn kind(&self) -> &InkAttributeKind {
        &self.kind
    }

    /// Returns the ink! attribute arguments.
    pub fn args(&self) -> &[InkArg] {
        &self.args
    }

    /// Returns the ink! path segment node.
    pub fn ink(&self) -> &PathSegment {
        &self.ink
    }

    /// Returns the ink! macro path segment node (if any) from which the attribute macro kind is derived.
    pub fn ink_macro(&self) -> Option<&PathSegment> {
        self.ink_macro.as_ref()
    }

    /// Returns the ink! argument name (if any) from which the attribute argument kind is derived.
    pub fn ink_arg_name(&self) -> Option<&MetaName> {
        self.ink_arg_name.as_ref()
    }
}

/// The ink! attribute kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InkAttributeKind {
    /// ink! attribute macros e.g `#[ink::contract]`.
    Macro(InkMacroKind),
    /// ink! attributes arguments e.g `#[ink(storage)]`.
    Arg(InkArgKind),
}

/// The ink! attribute macro kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InkMacroKind {
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
    /// Unknown ink! attribute macro.
    Unknown,
}

impl From<&str> for InkMacroKind {
    /// Convert a string slice representing an attribute path segment into an ink! attribute macro kind.
    fn from(path_segment: &str) -> Self {
        match path_segment {
            // `#[ink::chain_extension]`
            "chain_extension" => InkMacroKind::ChainExtension,
            // `#[ink::contract]`
            "contract" => InkMacroKind::Contract,
            // `#[ink::storage_item]`
            "storage_item" => InkMacroKind::StorageItem,
            // `#[ink::test]`
            "test" => InkMacroKind::Test,
            // `#[ink::trait_definition]`
            "trait_definition" => InkMacroKind::TraitDefinition,
            // unknown ink! attribute path (i.e unknown ink! attribute macro).
            _ => InkMacroKind::Unknown,
        }
    }
}

impl fmt::Display for InkMacroKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // `#[ink::chain_extension]`
                InkMacroKind::ChainExtension => "chain_extension",
                // `#[ink::contract]`
                InkMacroKind::Contract => "contract",
                // `#[ink::storage_item]`
                InkMacroKind::StorageItem => "storage_item",
                // `#[ink::test]`
                InkMacroKind::Test => "test",
                // `#[ink::trait_definition]`
                InkMacroKind::TraitDefinition => "trait_definition",
                // unknown ink! attribute path (i.e unknown ink! attribute macro).
                InkMacroKind::Unknown => "unknown",
            }
        )
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
        self.ast.as_ref()
    }

    /// Returns the ink! attribute's parent `SyntaxNode`.
    pub fn parent_syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
