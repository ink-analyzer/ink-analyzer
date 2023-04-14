//! ink! attribute IR.

use ink_analyzer_macro::FromAST;
use ra_ap_syntax::ast::Attr;
use ra_ap_syntax::{AstNode, AstToken, SyntaxNode};

use crate::{FromAST, IRItem};

pub use meta::{MetaArg, MetaOption, MetaSeparator, MetaValue};

mod meta;
mod utils;

/// An ink! specific attribute.
#[derive(Debug, Clone, PartialEq, Eq, FromAST)]
pub struct InkAttribute {
    /// The kind of the ink! attribute e.g path attribute like `#[ink::contract]`
    /// or argument attribute like `#[ink(storage)]`.
    kind: InkAttributeKind,
    /// ink! attribute arguments e.g message, payable, selector = 1
    /// for `#[ink(message, payable, selector = 1)]`
    args: Vec<MetaArg>,
    /// AST Node for ink! attribute.
    ast: Attr,
}

impl InkAttribute {
    pub fn cast(attr: Attr) -> Option<Self> {
        let path_segments = utils::get_path_segments(&attr);

        let num_segments = path_segments.len();
        if num_segments > 0 && path_segments[0].to_string() == "ink" {
            let args = utils::get_args(&attr);
            let ink_attr_kind = match num_segments {
                // 1 path segment indicates an ink! argument attribute
                1 => {
                    let mut ink_arg_kind = InkArgKind::Unknown;
                    if !args.is_empty() {
                        if let MetaOption::Ok(arg_name) = &args[0].name {
                            if let Some(kind) = InkArgKind::cast(arg_name.text()) {
                                ink_arg_kind = kind;
                            }
                        }
                    }
                    InkAttributeKind::Arg(ink_arg_kind)
                }
                // 2 path segments indicates an ink! path attribute
                2 => {
                    let attr_path = &path_segments[1].to_string();
                    let ink_path_kind = match InkPathKind::cast(attr_path) {
                        Some(kind) => kind,
                        _ => InkPathKind::Unknown,
                    };
                    InkAttributeKind::Path(ink_path_kind)
                }
                // treat attributes with more than 2 path segments as unknown path
                _ => InkAttributeKind::Path(InkPathKind::Unknown),
            };

            return Some(Self {
                ast: attr,
                kind: ink_attr_kind,
                args,
            });
        }

        None
    }

    /// Returns the ink! attribute kind e.g path attribute like `#[ink::contract]`
    /// or argument attribute like `#[ink(storage)]`.
    pub fn kind(&self) -> &InkAttributeKind {
        &self.kind
    }
}

/// The kind of the ink! attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InkAttributeKind {
    /// ink! path attributes e.g `#[ink::contract]`.
    Path(InkPathKind),
    /// ink! argument attributes e.g `#[ink(storage)]`.
    Arg(InkArgKind),
}

/// An ink! path attribute kind.
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
    /// Fallback for unknown ink! path attributes (i.e unknown ink! attribute macros).
    Unknown,
}

impl InkPathKind {
    fn cast(path_segment: &str) -> Option<Self> {
        match path_segment {
            // `#[ink::chain_extension]`
            "chain_extension" => Some(InkPathKind::ChainExtension),
            // `#[ink::contract]`
            "contract" => Some(InkPathKind::Contract),
            // `#[ink::storage_item]`
            "storage_item" => Some(InkPathKind::StorageItem),
            // `#[ink::test]`
            "test" => Some(InkPathKind::Test),
            // `#[ink::trait_definition]`
            "trait_definition" => Some(InkPathKind::TraitDefinition),
            // unknown ink! attribute path (i.e unknown ink! attribute macro)
            _ => None,
        }
    }
}

/// An ink! argument attribute kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InkArgKind {
    /// `#[ink(anonymous)]`
    Anonymous,
    /// `#[ink(constructor)]`
    Constructor,
    /// `#[ink(event)]`
    Event,
    /// `#[ink(extension)]`
    Extension,
    /// `#[ink(handle_status)]`
    HandleStatus,
    /// `#[ink(impl)]`
    Impl,
    /// `#[ink(message)]`
    Message,
    /// `#[ink(namespace)]`
    Namespace,
    /// `#[ink(payable)]`
    Payable,
    /// `#[ink(selector)]`
    Selector,
    /// `#[ink(storage)]`
    Storage,
    /// `#[ink(topic)]`
    Topic,
    /// Fallback for unknown ink! attribute argument.
    Unknown,
}

impl InkArgKind {
    fn cast(arg_name: &str) -> Option<Self> {
        match arg_name {
            // `#[ink(anonymous)]`
            "anonymous" => Some(InkArgKind::Anonymous),
            // `#[ink(constructor)]`
            "constructor" => Some(InkArgKind::Constructor),
            // `#[ink(event)]`
            "event" => Some(InkArgKind::Event),
            // `#[ink(extension)]`
            "extension" => Some(InkArgKind::Extension),
            // `#[ink(handle_status)]`
            "handle_status" => Some(InkArgKind::HandleStatus),
            // `#[ink(impl)]`
            "impl" => Some(InkArgKind::Impl),
            // `#[ink(message)]`
            "message" => Some(InkArgKind::Message),
            // `#[ink(namespace)]`
            "namespace" => Some(InkArgKind::Namespace),
            // `#[ink(payable)]`
            "payable" => Some(InkArgKind::Payable),
            // `#[ink(selector)]`
            "selector" => Some(InkArgKind::Selector),
            // `#[ink(storage)]`
            "storage" => Some(InkArgKind::Storage),
            // `#[ink(topic)]`
            "topic" => Some(InkArgKind::Topic),
            // unknown ink! attribute argument.
            _ => None,
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
    pub fn attr(&self) -> &InkAttribute {
        &self.attr
    }

    pub fn parent_ast(&self) -> Option<&T> {
        Option::from(&self.ast)
    }

    pub fn parent_syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
