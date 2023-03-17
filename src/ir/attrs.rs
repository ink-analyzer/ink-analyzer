//! ink! attribute intermediate representations (IRs) and abstractions.
use ra_ap_syntax::ast::{Attr, PathSegment};
use ra_ap_syntax::{AstNode, SyntaxKind};

/// Either an ink! specific attribute, or another uninterpreted attribute.
#[derive(Debug, PartialEq, Eq)]
pub enum Attribute {
    /// An ink! specific attribute e.g. `#[ink::contract]` or `#[ink(storage)]`.
    Ink(InkAttribute),
    /// Any other non-ink! attribute.
    Other(Attr),
}

impl From<Attr> for Attribute {
    fn from(attr: Attr) -> Self {
        if let Some(meta) = attr.meta() {
            let mut path_segments: Vec<String> = Vec::new();
            let mut args: Vec<String> = Vec::new();

            if let Some(path) = meta.path() {
                path_segments = path
                    .syntax()
                    .descendants()
                    .filter_map(PathSegment::cast)
                    .map(|item| item.syntax().text().to_string())
                    .collect();
            }

            if let Some(token_tree) = meta.token_tree() {
                for item in token_tree.syntax().descendants_with_tokens() {
                    if let Some(token) = item.as_token() {
                        if token.kind() == SyntaxKind::IDENT {
                            args.push(token.text().to_string());
                            // TODO: Parse argument values
                        }
                    }
                }
            }

            let num_segments = path_segments.len();
            if num_segments > 0 && path_segments[0] == "ink" {
                let ink_attr_kind = match num_segments {
                    // 1 path segment indicates an ink! argument attribute
                    1 => {
                        let mut ink_arg_kind = InkArgAttributeKind::Unknown;
                        if args.len() > 0 {
                            if let Ok(kind) = InkArgAttributeKind::try_from(&args[0][..]) {
                                ink_arg_kind = kind;
                            }
                        }
                        InkAttributeKind::Arg(ink_arg_kind)
                    }
                    // 2 path segments indicates an ink! path attribute
                    2 => {
                        let ink_path_kind =
                            match InkPathAttributeKind::try_from(&path_segments[1][..]) {
                                Ok(kind) => kind,
                                _ => InkPathAttributeKind::Unknown,
                            };
                        InkAttributeKind::Path(ink_path_kind)
                    }
                    // treat attributes with more than 2 path segments as unknown path
                    _ => InkAttributeKind::Path(InkPathAttributeKind::Unknown),
                };
                return Attribute::Ink(InkAttribute {
                    ast: attr,
                    kind: ink_attr_kind,
                });
            }
        }

        Attribute::Other(attr)
    }
}

/// An ink! specific attribute.
#[derive(Debug, PartialEq, Eq)]
pub struct InkAttribute {
    /// AST Node for ink! attribute.
    pub ast: Attr,
    /// The kind of the ink! attribute e.g path attribute like `#[ink::contract]`
    /// or argument attribute like `#[ink(storage)]`.
    pub kind: InkAttributeKind,
    // TODO: Add attribute arguments
}

/// The kind of the ink! attribute.
#[derive(Debug, PartialEq, Eq)]
pub enum InkAttributeKind {
    /// ink! path attributes e.g `#[ink::contract]`.
    Path(InkPathAttributeKind),
    /// ink! argument attributes e.g `#[ink(storage)]`.
    Arg(InkArgAttributeKind),
}

/// An ink! path attribute kind.
#[derive(Debug, PartialEq, Eq)]
pub enum InkPathAttributeKind {
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
    /// Fallback for unrecognized ink! path attributes.
    Unknown,
}

impl TryFrom<&str> for InkPathAttributeKind {
    type Error = &'static str;

    fn try_from(path_segment: &str) -> Result<Self, Self::Error> {
        match path_segment {
            // `#[ink::chain_extension]`
            "chain_extension" => Ok(InkPathAttributeKind::ChainExtension),
            // `#[ink::contract]`
            "contract" => Ok(InkPathAttributeKind::Contract),
            // `#[ink::contract]`
            "storage_item" => Ok(InkPathAttributeKind::StorageItem),
            // `#[ink::contract]`
            "test" => Ok(InkPathAttributeKind::Test),
            // `#[ink::contract]`
            "trait_definition" => Ok(InkPathAttributeKind::TraitDefinition),
            // unknown attribute
            _ => Err("Unknown ink! path attribute."),
        }
    }
}

/// An ink! argument attribute kind.
#[derive(Debug, PartialEq, Eq)]
pub enum InkArgAttributeKind {
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
    /// Fallback for unrecognized ink! argument attribute.
    Unknown,
}

impl TryFrom<&str> for InkArgAttributeKind {
    type Error = &'static str;

    fn try_from(path_segment: &str) -> Result<Self, Self::Error> {
        match path_segment {
            // `#[ink::constructor]`
            "anonymous" => Ok(InkArgAttributeKind::Anonymous),
            // `#[ink::constructor]`
            "constructor" => Ok(InkArgAttributeKind::Constructor),
            // `#[ink::constructor]`
            "event" => Ok(InkArgAttributeKind::Event),
            // `#[ink::constructor]`
            "extension" => Ok(InkArgAttributeKind::Extension),
            // `#[ink::constructor]`
            "handle_status" => Ok(InkArgAttributeKind::HandleStatus),
            // `#[ink::constructor]`
            "impl" => Ok(InkArgAttributeKind::Impl),
            // `#[ink::constructor]`
            "message" => Ok(InkArgAttributeKind::Message),
            // `#[ink::constructor]`
            "namespace" => Ok(InkArgAttributeKind::Namespace),
            // `#[ink::constructor]`
            "payable" => Ok(InkArgAttributeKind::Payable),
            // `#[ink::constructor]`
            "selector" => Ok(InkArgAttributeKind::Selector),
            // `#[ink::constructor]`
            "storage" => Ok(InkArgAttributeKind::Storage),
            // `#[ink::constructor]`
            "topic" => Ok(InkArgAttributeKind::Topic),
            // unknown attribute
            _ => Err("Unknown ink! argument attribute."),
        }
    }
}
