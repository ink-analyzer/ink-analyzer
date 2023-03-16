//! ink! attribute intermediate representations (IRs) and abstractions.
use ra_ap_syntax::ast::{Attr, PathSegment};
use ra_ap_syntax::AstNode;

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
        let path_segments: Vec<String> = attr
            .syntax()
            .descendants()
            .filter_map(PathSegment::cast)
            .map(|item| item.syntax().text().to_string())
            .collect();

        let num_segments = path_segments.len();
        if num_segments > 0 && path_segments[0] == "ink" {
            let ink_attr_kind = match num_segments {
                // 1 path segment indicates an ink! argument attribute
                1 => InkAttributeKind::Arg(InkArgAttributeKind::Unknown),
                // 2 path segments indicates an ink! macro attribute
                2 => {
                    let ink_macro_kind =
                        match InkMacroAttributeKind::try_from(&path_segments[1][..]) {
                            Ok(kind) => kind,
                            _ => InkMacroAttributeKind::Unknown,
                        };
                    InkAttributeKind::Macro(ink_macro_kind)
                }
                // treat anything more than 2 path segments as unknown
                _ => InkAttributeKind::Unknown,
            };
            return Attribute::Ink(InkAttribute {
                ast: attr,
                kind: ink_attr_kind,
            });
        }
        Attribute::Other(attr)
    }
}

/// An ink! specific attribute.
#[derive(Debug, PartialEq, Eq)]
pub struct InkAttribute {
    /// AST Node for ink! attribute.
    pub ast: Attr,
    /// The kind of the ink! attribute e.g macro attributes like `#[ink::contract]`
    /// or an argument attribute like `#[ink(storage)]`.
    pub kind: InkAttributeKind,
}

/// The kind of the ink! attribute.
#[derive(Debug, PartialEq, Eq)]
pub enum InkAttributeKind {
    /// ink! macro attributes e.g `#[ink::contract]`.
    Macro(InkMacroAttributeKind),
    /// ink! argument attributes e.g `#[ink(storage)]`.
    Arg(InkArgAttributeKind),
    /// Fallback for unrecognized ink attributes.
    Unknown,
}

/// The kind of the ink! macro attribute.
#[derive(Debug, PartialEq, Eq)]
pub enum InkMacroAttributeKind {
    /// `#[ink::contract]`.
    Contract,
    // TODO: Add more macro attributes
    /// Fallback for unrecognized ink macro attributes.
    Unknown,
}

impl TryFrom<&str> for InkMacroAttributeKind {
    type Error = &'static str;

    fn try_from(path_segment: &str) -> Result<Self, Self::Error> {
        match path_segment {
            // `#[ink::contract]`
            "contract" => Ok(InkMacroAttributeKind::Contract),
            // TODO: Add more macro attributes e.g `#[ink::chain_extension]`
            _ => Err("Unknown ink! macro attribute"),
        }
    }
}

/// The kind of the ink! argument attribute.
#[derive(Debug, PartialEq, Eq)]
pub enum InkArgAttributeKind {
    // TODO: Add argument attributes e.g `#[ink(storage)]`
    /// Fallback for unrecognized ink macro attributes.
    Unknown,
}
