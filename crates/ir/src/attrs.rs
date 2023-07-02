//! ink! attribute IR.

use ink_analyzer_macro::FromAST;
use ra_ap_syntax::{ast, AstNode, SyntaxNode};
use std::fmt;

use crate::traits::{FromAST, FromSyntax};

use crate::meta::MetaName;
pub use arg::{InkArg, InkArgKind, InkArgValueKind};

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
    ast: ast::Attr,
    /// ink! path segment node.
    ink: ast::PathSegment,
    /// ink! macro path segment node (if any) from which the attribute macro kind is derived.
    ink_macro: Option<ast::PathSegment>,
    /// ink! argument name (if any) from which the attribute argument kind is derived.
    ink_arg_name: Option<MetaName>,
}

impl InkAttribute {
    /// Converts an AST attribute (`Attr`) into an `InkAttribute` IR type.
    pub fn cast(attr: ast::Attr) -> Option<Self> {
        // Get attribute path segments.
        let mut path_segments = attr.path()?.segments();

        let ink_segment = path_segments.next()?;

        (ink_segment.to_string() == "ink").then(|| {
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
                    // No additional path segments means either an ink! attribute argument (e.g `#[ink(storage)]`) or an unknown attribute.
                    if args.is_empty() {
                        match attr.token_tree() {
                            // A token tree means an unknown ink! attribute argument.
                            Some(_) => InkAttributeKind::Arg(InkArgKind::Unknown),
                            // No token tree means an unknown ink! attribute macro.
                            None => InkAttributeKind::Macro(InkMacroKind::Unknown),
                        }
                    } else {
                        // Prioritize arguments so that we choose the best `InkArgKind` for the attribute.
                        // See `utils::sort_ink_args_by_kind` doc.
                        // Returns a new list so we don't change the original order for later analysis.
                        let sorted_args = utils::sort_ink_args_by_kind(&args);
                        let primary_arg = &sorted_args[0];
                        possible_ink_arg_name = primary_arg.name().cloned();
                        InkAttributeKind::Arg(*primary_arg.kind())
                    }
                }
            };

            Self {
                ast: attr,
                kind: ink_attr_kind,
                args,
                ink: ink_segment,
                ink_macro: possible_ink_macro_segment,
                ink_arg_name: possible_ink_arg_name,
            }
        })
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
    pub fn ink(&self) -> &ast::PathSegment {
        &self.ink
    }

    /// Returns the ink! macro path segment node (if any) from which the attribute macro kind is derived.
    pub fn ink_macro(&self) -> Option<&ast::PathSegment> {
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
    /// Converts a string slice representing an attribute path segment into an ink! attribute macro kind.
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
            ast: attr.syntax().parent().and_then(T::cast),
            syntax: attr
                .syntax()
                .parent()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use ra_ap_syntax::SyntaxKind;
    use test_utils::quote_as_str;

    #[test]
    fn cast_ink_attribute_works() {
        for (code, expected_ink_attr) in [
            // Macro with no arguments.
            (
                quote_as_str! {
                    #[ink::chain_extension]
                },
                Some((
                    InkAttributeKind::Macro(InkMacroKind::ChainExtension),
                    vec![],
                )),
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                },
                Some((InkAttributeKind::Macro(InkMacroKind::Contract), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink::storage_item]
                },
                Some((InkAttributeKind::Macro(InkMacroKind::StorageItem), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink::test]
                },
                Some((InkAttributeKind::Macro(InkMacroKind::Test), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition]
                },
                Some((
                    InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                    vec![],
                )),
            ),
            // Macro with arguments.
            (
                quote_as_str! {
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                },
                Some((
                    InkAttributeKind::Macro(InkMacroKind::Contract),
                    vec![
                        (InkArgKind::Env, Some(SyntaxKind::PATH)),
                        (InkArgKind::KeepAttr, Some(SyntaxKind::STRING)),
                    ],
                )),
            ),
            (
                quote_as_str! {
                    #[ink::storage_item(derive=true)]
                },
                Some((
                    InkAttributeKind::Macro(InkMacroKind::StorageItem),
                    vec![(InkArgKind::Derive, Some(SyntaxKind::TRUE_KW))],
                )),
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar")]
                },
                Some((
                    InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                    vec![
                        (InkArgKind::Namespace, Some(SyntaxKind::STRING)),
                        (InkArgKind::KeepAttr, Some(SyntaxKind::STRING)),
                    ],
                )),
            ),
            // Argument with no value.
            (
                quote_as_str! {
                    #[ink(storage)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Storage),
                    vec![(InkArgKind::Storage, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(anonymous)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Anonymous),
                    vec![(InkArgKind::Anonymous, None)],
                )),
            ),
            // Compound arguments with no value.
            // NOTE: Required and/or root-level/unambiguous arguments always have the highest priority,
            // so they become the attribute kind even when they're not the first attribute.
            (
                quote_as_str! {
                    #[ink(event, anonymous)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Event),
                    vec![(InkArgKind::Event, None), (InkArgKind::Anonymous, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(anonymous, event)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Event),
                    vec![(InkArgKind::Anonymous, None), (InkArgKind::Event, None)],
                )),
            ),
            // Argument with integer value.
            (
                quote_as_str! {
                    #[ink(selector=1)] // Decimal.
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Selector),
                    vec![(InkArgKind::Selector, Some(SyntaxKind::INT_NUMBER))],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(extension=0x1)] // Hexadecimal.
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Extension),
                    vec![(InkArgKind::Extension, Some(SyntaxKind::INT_NUMBER))],
                )),
            ),
            // Argument with wildcard/underscore value.
            (
                quote_as_str! {
                    #[ink(selector=_)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Selector),
                    vec![(InkArgKind::Selector, Some(SyntaxKind::UNDERSCORE))],
                )),
            ),
            // Argument with string value.
            (
                quote_as_str! {
                    #[ink(namespace="my_namespace")]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Namespace),
                    vec![(InkArgKind::Namespace, Some(SyntaxKind::STRING))],
                )),
            ),
            // Argument with boolean value.
            (
                quote_as_str! {
                    #[ink(handle_status=true)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::HandleStatus),
                    vec![(InkArgKind::HandleStatus, Some(SyntaxKind::TRUE_KW))],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(derive=false)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Derive),
                    vec![(InkArgKind::Derive, Some(SyntaxKind::FALSE_KW))],
                )),
            ),
            // Argument with path value.
            (
                quote_as_str! {
                    #[ink(env=my::env::Types)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Env),
                    vec![(InkArgKind::Env, Some(SyntaxKind::PATH))],
                )),
            ),
            // Compound arguments of different kinds.
            // NOTE: Required and/or root-level/unambiguous arguments always have the highest priority,
            // so they become the attribute kind even when they're not the first attribute.
            (
                quote_as_str! {
                    #[ink(message, payable, selector=1)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Message),
                    vec![
                        (InkArgKind::Message, None),
                        (InkArgKind::Payable, None),
                        (InkArgKind::Selector, Some(SyntaxKind::INT_NUMBER)),
                    ],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(selector=1, payable, message)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Message),
                    vec![
                        (InkArgKind::Selector, Some(SyntaxKind::INT_NUMBER)),
                        (InkArgKind::Payable, None),
                        (InkArgKind::Message, None),
                    ],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(event, anonymous)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Event),
                    vec![(InkArgKind::Event, None), (InkArgKind::Anonymous, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(anonymous, event)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Event),
                    vec![(InkArgKind::Anonymous, None), (InkArgKind::Event, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(extension=1, handle_status=false)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Extension),
                    vec![
                        (InkArgKind::Extension, Some(SyntaxKind::INT_NUMBER)),
                        (InkArgKind::HandleStatus, Some(SyntaxKind::FALSE_KW)),
                    ],
                )),
            ),
            // Unknown ink! macro.
            // NOTE: Macros always have the highest priority, even the unknown variety.
            (
                quote_as_str! {
                    #[ink]
                },
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink::]
                },
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink::unknown]
                },
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink::xyz]
                },
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink::unknown(message)]
                },
                Some((
                    InkAttributeKind::Macro(InkMacroKind::Unknown),
                    vec![(InkArgKind::Message, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink::unknown(selector=1)]
                },
                Some((
                    InkAttributeKind::Macro(InkMacroKind::Unknown),
                    vec![(InkArgKind::Selector, Some(SyntaxKind::INT_NUMBER))],
                )),
            ),
            // Unknown ink! argument.
            // NOTE: Unknown arguments always have the lowest priority.
            (
                quote_as_str! {
                    #[ink()]
                },
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), vec![])),
            ),
            (
                quote_as_str! {
                    #[ink(unknown)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Unknown),
                    vec![(InkArgKind::Unknown, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(xyz)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Unknown),
                    vec![(InkArgKind::Unknown, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(xyz="abc")]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Unknown),
                    vec![(InkArgKind::Unknown, Some(SyntaxKind::STRING))],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(message, unknown)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Message),
                    vec![(InkArgKind::Message, None), (InkArgKind::Unknown, None)],
                )),
            ),
            (
                quote_as_str! {
                    #[ink(unknown, message)]
                },
                Some((
                    InkAttributeKind::Arg(InkArgKind::Message),
                    vec![(InkArgKind::Unknown, None), (InkArgKind::Message, None)],
                )),
            ),
            // Non-ink attributes.
            // These simply return none.
            (
                quote_as_str! {
                    #[cfg_attr(not(feature = "std"), no_std)]
                },
                None,
            ),
        ] {
            // Parse attribute.
            let attr = parse_first_attribute(code);

            // Converts an attribute to an ink! attribute (if possible).
            let possible_ink_attr = InkAttribute::cast(attr);

            // Converts the ink! attribute to an array of tuples with
            // ink! attribute argument kind and an inner array of tuples with
            // ink! attribute argument kind and meta value syntax kind for easy comparisons.
            let actual_ink_attr: Option<(InkAttributeKind, Vec<(InkArgKind, Option<SyntaxKind>)>)> =
                possible_ink_attr.map(|ink_attr| {
                    (
                        // ink! attribute kind.
                        *ink_attr.kind(),
                        // array tuples of ink! attribute argument kind and meta value syntax kind.
                        ink_attr
                            .args()
                            .iter()
                            .map(|arg| (*arg.kind(), arg.value().map(|value| value.kind())))
                            .collect(),
                    )
                });

            // actual arguments should match expected arguments.
            assert_eq!(actual_ink_attr, expected_ink_attr);
        }
    }
}
