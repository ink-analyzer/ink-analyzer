//! Utilities for ink! diagnostics.

use ink_analyzer_ir::ast::{AstNode, AstToken, HasGenericParams, HasTypeBounds, HasVisibility};
use ink_analyzer_ir::meta::{MetaOption, MetaValue};
use ink_analyzer_ir::syntax::{SourceFile, SyntaxElement, SyntaxKind};
use ink_analyzer_ir::{
    ast, Contract, FromSyntax, InkArg, InkArgKind, InkArgValueKind, InkAttribute, InkAttributeKind,
    InkMacroKind, IsInkEntity, IsInkFn, IsInkImplItem, IsInkStruct, IsInkTrait,
};
use std::collections::HashSet;

use crate::analysis::utils;
use crate::{Diagnostic, Severity};

/// Runs generic diagnostics that apply to all ink! entities.
/// (e.g `ensure_no_unknown_ink_attributes`, `ensure_no_ink_identifiers`,
/// `ensure_no_duplicate_attributes_and_arguments`, `ensure_valid_attribute_arguments`).
pub fn run_generic_diagnostics<T: FromSyntax>(results: &mut Vec<Diagnostic>, item: &T) {
    // Ensures that no `__ink_` prefixed identifiers, see `ensure_no_ink_identifiers` doc.
    ensure_no_ink_identifiers(results, item);

    // Ensures that no invalid ink! attributes, see `ensure_no_invalid_ink_attributes` doc.
    ensure_no_unknown_ink_attributes(
        results,
        &item
            .tree()
            .ink_attrs_in_scope()
            .collect::<Vec<InkAttribute>>(),
    );

    // Ensures that ink! attribute arguments are of the right format and have values are of the correct type (if any),
    // See `ensure_valid_attribute_arguments` doc.
    item.tree()
        .ink_attrs()
        .for_each(|attr| ensure_valid_attribute_arguments(results, &attr));

    // Ensures that no duplicate ink! attributes and/or arguments, see `ensure_no_duplicate_attributes_and_arguments` doc.
    ensure_no_duplicate_attributes_and_arguments(
        results,
        &item.tree().ink_attrs().collect::<Vec<InkAttribute>>(),
    );

    // Ensures that no conflicting ink! attributes and/or arguments, see `ensure_no_conflicting_attributes_and_arguments` doc.
    ensure_no_conflicting_attributes_and_arguments(
        results,
        &item.tree().ink_attrs().collect::<Vec<InkAttribute>>(),
    );
}

/// Returns an error diagnostic for every instance of `__ink_` prefixed identifier found.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/idents_lint.rs#L20>.
fn ensure_no_ink_identifiers<T: FromSyntax>(results: &mut Vec<Diagnostic>, item: &T) {
    item.syntax().descendants_with_tokens().for_each(|elem| {
        elem.into_token()
            .and_then(ast::Ident::cast)
            .and_then(|ident| {
                ident.to_string().starts_with("__ink_").then(|| {
                    results.push(Diagnostic {
                        message: format!(
                            "Invalid identifier starting with __ink_: {}",
                            ident.text()
                        ),
                        range: ident.syntax().text_range(),
                        severity: Severity::Error,
                    });
                })
            });
    });
}

/// Returns a warning diagnostic for every unknown ink! attribute found.
///
/// Handles both ink! attribute macros (e.g `#[ink::xyz]`)
/// and ink! attribute arguments (e.g `#[ink(xyz)]`).
/// Relies on the ink! attribute kind, so while it catches all unknown ink! attribute macros (e.g `#[ink::xyz]`),
/// It only catches unknown ink! attribute arguments if they're the only annotation for the attribute (e.g `#[ink(xyz)]`),
/// It doesn't catch unknown arguments appearing in combination with valid ink! attribute macros and arguments
/// (e.g `#[ink::contract(xyz)]` or `#[ink(storage, xyz)]`).
/// Those are handled by `ensure_valid_attribute_arguments`.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L876-L1024>.
fn ensure_no_unknown_ink_attributes(results: &mut Vec<Diagnostic>, attrs: &[InkAttribute]) {
    for attr in attrs {
        matches!(
            attr.kind(),
            InkAttributeKind::Macro(InkMacroKind::Unknown)
                | InkAttributeKind::Arg(InkArgKind::Unknown)
        )
        .then(|| {
            results.push(Diagnostic {
                message: format!("Unknown ink! attribute: '{}'", attr.syntax()),
                range: if let Some(ink_path) = attr.ink_macro() {
                    ink_path.syntax().text_range()
                } else if let Some(ink_arg) = attr.ink_arg_name() {
                    ink_arg.syntax().text_range()
                } else {
                    attr.syntax().text_range()
                },
                severity: Severity::Warning, // warning because it's possible ink! analyzer is just outdated.
            });
        });
    }
}

/// Ensures that ink! attribute arguments are of the right format and have values (if any) of the correct type.
///
/// This utility only cares about ink! attribute arguments, not ink! attribute macros.
/// So `#[ink(env=my::env::Types)]` will pass with no complaints.
/// Invalid ink! attribute macros are handled by `ensure_no_unknown_ink_attributes`
/// while `ensure_no_conflicting_attributes_and_arguments` is responsible for
/// flagging ink! attribute conflicts across ink! attribute macros and ink! attribute arguments.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L879-L1023>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/utils.rs#L92-L107>.
fn ensure_valid_attribute_arguments(results: &mut Vec<Diagnostic>, attr: &InkAttribute) {
    for arg in attr.args() {
        let text_range = arg.text_range();
        let arg_name_text = arg.meta().name().to_string();
        match arg.kind() {
            // Handle unknown argument.
            InkArgKind::Unknown => results.push(Diagnostic {
                message: if arg_name_text.is_empty() {
                    "Missing ink! attribute argument.".to_string()
                } else {
                    format!("Unknown ink! attribute argument: '{arg_name_text}'.")
                },
                range: text_range,
                severity: if arg_name_text.is_empty() {
                    // error for missing.
                    Severity::Error
                } else {
                    // warning because it's possible ink! analyzer is just outdated.
                    Severity::Warning
                },
            }),
            arg_kind => {
                let arg_value_type = InkArgValueKind::from(*arg_kind);
                match arg_value_type {
                    // Arguments that must have no value.
                    InkArgValueKind::None => {
                        if arg.meta().eq().is_some() || arg.meta().value().is_some() {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument shouldn't have a value."
                                ),
                                range: text_range,
                                severity: Severity::Error,
                            });
                        }
                    }
                    // Arguments that should have an integer (`u32` to be specific) value.
                    InkArgValueKind::U32 | InkArgValueKind::U32OrWildcard => {
                        let can_be_wildcard = arg_value_type == InkArgValueKind::U32OrWildcard;
                        if !ensure_valid_attribute_arg_value(
                            arg,
                            |meta_value| {
                                // Ensures that the meta value is either a decimal or hex encoded `u32`.
                                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L903-L910>.
                                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L938-L943>.
                                meta_value.as_u32().is_some()
                                        // A wildcard/underscore (`_`) is also valid value for selectors.
                                        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L884-L900>.
                                        || (can_be_wildcard
                                        && meta_value.is_wildcard())
                            },
                            |_| false,
                            false,
                        ) {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{}` argument should have an `integer` (`u32`) {} value.",
                                    arg_name_text,
                                    if can_be_wildcard {
                                        "or wildcard/underscore (`_`)"
                                    } else {
                                        ""
                                    }
                                ),
                                range: text_range,
                                severity: Severity::Error,
                            });
                        }
                    }
                    // Arguments that should have a string value.
                    InkArgValueKind::String | InkArgValueKind::StringIdentifier => {
                        if !ensure_valid_attribute_arg_value(
                            arg,
                            |meta_value| {
                                meta_value.as_string().is_some()
                                    // For namespace arguments, ensure the meta value is a valid Rust identifier.
                                    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L922-L926>.
                                    && (arg_value_type == InkArgValueKind::String || meta_value.as_string().and_then(|value| parse_ident(value.as_str())).is_some())
                            },
                            |_| false,
                            false,
                        ) {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{}` argument should have a {} `string` (`&str`) value.",
                                    arg_name_text,
                                    if *arg.kind() == InkArgKind::KeepAttr {
                                        "comma separated"
                                    } else {
                                        ""
                                    }
                                ),
                                range: text_range,
                                severity: Severity::Error,
                            });
                        }
                    }
                    // Arguments that should have a boolean value.
                    InkArgValueKind::Bool => {
                        if !ensure_valid_attribute_arg_value(
                            arg,
                            |meta_value| meta_value.as_boolean().is_some(),
                            |_| false,
                            false,
                        ) {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument should have a `boolean` (`bool`) value."
                                ),
                                range: text_range,
                                severity: Severity::Error,
                            });
                        }
                    }
                    // Arguments that should have a path value.
                    InkArgValueKind::Path => {
                        if !ensure_valid_attribute_arg_value(
                            arg,
                            |meta_value| {
                                matches!(
                                    meta_value.kind(),
                                    SyntaxKind::PATH | SyntaxKind::PATH_EXPR
                                )
                            },
                            |_| false,
                            false,
                        ) {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument should have a `path` (e.g `my::env::Types`) value."
                                ),
                                range: text_range,
                                severity: Severity::Error,
                            });
                        }
                    }
                }
            }
        };
    }
}

/// Casts a string to an Rust identifier (`Ident`) (if possible).
fn parse_ident(value: &str) -> Option<ast::Ident> {
    // Parse sanitized value and find the first identifier.
    let file = SourceFile::parse(value).tree();

    // Retrieve the first `ident` in the syntax tree.
    let ident = file
        .syntax()
        .descendants_with_tokens()
        .find_map(|elem| ast::Ident::cast(elem.into_token()?))?;

    // Parsed identifier must be equal to the sanitized meta value.
    (ident.text() == value).then_some(ident)
}

/// Ensures the validity of an ink! argument value using provided ok and err handlers and none outcome.
fn ensure_valid_attribute_arg_value<F, G>(
    arg: &InkArg,
    ok_handler: F,
    err_handler: G,
    none_outcome: bool,
) -> bool
where
    F: Fn(&MetaValue) -> bool,
    G: Fn(&[SyntaxElement]) -> bool,
{
    match &arg.meta().value() {
        MetaOption::Ok(meta_value) => ok_handler(meta_value),
        MetaOption::Err(items) => err_handler(items),
        MetaOption::None => none_outcome,
    }
}

/// Ensures that no duplicate ink! attributes and/or arguments.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L169-L208>.
fn ensure_no_duplicate_attributes_and_arguments(
    results: &mut Vec<Diagnostic>,
    attrs: &[InkAttribute],
) {
    let mut seen_macros: HashSet<&InkMacroKind> = HashSet::new();
    let mut seen_args: HashSet<&InkArgKind> = HashSet::new();

    for attr in attrs {
        if let InkAttributeKind::Macro(macro_kind) = attr.kind() {
            // Unknown ink! attribute macros are ignored.
            if *macro_kind != InkMacroKind::Unknown && seen_macros.get(macro_kind).is_some() {
                results.push(Diagnostic {
                    message: format!("Duplicate ink! attribute macro: '{}'", attr.syntax()),
                    range: attr.syntax().text_range(),
                    severity: Severity::Error,
                });
            }
            seen_macros.insert(macro_kind);
        }

        for arg in attr.args() {
            let arg_kind = arg.kind();
            // Unknown ink! attribute arguments are ignored.
            if *arg_kind != InkArgKind::Unknown && seen_args.get(arg_kind).is_some() {
                results.push(Diagnostic {
                    message: format!("Duplicate ink! attribute argument: '{}'", arg.meta().name()),
                    range: arg.text_range(),
                    severity: Severity::Error,
                });
            }

            seen_args.insert(arg_kind);
        }
    }
}

/// Ensures that no conflicting ink! attributes and/or arguments.
///
/// In addition to straight forward conflicts
/// (e.g both `contract` and `trait_definition` applied to the same item,
/// `anonymous` combined with `message`  or `derive` on a `contract` attribute),
/// 3 more cases are considered conflicts:
///
/// 1. Wrong order of otherwise valid attributes and/or arguments
/// (e.g `payable` declared before `message` or `anonymous` declared before `event`).
///
/// 2. Incompleteness (e.g `anonymous` without `event` or `derive` without `storage_item` attribute macro)
///
/// 3. Ambiguity (e.g `payable` with neither `constructor` nor `message` or
/// `keep_attr` with neither `contract` nor `trait_definition` attribute macros) are also treated as conflicts
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L613-L658>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L829-L872>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L154-L167>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L829-L873>.
fn ensure_no_conflicting_attributes_and_arguments(
    results: &mut Vec<Diagnostic>,
    attrs: &[InkAttribute],
) {
    // We can only move forward if there is at least one "valid" attribute.
    // We treat the first "valid" attribute like the "primary" attribute that other attributes and arguments shouldn't conflict with.
    if let Some(first_valid_attribute) = attrs.iter().find(|attr| {
        // Ignore unknown attributes.
        !matches!(
            attr.kind(),
            InkAttributeKind::Macro(InkMacroKind::Unknown)
                | InkAttributeKind::Arg(InkArgKind::Unknown)
        )
    }) {
        // sibling arguments are arguments that don't conflict with the primary attribute's kind,
        // see `utils::valid_sibling_ink_args` doc.
        let valid_sibling_args = utils::valid_sibling_ink_args(*first_valid_attribute.kind());

        // We want to suggest a primary attribute in case the current one is either
        // incomplete (e.g `anonymous` without `event` or `derive` without `storage_item` attribute macro)
        // or ambiguous (e.g `payable` with neither `constructor` nor `message` or
        // `keep_attr` with neither `contract` nor `trait_definition` attribute macros).
        let primary_attribute_kind_suggestions = match first_valid_attribute.kind() {
            InkAttributeKind::Arg(arg_kind) => {
                // Only ink! attribute arguments when set as the primary attribute have
                // the potential to be either incomplete or ambiguous.
                // See respective match pattern in the `utils::valid_sibling_ink_args` function for the rationale and references.
                match arg_kind {
                    InkArgKind::Anonymous => vec![InkAttributeKind::Arg(InkArgKind::Event)],
                    InkArgKind::KeepAttr => vec![
                        InkAttributeKind::Macro(InkMacroKind::Contract),
                        InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                    ],
                    InkArgKind::HandleStatus => vec![InkAttributeKind::Arg(InkArgKind::Extension)],
                    InkArgKind::Namespace => vec![
                        InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                        InkAttributeKind::Arg(InkArgKind::Impl),
                    ],
                    InkArgKind::Payable | InkArgKind::Default | InkArgKind::Selector => vec![
                        InkAttributeKind::Arg(InkArgKind::Constructor),
                        InkAttributeKind::Arg(InkArgKind::Message),
                    ],
                    // Default
                    _ => Vec::new(),
                }
            }
            // ink! attribute macros are always complete and unambiguous on their own.
            InkAttributeKind::Macro(_) => Vec::new(),
        };

        // For `namespace`, additional context is required to determine what do with
        // the primary attribute kind suggestions, because while namespace can be ambiguous,
        // it's also valid on its own. See its match pattern in
        // the `utils::valid_sibling_ink_args` function for the rationale and references.
        let is_namespace =
            *first_valid_attribute.kind() == InkAttributeKind::Arg(InkArgKind::Namespace);

        // If the first valid ink! attribute is complete and unambiguous (or it's `namespace` :-)),
        // and it's ink! attribute kind is a ink! attribute argument kind,
        // make sure that ink! attribute argument is also the first in the argument list.
        if primary_attribute_kind_suggestions.is_empty() || is_namespace {
            if let InkAttributeKind::Arg(arg_kind) = first_valid_attribute.kind() {
                let is_primary_arg_first =
                    if let Some(first_arg) = first_valid_attribute.args().first() {
                        first_arg.kind() == arg_kind
                    } else {
                        // This case shouldn't really ever happen.
                        false
                    };
                if !is_primary_arg_first {
                    // Find the primary arg.
                    let primary_arg = first_valid_attribute
                        .args()
                        .iter()
                        .find(|arg| arg.kind() == arg_kind);
                    // Suggest that it should become the first argument.
                    results.push(Diagnostic {
                        message: format!(
                            "`{}` should be the first argument for this ink! attribute: {}.",
                            arg_kind,
                            first_valid_attribute.syntax()
                        ),
                        range: if let Some(arg) = primary_arg {
                            arg.text_range()
                        } else {
                            first_valid_attribute.syntax().text_range()
                        },
                        severity: Severity::Error,
                    });
                }
            }
        }

        // If the first valid ink! attribute is either incomplete or ambiguous or both,
        // then suggest possible primary attributes.
        if !primary_attribute_kind_suggestions.is_empty() {
            // Find the first potential primary attribute (if any).
            let potential_primary = attrs
                .iter()
                .find(|attr| primary_attribute_kind_suggestions.contains(attr.kind()));
            if let Some(attr) = potential_primary {
                // If there's already a potential primary attribute in the list, suggest it.
                results.push(Diagnostic {
                    message: format!(
                        "`{}` should be the first ink! attribute for this item.",
                        attr.syntax(),
                    ),
                    range: first_valid_attribute.syntax().text_range(),
                    severity: Severity::Error,
                });
            } else if !is_namespace {
                // Otherwise make a generic suggestion about adding the suggested ink! attribute kinds,
                // unless the first valid ink! attribute is a `namespace` which is valid on it's own.
                results.push(Diagnostic {
                    message: format!(
                        "An {} attribute should be the first ink! attribute for this item.",
                        primary_attribute_kind_suggestions
                            .iter()
                            .map(|attr_kind| {
                                match attr_kind {
                                    InkAttributeKind::Arg(arg_kind) => {
                                        format!("`ink! {arg_kind}`")
                                    }
                                    InkAttributeKind::Macro(macro_kind) => {
                                        format!("`ink! {macro_kind}`")
                                    }
                                }
                            })
                            .collect::<Vec<String>>()
                            .join(" or "), // It's never more than suggestions at the moment.
                    ),
                    range: first_valid_attribute.syntax().text_range(),
                    severity: Severity::Error,
                });
            }
        }

        for attr in attrs {
            // Check for attribute kind level conflict.
            let is_conflicting_attribute = if attr == first_valid_attribute {
                // Primary attribute can't conflict with itself.
                false
            } else {
                match first_valid_attribute.kind() {
                    // ink! attribute macros are never mixed with other ink! attributes.
                    // So any additional ink! attribute macro is always a conflict.
                    InkAttributeKind::Macro(_) => true,
                    // ink! attribute arguments can be mixed with other ink! attributes.
                    InkAttributeKind::Arg(_) => {
                        match attr.kind() {
                            // Additional ink! attribute macros have to be
                            // potential primary attributes inorder not to conflict,
                            // in which case our we let incompleteness and ambiguity above checks take care of that case.
                            InkAttributeKind::Macro(_) => {
                                !primary_attribute_kind_suggestions.contains(attr.kind())
                            }
                            // Additional ink! attribute arguments have to be valid siblings.
                            InkAttributeKind::Arg(arg_kind) => {
                                !valid_sibling_args.contains(arg_kind)
                            }
                        }
                    }
                }
            };

            // Handle attribute kind level conflict.
            if is_conflicting_attribute {
                results.push(Diagnostic {
                    message: format!(
                        "ink! attribute `{}` conflicts with the first ink! attribute `{}` for this item.",
                        attr.syntax(),
                        first_valid_attribute.syntax()
                    ),
                    range: attr.syntax().text_range(),
                    severity: Severity::Error,
                });
            } else {
                // Handle argument level conflicts if the top level attribute kind doesn't conflict.
                for arg in attr.args() {
                    // Checks if the this arg kind is the same as the one being used by the attribute,
                    // which is already known to not be conflicting at this point.
                    let is_attribute_kind =
                        if let InkAttributeKind::Arg(attr_arg_kind) = attr.kind() {
                            arg.kind() == attr_arg_kind
                        } else {
                            false
                        };

                    // Ignore arg if it's already been handled at attribute level.
                    if !is_attribute_kind && !valid_sibling_args.contains(arg.kind()) {
                        results.push(Diagnostic {
                            message: format!(
                                "ink! attribute argument `{}` conflicts with the {} for this item.",
                                arg.meta().name(),
                                if attr == first_valid_attribute {
                                    match first_valid_attribute.kind() {
                                        InkAttributeKind::Arg(arg_kind) => {
                                            format!("ink! attribute argument `{arg_kind}`",)
                                        }
                                        InkAttributeKind::Macro(macro_kind) => {
                                            format!("ink! attribute macro `{macro_kind}`")
                                        }
                                    }
                                } else {
                                    format!(
                                        "first ink! attribute `{}`",
                                        first_valid_attribute.syntax()
                                    )
                                }
                            ),
                            range: arg.text_range(),
                            severity: Severity::Error,
                        });
                    }
                }
            }
        }
    }
}

/// Ensures that at least one item is defined.
pub fn ensure_at_least_one_item<T>(
    items: &[T],
    empty_diagnostic: Diagnostic,
) -> Option<Diagnostic> {
    items.is_empty().then_some(empty_diagnostic)
}

/// Ensures that an item is not missing and there are not multiple definitions of it as well.
pub fn ensure_exactly_one_item<T: FromSyntax>(
    results: &mut Vec<Diagnostic>,
    items: &[T],
    empty_diagnostic: Diagnostic,
    error_too_many: &str,
    severity_too_many: Severity,
) {
    if items.is_empty() {
        results.push(empty_diagnostic);
    } else {
        ensure_at_most_one_item(results, items, error_too_many, severity_too_many);
    }
}

/// Ensures that there are not multiple definitions of an item.
pub fn ensure_at_most_one_item<T: FromSyntax>(
    results: &mut Vec<Diagnostic>,
    items: &[T],
    message: &str,
    severity: Severity,
) {
    if items.len() > 1 {
        items[1..].iter().for_each(|item| {
            results.push(Diagnostic {
                message: message.to_string(),
                range: item.syntax().text_range(),
                severity,
            });
        });
    }
}

/// Ensures that ink! entity is a `struct` with `pub` visibility.
pub fn ensure_pub_struct<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: FromSyntax + IsInkStruct,
{
    let mut marker_token = None;

    let error = match item.struct_item() {
        Some(struct_item) => {
            let has_pub_visibility = match struct_item.visibility() {
                Some(visibility) => {
                    marker_token = Some(visibility.clone());
                    visibility.to_string() == "pub"
                }
                None => false,
            };

            (!has_pub_visibility)
                .then_some(format!("ink! {ink_scope_name} must have `pub` visibility.",))
        }
        None => Some(format!("ink! {ink_scope_name} must be a `struct` item.",)),
    };

    error.map(|message| Diagnostic {
        message,
        range: if let Some(marker) = marker_token {
            marker.syntax().text_range()
        } else {
            item.syntax().text_range()
        },
        severity: Severity::Error,
    })
}

/// Ensures that ink! entity is an `fn` item.
pub fn ensure_fn<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: FromSyntax + IsInkFn,
{
    item.fn_item().is_none().then_some(Diagnostic {
        message: format!("ink! {ink_scope_name} must be an `fn` item.",),
        range: item.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that ink! entity is a `trait` item.
pub fn ensure_trait<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: FromSyntax + IsInkTrait,
{
    item.trait_item().is_none().then_some(Diagnostic {
        message: format!("ink! {ink_scope_name} must be a `trait` item.",),
        range: item.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that an `fn` item has no self receiver (i.e no `&self`, `&mut self`, self or mut self).
pub fn ensure_no_self_receiver(fn_item: &ast::Fn, ink_scope_name: &str) -> Option<Diagnostic> {
    fn_item.param_list()?.self_param().map(|self_param| Diagnostic {
        message: format!("ink! {ink_scope_name} must not have a self receiver (i.e no `&self`, `&mut self`, self or mut self)."),
        range: self_param.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that item is has no generic parameters.
pub fn ensure_no_generics<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: HasGenericParams,
{
    item.generic_param_list().map(|generics| Diagnostic {
        message: format!(
            "Generic parameters on an ink! {ink_scope_name} are not currently supported."
        ),
        range: generics.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that item is has no trait bounds.
pub fn ensure_no_trait_bounds<T>(item: &T, message: &str) -> Option<Diagnostic>
where
    T: HasTypeBounds,
{
    item.type_bound_list().map(|type_bound_list| Diagnostic {
        message: message.to_string(),
        range: type_bound_list.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that `fn` item satisfies all common invariants of method-based ink! entities
/// (i.e `constructor`s, `message`s and `extension`s).
///
/// See reference below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/callable.rs#L355-L440>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L395-L465>.
pub fn ensure_method_invariants(
    results: &mut Vec<Diagnostic>,
    fn_item: &ast::Fn,
    ink_scope_name: &str,
) {
    if let Some(diagnostic) = ensure_no_generics(fn_item, ink_scope_name) {
        results.push(diagnostic);
    }

    if let Some(const_token) = fn_item.const_token() {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `const`."),
            range: const_token.text_range(),
            severity: Severity::Error,
        });
    }

    if let Some(async_token) = fn_item.async_token() {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `async`."),
            range: async_token.text_range(),
            severity: Severity::Error,
        });
    }

    if let Some(unsafe_token) = fn_item.unsafe_token() {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `unsafe`."),
            range: unsafe_token.text_range(),
            severity: Severity::Error,
        });
    }

    if let Some(abi) = fn_item.abi() {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not have explicit ABI."),
            range: abi.syntax().text_range(),
            severity: Severity::Error,
        });
    }

    if let Some(param_list) = fn_item.param_list() {
        results.append(
            &mut param_list
                .params()
                .filter_map(|param| {
                    param.dotdotdot_token().map(|dotdotdot| Diagnostic {
                        message: format!("ink! {ink_scope_name} must not be variadic."),
                        range: dotdotdot.text_range(),
                        severity: Severity::Error,
                    })
                })
                .collect(),
        );
    }
}

/// Ensures that `fn` item satisfies all common invariants of externally callable ink! entities
/// (i.e `constructor`s and `message`s).
///
/// See reference below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/callable.rs#L355-L440>.
pub fn ensure_callable_invariants(
    results: &mut Vec<Diagnostic>,
    fn_item: &ast::Fn,
    ink_scope_name: &str,
) {
    let (has_pub_or_inherited_visibility, visibility) = match fn_item.visibility() {
        // Check `pub` visibility.
        Some(visibility) => (visibility.to_string() == "pub", Some(visibility)),
        // Inherited visibility.
        None => (true, None),
    };

    if !has_pub_or_inherited_visibility {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must have `pub` or inherited visibility."),
            range: match visibility {
                Some(vis) => vis.syntax().text_range(),
                None => fn_item.syntax().text_range(),
            },
            severity: Severity::Error,
        });
    }

    // See `ensure_method_invariants` doc.
    ensure_method_invariants(results, fn_item, ink_scope_name);
}

/// Ensures that `trait` item satisfies all common invariants of trait-based ink! entities
/// (i.e `trait_definition`s and `chain_extension`s).
///
/// See references below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L108-L148>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L213-L254>.
pub fn ensure_trait_invariants(
    results: &mut Vec<Diagnostic>,
    trait_item: &ast::Trait,
    ink_scope_name: &str,
) {
    if let Some(unsafe_token) = trait_item.unsafe_token() {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `unsafe`."),
            range: unsafe_token.text_range(),
            severity: Severity::Error,
        });
    }

    if let Some(auto_token) = trait_item.auto_token() {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `auto` implemented."),
            range: auto_token.text_range(),
            severity: Severity::Error,
        });
    }

    if let Some(diagnostic) = ensure_no_generics(trait_item, ink_scope_name) {
        results.push(diagnostic);
    }

    let (has_pub_visibility, visibility) = match trait_item.visibility() {
        // Check `pub` visibility.
        Some(visibility) => (visibility.to_string() == "pub", Some(visibility)),
        // Inherited visibility.
        None => (false, None),
    };

    if !has_pub_visibility {
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must have `pub` visibility."),
            range: match visibility {
                Some(vis) => vis.syntax().text_range(),
                None => trait_item.syntax().text_range(),
            },
            severity: Severity::Error,
        });
    }

    if let Some(diagnostic) = ensure_no_trait_bounds(
        trait_item,
        format!("ink! {ink_scope_name} must not have any `supertraits`.").as_str(),
    ) {
        results.push(diagnostic);
    }
}

/// Ensures that item is a `trait` whose associated items satisfy all common invariants of associated items for ink! entities
/// (i.e `trait_definition`s and `chain_extension`s).
///
/// See references below for details about checked invariants.
/// Note that this utility only implements the common invariants between both `trait_definition`s and `chain_extension`s.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L150-L208>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L309-L393>.
pub fn ensure_trait_item_invariants<F, G>(
    results: &mut Vec<Diagnostic>,
    trait_item: &ast::Trait,
    ink_scope_name: &str,
    assoc_fn_handler: F,
    assoc_type_handler: G,
) where
    F: Fn(&mut Vec<Diagnostic>, &ast::Fn),
    G: Fn(&mut Vec<Diagnostic>, &ast::TypeAlias),
{
    if let Some(assoc_item_list) = trait_item.assoc_item_list() {
        assoc_item_list.assoc_items().for_each(|assoc_item| {
            match assoc_item {
                ast::AssocItem::Const(node) => results.push(Diagnostic {
                    message: format!(
                        "Associated `const` items in an ink! {ink_scope_name} are not yet supported."
                    ),
                    range: node.syntax().text_range(),
                    severity: Severity::Error,
                }),
                ast::AssocItem::MacroCall(node) => results.push(Diagnostic {
                    message: format!(
                        "Macros in an ink! {ink_scope_name} are not supported."
                    ),
                    range: node.syntax().text_range(),
                    severity: Severity::Error,
                }),
                ast::AssocItem::TypeAlias(type_alias) => assoc_type_handler(results, &type_alias),
                ast::AssocItem::Fn(fn_item) => {
                    // No default implementations.
                    if let Some(body) = fn_item.body() {
                        results.push(Diagnostic {
                            message: format!("ink! {ink_scope_name} methods with a default implementation are not currently supported."),
                            range: body.syntax().text_range(),
                            severity: Severity::Error,
                        });
                    }

                    assoc_fn_handler(results, &fn_item);
                },
            }
        });
    }
}

/// Ensures that item is defined in the root of an ink! contract.
pub fn ensure_contract_parent<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: FromSyntax,
{
    let has_contract_parent = ink_analyzer_ir::ink_parent::<Contract>(item.syntax()).is_some();
    (!has_contract_parent).then_some(Diagnostic {
        message: format!(
            "ink! {ink_scope_name} must be defined in the root of an ink! contract `mod`.",
        ),
        range: item.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that item is defined in the root of an `impl` item.
pub fn ensure_impl_parent<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: FromSyntax + IsInkImplItem,
{
    item.impl_item().is_none().then_some(Diagnostic {
        message: format!("ink! {ink_scope_name} must be defined in the root of an `impl` block.",),
        range: item.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
pub fn ensure_valid_quasi_direct_ink_descendants<T, F>(
    results: &mut Vec<Diagnostic>,
    item: &T,
    is_valid_quasi_direct_descendant: F,
) where
    T: FromSyntax,
    F: Fn(&InkAttribute) -> bool,
{
    item.tree()
        .ink_attrs_closest_descendants()
        .for_each(|attr| {
            (!is_valid_quasi_direct_descendant(&attr)).then(|| {
                results.push(Diagnostic {
                    message: format!("Invalid scope for an `{}` item.", attr.syntax()),
                    range: attr
                        .syntax()
                        .parent()
                        .unwrap_or(attr.syntax().clone())
                        .text_range(),
                    severity: Severity::Error,
                });
            });
        });
}

/// Ensures that no ink! descendants in the item's scope.
pub fn ensure_no_ink_descendants<T>(results: &mut Vec<Diagnostic>, item: &T, ink_scope_name: &str)
where
    T: FromSyntax,
{
    item.tree().ink_attrs_descendants().for_each(|attr| {
        results.push(Diagnostic {
            message: format!(
                "`{}` cannot be used inside an ink! {ink_scope_name}.",
                attr.syntax()
            ),
            range: attr
                .syntax()
                .parent()
                .unwrap_or(attr.syntax().clone())
                .text_range(),
            severity: Severity::Error,
        });
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::InkFile;
    use test_utils::quote_as_str;

    fn parse_first_ink_attr(code: &str) -> InkAttribute {
        InkFile::parse(code)
            .tree()
            .ink_attrs_in_scope()
            .next()
            .unwrap()
    }

    fn parse_all_ink_attrs(code: &str) -> Vec<InkAttribute> {
        InkFile::parse(code).tree().ink_attrs_in_scope().collect()
    }

    // List of valid ink! attributes used for positive(`works`) tests
    // for ink! attribute and/argument verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L188-L197>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L111-L199>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/config.rs#L36-L59>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27-L30>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/config.rs#L60-L85>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L83-L93>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L88-L98>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L301-L315>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L136-L148>.
    // Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/constructor.rs#L136-L149>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L182-L194>.
    // Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/message.rs#L182-L195>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L476-L487>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/config.rs#L36-L59>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L316-L321>.
    macro_rules! valid_attributes {
        () => {
            [
                // ink! attribute macros.
                quote_as_str! {
                    #[ink::chain_extension]
                },
                quote_as_str! {
                    #[ink::contract]
                },
                quote_as_str! {
                    #[ink::storage_item]
                },
                quote_as_str! {
                    #[ink::test]
                },
                quote_as_str! {
                    #[ink::trait_definition]
                },
                // Arguments that should have no value.
                quote_as_str! {
                    #[ink(constructor)]
                },
                quote_as_str! {
                    #[ink(event)]
                },
                quote_as_str! {
                    #[ink(message)]
                },
                quote_as_str! {
                    #[ink(storage)]
                },
                quote_as_str! {
                    #[ink(topic)]
                },
                quote_as_str! {
                    #[ink(impl)]
                },
                // ... See "Compound arguments" section for `anonymous`, `default` and `payable`.
                // Arguments that should have an integer (`u32` to be specific) value.
                quote_as_str! {
                    #[ink(extension=1)]
                },
                quote_as_str! {
                    #[ink(extension=0xA)] // hex encoded.
                },
                quote_as_str! {
                    #[ink(message, selector=1)] // message is required, otherwise this would be incomplete.
                },
                quote_as_str! {
                    #[ink(message, selector=0xA)] // message is required, otherwise this would be incomplete.
                },
                // Arguments that can have a wildcard/underscore value.
                quote_as_str! {
                    #[ink(message, selector=_)] // message is required, otherwise this would be incomplete.
                },
                // Arguments that should have a string value.
                quote_as_str! {
                    #[ink(namespace="my_namespace")]
                },
                quote_as_str! {
                    #[ink::contract(keep_attr="foo,bar")]
                },
                quote_as_str! {
                    #[ink::trait_definition(keep_attr="foo,bar")]
                },
                // Arguments that should have a boolean value.
                quote_as_str! {
                    #[ink(extension=1, handle_status=true)] // `handle_status` is incomplete without `extension`.
                },
                quote_as_str! {
                    #[ink::storage_item(derive=false)]
                },
                // Arguments that should have a path value.
                quote_as_str! {
                    #[ink::contract(env=my::env::Types)]
                },
                // Compound arguments.
                quote_as_str! {
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                },
                quote_as_str! {
                    #[ink(constructor, payable, default, selector=1)]
                },
                quote_as_str! {
                    #[ink(event, anonymous)]
                },
                quote_as_str! {
                    #[ink(extension=1, handle_status=true)]
                },
                quote_as_str! {
                    #[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar")]
                },
                quote_as_str! {
                    #[ink(impl, namespace="my_namespace")]
                },
                quote_as_str! {
                    #[ink(message, payable, default, selector=0xA)]
                },
                quote_as_str! {
                    #[ink(message)]
                    #[ink(payable)]
                    #[ink(default, selector=2)]
                },
            ]
        };
    }

    #[test]
    fn identifiers_not_prefixed_with_ink_works() {
        let file = InkFile::parse(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                #[ink(storage)]
                pub struct MyContract {
                    value: bool,
                }

                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor(init_value: bool) -> Self {
                        Self { value: init_value }
                    }
                }
            }
        });

        let mut results = Vec::new();
        ensure_no_ink_identifiers(&mut results, &file);
        assert!(results.is_empty());
    }

    #[test]
    fn identifiers_prefixed_with_ink_fails() {
        let file = InkFile::parse(quote_as_str! {
            #[ink::contract]
            mod __ink_example {
                #[ink(storage)]
                struct __ink_Example {
                    value: bool,
                }

                impl __ink_Example {
                    #[ink(constructor)]
                    pub fn __ink_new(__ink_init_value: bool) -> Self {
                        Self { value: __ink_init_value }
                    }
                }
            }
        });

        let mut results = Vec::new();
        ensure_no_ink_identifiers(&mut results, &file);
        // There are 6 occurrences of __ink_ prefixed identifiers in the code snippet.
        assert_eq!(results.len(), 6);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            6
        );
    }

    #[test]
    fn known_ink_attributes_works() {
        for code in valid_attributes!() {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_unknown_ink_attributes(&mut results, &attrs);
            assert!(results.is_empty());
        }
    }

    #[test]
    fn unknown_ink_attributes_fails() {
        for code in [
            quote_as_str! {
                #[ink::xyz]
            },
            quote_as_str! {
                #[ink::abc::xyz]
            },
            quote_as_str! {
                #[ink(xyz)]
            },
        ] {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_unknown_ink_attributes(&mut results, &attrs);
            assert_eq!(results.len(), 1);
            assert_eq!(results[0].severity, Severity::Warning);
        }
    }

    #[test]
    fn valid_attribute_argument_format_and_value_type_works() {
        // NOTE: This test only cares about ink! attribute arguments not macros,
        // See `ensure_valid_attribute_arguments` doc.
        for code in valid_attributes!() {
            let attr = parse_first_ink_attr(code);

            let mut results = Vec::new();
            ensure_valid_attribute_arguments(&mut results, &attr);
            assert!(results.is_empty(), "attribute: {code}");
        }
    }

    #[test]
    fn invalid_attribute_argument_format_and_value_type_fails() {
        // NOTE: This test only cares about ink! attribute arguments not macros,
        // See `ensure_valid_attribute_arguments` doc.
        for code in [
            // Arguments that should have no value.
            quote_as_str! {
                #[ink(storage=1)]
            },
            quote_as_str! {
                #[ink(constructor=)]
            },
            quote_as_str! {
                #[ink(default="hello")]
            },
            quote_as_str! {
                #[ink(message=true)]
            },
            quote_as_str! {
                #[ink(event='a')]
            },
            quote_as_str! {
                #[ink(anonymous=0x1)]
            },
            quote_as_str! {
                #[ink(topic=b"")]
            },
            quote_as_str! {
                #[ink(payable=3.2)]
            },
            quote_as_str! {
                #[ink(impl=my::path::item)]
            },
            // Arguments that should have an integer (`u32` to be specific) value.
            quote_as_str! {
                #[ink(selector)]
            },
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L1200-L1211>.
            quote_as_str! {
                #[ink(selector=-1)] // out of range for `u32`
            },
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L1213-L1224>.
            quote_as_str! {
                #[ink(selector=0xFFFF_FFFF_FFFF_FFFF)] // too large for `u32`
            },
            quote_as_str! {
                #[ink(selector="hello")]
            },
            quote_as_str! {
                #[ink(extension='a')]
            },
            quote_as_str! {
                #[ink(extension=false)]
            },
            // Arguments that can have a wildcard/underscore value.
            quote_as_str! {
                #[ink(selector=*)]
            },
            quote_as_str! {
                #[ink(selector="_")] // should be an underscore expression, not a string
            },
            // Arguments that should have a string value.
            quote_as_str! {
                #[ink::contract(keep_attr=my::path::item)]
            },
            quote_as_str! {
                #[ink(namespace=0x1)]
            },
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L1248-L1256>.
            quote_as_str! {
                #[ink(namespace="::invalid_identifier")] // namespace should be a valid identifier.
            },
            // Arguments that should have a boolean value.
            quote_as_str! {
                #[ink(handle_status=1)]
            },
            quote_as_str! {
                #[ink::storage_item(derive)]
            },
            // Arguments that should have a path value.
            quote_as_str! {
                #[ink::contract(env="my::env::Types")]
            },
            quote_as_str! {
                #[ink::contract(env=2.4)]
            },
            // Compound arguments.
            quote_as_str! {
                #[ink::contract(env=my::env::Types, keep_attr)] // Bad keep_attr.
            },
            quote_as_str! {
                #[ink(constructor, payable=1, default, selector=1)] // Bad payable.
            },
            quote_as_str! {
                #[ink(message="hello", payable, selector=2)] // message.
            },
            quote_as_str! {
                #[ink(event=0x1, anonymous)] // bad event.
            },
            quote_as_str! {
                #[ink(extension, handle_status=true)] // bad extension.
            },
        ] {
            let attr = parse_first_ink_attr(code);

            let mut results = Vec::new();
            ensure_valid_attribute_arguments(&mut results, &attr);
            assert_eq!(results.len(), 1, "attribute: {code}");
            assert_eq!(results[0].severity, Severity::Error, "attribute: {code}");
        }
    }

    #[test]
    fn no_duplicate_attributes_and_arguments_works() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        for code in valid_attributes!() {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_duplicate_attributes_and_arguments(&mut results, &attrs);
            assert!(results.is_empty(), "attributes: {code}");
        }
    }

    #[test]
    fn duplicate_attributes_and_arguments_fails() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        for code in [
            quote_as_str! {
                #[ink::contract(env=my::env::Types, keep_attr="foo,bar", keep_attr="hello")] // duplicate `keep_attr`.
            },
            quote_as_str! {
                #[ink::contract(env=my::env::Types)]
                #[ink::contract(keep_attr="foo,bar")] // duplicate `contract`.
            },
            quote_as_str! {
                #[ink::contract]
                #[ink::contract(keep_attr="foo,bar")] // duplicate `contract`.
            },
            quote_as_str! {
                #[ink(constructor, payable, default, selector=1)]
                #[ink(constructor)] // duplicate `constructor`.
            },
            quote_as_str! {
                #[ink(message)]
                #[ink(payable)]
                #[ink(selector=2)]
                #[ink(selector=0xA)] // duplicate `selector`.
            },
        ] {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_duplicate_attributes_and_arguments(&mut results, &attrs);
            assert_eq!(results.len(), 1, "attributes: {code}");
            assert_eq!(results[0].severity, Severity::Error, "attributes: {code}");
        }
    }

    #[test]
    fn no_conflicting_attributes_and_arguments_works() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        for code in valid_attributes!() {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_conflicting_attributes_and_arguments(&mut results, &attrs);
            assert!(results.is_empty(), "attributes: {code}");
        }
    }

    #[test]
    fn conflicting_attributes_and_arguments_fails() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        for code in [
            // Single attributes.
            quote_as_str! {
                #[ink::chain_extension(env=my::env::Types)] // conflicting `env`.
            },
            quote_as_str! {
                #[ink::contract(namespace="my_namespace")] // conflicting `namespace`.
            },
            quote_as_str! {
                #[ink::storage_item(keep_attr="foo,bar")] // conflicting `keep_attr`.
            },
            quote_as_str! {
                #[ink::test(payable)] // conflicting `payable`.
            },
            quote_as_str! {
                #[ink::trait_definition(derive=false)] // conflicting `derive`.
            },
            quote_as_str! {
                #[ink(storage, anonymous)] // conflicting `anonymous`.
            },
            quote_as_str! {
                #[ink(event, default)] // conflicting `default`.
            },
            quote_as_str! {
                #[ink(topic, selector=1)] // conflicting `selector`.
            },
            quote_as_str! {
                #[ink(constructor, derive=true)] // conflicting `derive`.
            },
            quote_as_str! {
                #[ink(message, namespace="my_namespace")] // conflicting `namespace`.
            },
            quote_as_str! {
                #[ink(extension=1, env=my::env::Types)] // conflicting `env`.
            },
            // Multiple attributes.
            quote_as_str! {
                #[ink::contract(env=my::env::Types)]
                #[ink::trait_definition(namespace="my_namespace")] // conflicts with `contract`.
            },
            quote_as_str! {
                #[ink::contract]
                #[ink(message)] // conflicts with `contract`.
            },
            quote_as_str! {
                #[ink(constructor, payable, default, selector=1)]
                #[ink(event)] // conflicts with `constructor`.
            },
            quote_as_str! {
                #[ink(message)]
                #[ink(payable, selector=2, topic)] // `topic` conflicts with `message`.
            },
            // Wrong order of attributes and/or arguments.
            quote_as_str! {
                #[ink(anonymous, event)] // `event` should come first.
            },
            quote_as_str! {
                #[ink(anonymous)] // `event` should come first.
                #[ink(event)]
            },
            quote_as_str! {
                #[ink(handle_status=true, extension=1)] // `extension` should come first.
            },
            quote_as_str! {
                #[ink(handle_status=true)] // `extension` should come first.
                #[ink(extension=1)]
            },
            quote_as_str! {
                #[ink(payable, message, default, selector=1)] // `message` should come first.
            },
            quote_as_str! {
                #[ink(payable, default, selector=1)] // `message` should come first.
                #[ink(message)]
            },
            // Macro arguments as standalone attributes.
            quote_as_str! {
                #[ink::contract]
                #[ink(env=my::env::Types)] // conflicts with `contract`, should be an argument.
            },
            quote_as_str! {
                #[ink::trait_definition]
                #[ink(keep_attr="foo,bar")] // conflicts with `trait_definition`, should be an argument.
            },
            quote_as_str! {
                #[ink::storage_item]
                #[ink(derive=false)] // conflicts with `storage_item`, should be an argument.
            },
            // Incomplete and/or ambiguous.
            quote_as_str! {
                #[ink(anonymous)] // missing `event`.
            },
            quote_as_str! {
                #[ink(handle_status=true)] // missing `extension`.
            },
            quote_as_str! {
                #[ink(payable, default, selector=1)] // incomplete and ambiguous.
            },
            quote_as_str! {
                #[ink(payable, default)] // incomplete and ambiguous.
                #[ink(selector=1)]
            },
            quote_as_str! {
                #[ink(keep_attr="foo,bar")] // incomplete and ambiguous.
            },
            // Namespace :-).
            quote_as_str! {
                #[ink(namespace="my_namespace")]
                #[ink::trait_definition] // `trait_definition` should come first.
            },
            quote_as_str! {
                #[ink::trait_definition]
                #[ink(namespace="my_namespace")] // conflicts with `trait_definition`, it should be an argument.
            },
            quote_as_str! {
                #[ink(namespace="my_namespace")]
                #[ink(impl)] // `impl` should come first.
            },
        ] {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_conflicting_attributes_and_arguments(&mut results, &attrs);
            assert_eq!(results.len(), 1, "attributes: {code}");
            assert_eq!(results[0].severity, Severity::Error, "attributes: {code}");
        }
    }
}
