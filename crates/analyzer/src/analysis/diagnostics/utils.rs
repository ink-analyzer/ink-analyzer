//! Utilities for ink! diagnostics.

use ink_analyzer_ir::ast::{
    AstNode, AstToken, HasAttrs, HasGenericParams, HasName, HasTypeBounds, HasVisibility,
};
use ink_analyzer_ir::meta::MetaValue;
use ink_analyzer_ir::syntax::{SourceFile, SyntaxKind, SyntaxNode, SyntaxToken, TextRange};
use ink_analyzer_ir::{
    ast, Contract, HasInkImplParent, InkArg, InkArgKind, InkArgValueKind, InkArgValueStringKind,
    InkAttribute, InkAttributeKind, InkEntity, InkMacroKind, IsInkFn, IsInkStruct, IsInkTrait,
};
use itertools::Itertools;
use std::collections::HashSet;

use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils;
use crate::{resolution, Action, ActionKind, Diagnostic, Severity, Version};

/// Runs generic diagnostics that apply to all ink! entities.
/// (e.g `ensure_no_unknown_ink_attributes`, `ensure_no_ink_identifiers`,
/// `ensure_no_duplicate_attributes_and_arguments`, `ensure_valid_attribute_arguments`).
pub fn run_generic_diagnostics<T: InkEntity>(
    results: &mut Vec<Diagnostic>,
    item: &T,
    version: Version,
) {
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

    // Ensures that ink! attribute arguments are of the right format
    // and have values are of the correct type (if any),
    // See `ensure_valid_attribute_arguments` doc.
    for attr in item.tree().ink_attrs_in_scope() {
        ensure_valid_attribute_arguments(results, &attr, version);
    }

    // Iterates over all ink! parent nodes in scope.
    for ink_parent in item
        .tree()
        .ink_attrs_in_scope()
        .filter_map(|attr| attr.syntax().parent())
        .unique_by(SyntaxNode::text_range)
    {
        // Gets all ink! attributes for the parent node.
        let attrs: Vec<InkAttribute> = ink_analyzer_ir::ink_attrs(&ink_parent).collect();

        if !attrs.is_empty() {
            // Ensures that no duplicate ink! attributes and/or arguments,
            // see `ensure_no_duplicate_attributes_and_arguments` doc.
            ensure_no_duplicate_attributes_and_arguments(results, &attrs);

            // Ensures that no conflicting ink! attributes and/or arguments,
            // see `ensure_no_conflicting_attributes_and_arguments` doc.
            ensure_no_conflicting_attributes_and_arguments(results, &attrs, version);
        }
    }
}

/// Returns an error diagnostic for every instance of `__ink_` prefixed identifier found.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/idents_lint.rs#L20>.
fn ensure_no_ink_identifiers<T: InkEntity>(results: &mut Vec<Diagnostic>, item: &T) {
    for elem in item.syntax().descendants_with_tokens() {
        if let Some(ident) = elem.into_token().and_then(ast::Ident::cast) {
            let name = ident.to_string();
            if name.starts_with("__ink_") {
                // Suggested name for quickfix.
                let suggested_name = name.replace("__ink_", "");
                results.push(Diagnostic {
                    message: format!("Invalid identifier starting with __ink_: {}", ident.text()),
                    range: ident.syntax().text_range(),
                    severity: Severity::Error,
                    quickfixes: (!suggested_name.is_empty()).then(|| {
                        vec![Action {
                            label: format!("Rename identifier to `{suggested_name}`"),
                            kind: ActionKind::QuickFix,
                            range: ident.syntax().text_range(),
                            edits: vec![TextEdit::replace_with_snippet(
                                suggested_name.clone(),
                                ident.syntax().text_range(),
                                Some(format!("${{1:{suggested_name}}}")),
                            )],
                        }]
                    }),
                });
            }
        }
    }
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
        if matches!(
            attr.kind(),
            InkAttributeKind::Macro(InkMacroKind::Unknown)
                | InkAttributeKind::Arg(InkArgKind::Unknown)
        ) {
            results.push(Diagnostic {
                message: format!("Unknown ink! attribute: `{}`", attr.syntax()),
                range: attr
                    .ink_macro()
                    .map(|ink_path| ink_path.syntax().text_range())
                    .or(attr
                        .ink_arg_name()
                        .map(|ink_arg| ink_arg.syntax().text_range()))
                    .unwrap_or(attr.syntax().text_range()),
                // warning because it's possible ink! analyzer is just outdated.
                severity: Severity::Warning,
                quickfixes: Some(vec![Action::remove_attribute(attr)]),
            });
        }
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
fn ensure_valid_attribute_arguments(
    results: &mut Vec<Diagnostic>,
    attr: &InkAttribute,
    version: Version,
) {
    for arg in attr.args() {
        let arg_name_text = arg.meta().name().to_string();
        match arg.kind() {
            // Handle unknown argument.
            InkArgKind::Unknown => {
                // Edit range for quickfix.
                let range = utils::ink_arg_and_delimiter_removal_range(arg, Some(attr));
                results.push(Diagnostic {
                    message: if arg_name_text.is_empty() {
                        "Missing ink! attribute argument.".to_owned()
                    } else {
                        format!("Unknown ink! attribute argument: '{arg_name_text}'.")
                    },
                    range: arg.text_range(),
                    severity: if arg_name_text.is_empty() {
                        // error for missing.
                        Severity::Error
                    } else {
                        // warning because it's possible ink! analyzer is just outdated.
                        Severity::Warning
                    },
                    quickfixes: Some(vec![Action {
                        label: format!(
                            "Remove unknown ink! attribute argument: '{arg_name_text}'."
                        ),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::delete(range)],
                    }]),
                });
            }
            arg_kind => {
                let arg_value_type = if version == Version::V5 {
                    InkArgValueKind::from_v5(*arg_kind)
                } else {
                    InkArgValueKind::from(*arg_kind)
                };
                match arg_value_type {
                    // Arguments that must have no value.
                    InkArgValueKind::None => {
                        if arg.meta().eq().is_some() || arg.meta().value().is_some() {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument shouldn't have a value."
                                ),
                                range: arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!("Remove `{arg_name_text}` argument value"),
                                    kind: ActionKind::QuickFix,
                                    range: arg.text_range(),
                                    edits: vec![TextEdit::replace(arg_name_text, arg.text_range())],
                                }]),
                            });
                        }
                    }
                    // Arguments that should have an integer (`u16` or `u32` to be specific) value.
                    // `u16` values (i.e. ink! v5 chain extension and extension method ids).
                    InkArgValueKind::U16 => {
                        if arg.value().and_then(MetaValue::as_u16).is_none() {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument should have an `integer` (`u16`) value.",
                                ),
                                range: arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!("Add `{arg_name_text}` argument value"),
                                    kind: ActionKind::QuickFix,
                                    range: arg.text_range(),
                                    edits: vec![TextEdit::replace_with_snippet(
                                        format!("{arg_name_text} = 1"),
                                        arg.text_range(),
                                        Some(format!("{arg_name_text} = ${{1:1}}")),
                                    )],
                                }]),
                            });
                        }
                    }
                    // `u32` values and wildcards (i.e `_` and `@` for selectors).
                    InkArgValueKind::U32
                    | InkArgValueKind::U32OrWildcard
                    | InkArgValueKind::U32OrWildcardOrComplement => {
                        let can_be_wildcard = matches!(
                            arg_value_type,
                            InkArgValueKind::U32OrWildcard
                                | InkArgValueKind::U32OrWildcardOrComplement
                        );
                        let can_be_wildcard_complement =
                            arg_value_type == InkArgValueKind::U32OrWildcardOrComplement;
                        let is_u32_or_valid_wildcard = arg.value().is_some_and(|value| {
                            // Ensures that the meta value is either:
                            // - a decimal or hex encoded `u32`.
                            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L903-L910>.
                            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L938-L943>.
                            // - a wildcard/underscore (`_`) symbol (i.e. for selectors).
                            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L884-L900>.
                            // - a wildcard complement/at (`@`) symbol (i.e. for message selectors).
                            // Ref: <https://github.com/paritytech/ink/pull/1708>.
                            value.as_u32().is_some()
                                || (can_be_wildcard && value.is_wildcard())
                                || (can_be_wildcard_complement && value.is_at_symbol())
                        });
                        if !is_u32_or_valid_wildcard {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument should have an `integer` (`u32`) {} value.",
                                    if can_be_wildcard_complement {
                                        ", wildcard/underscore (`_`) or wildcard complement (`@`) symbol"
                                    } else if can_be_wildcard {
                                        "or wildcard/underscore (`_`)"
                                    } else {
                                        ""
                                    }
                                ),
                                range: arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!("Add `{arg_name_text}` argument value"),
                                    kind: ActionKind::QuickFix,
                                    range: arg.text_range(),
                                    edits: vec![TextEdit::replace_with_snippet(
                                        format!("{arg_name_text} = 1"),
                                        arg.text_range(),
                                        Some(format!("{arg_name_text} = ${{1:1}}")),
                                    )],
                                }]),
                            });
                        }
                    }
                    // Arguments that should have a string value.
                    InkArgValueKind::String(str_kind) => {
                        let is_valid_string = arg.value().is_some_and(|value| {
                            // For namespace arguments, ensure the meta value is a valid Rust identifier.
                            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L922-L926>.
                            value.as_string().is_some()
                                && (str_kind != InkArgValueStringKind::Identifier
                                    || value
                                        .as_string()
                                        .and_then(|value| parse_ident(value.as_str()))
                                        .is_some())
                        });
                        if !is_valid_string {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument should have a {} `string` (`&str`) value.",
                                    if *arg.kind() == InkArgKind::KeepAttr {
                                        "comma separated"
                                    } else {
                                        ""
                                    }
                                ),
                                range: arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!("Add `{arg_name_text}` argument value"),
                                    kind: ActionKind::QuickFix,
                                    range: arg.text_range(),
                                    edits: vec![TextEdit::replace_with_snippet(
                                        format!(
                                            r#"{arg_name_text} = "{}""#,
                                            if str_kind == InkArgValueStringKind::Identifier {
                                                "my_namespace"
                                            } else {
                                                ""
                                            }
                                        ),
                                        arg.text_range(),
                                        Some(format!(
                                            r#"{arg_name_text} = "{}""#,
                                            if str_kind == InkArgValueStringKind::Identifier {
                                                "${1:my_namespace}"
                                            } else {
                                                "$1"
                                            }
                                        )),
                                    )],
                                }]),
                            });
                        }
                    }
                    // Arguments that should have a boolean value.
                    InkArgValueKind::Bool => {
                        if arg.value().and_then(MetaValue::as_bool).is_none() {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument should have a `boolean` (`bool`) value."
                                ),
                                range: arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!("Add `{arg_name_text}` argument value"),
                                    kind: ActionKind::QuickFix,
                                    range: arg.text_range(),
                                    edits: vec![TextEdit::replace_with_snippet(
                                        format!("{arg_name_text} = true"),
                                        arg.text_range(),
                                        Some(format!("{arg_name_text} = ${{1:true}}")),
                                    )],
                                }]),
                            });
                        }
                    }
                    // Arguments that should have a path value.
                    InkArgValueKind::Path(_) => {
                        let is_path = arg
                            .value()
                            .filter(|value| value.kind() == SyntaxKind::PATH)
                            .is_some();
                        if !is_path {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{arg_name_text}` argument should have a `path` (e.g `my::env::Types`) value."
                                ),
                                range: arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!("Add `{arg_name_text}` argument value"),
                                    kind: ActionKind::QuickFix,
                                    range: arg.text_range(),
                                    edits: vec![TextEdit::replace_with_snippet(
                                        format!("{arg_name_text} = crate::"),
                                        arg.text_range(),
                                        Some(format!("{arg_name_text} = ${{1:crate::}}")),
                                    )],
                                }]),
                            });
                        }
                    }
                    // Nested arguments.
                    // TODO: Implement validation for nested attributes for ink! v5.
                    InkArgValueKind::Arg(_) => (),
                    InkArgValueKind::Choice(_, _) => (),
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
                    message: format!("Duplicate ink! attribute macro: `{}`", attr.syntax()),
                    range: attr.syntax().text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action::remove_attribute(attr)]),
                });
            }
            seen_macros.insert(macro_kind);
        }

        for arg in attr.args() {
            let arg_kind = arg.kind();
            // Unknown ink! attribute arguments are ignored.
            if *arg_kind != InkArgKind::Unknown && seen_args.get(arg_kind).is_some() {
                // Edit range for quickfix.
                let range = utils::ink_arg_and_delimiter_removal_range(arg, Some(attr));
                results.push(Diagnostic {
                    message: format!("Duplicate ink! attribute argument: `{}`", arg.meta().name()),
                    range: arg.text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action {
                        label: format!("Remove ink! `{}` attribute argument.", arg.meta().name()),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::delete(range)],
                    }]),
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
    version: Version,
) {
    // We can only move forward if there is at least one "valid" attribute.
    // We get the primary ink! attribute candidate (if any) that other attributes and arguments shouldn't conflict with.
    if let Some((primary_ink_attr_candidate, primary_candidate_first)) =
        utils::primary_ink_attribute_candidate(attrs.iter().cloned())
    {
        // sibling arguments are arguments that don't conflict with the primary attribute's kind,
        // see `utils::valid_sibling_ink_args` doc.
        let valid_sibling_args =
            utils::valid_sibling_ink_args(*primary_ink_attr_candidate.kind(), version);

        // We want to suggest a primary attribute in case the current one is either incomplete or ambiguous
        // (See [`utils::primary_ink_attribute_kind_suggestions`] doc).
        let primary_attribute_kind_suggestions =
            utils::primary_ink_attribute_kind_suggestions(*primary_ink_attr_candidate.kind());

        // Determines the insertion offset for creating a valid "primary" attribute as the first attribute.
        let primary_attr_insert_offset_option = || {
            ink_analyzer_ir::parent_ast_item(primary_ink_attr_candidate.syntax())
                .as_ref()
                // Determines the insertion offset for the quickfix.
                .map(ast::Item::syntax)
                .map(utils::first_ink_attribute_insert_offset)
                // Defaults to inserting before the first ink! attribute if the passed list of attributes.
                .or(attrs.first().map(|it| it.syntax().text_range().start()))
        };

        // If the primary ink! attribute candidate is complete and unambiguous,
        // but isn't the first attribute, then suggest that it be made the first attribute.
        if !primary_candidate_first && primary_attribute_kind_suggestions.is_empty() {
            results.push(Diagnostic {
                message: format!(
                    "`{}` should be the first ink! attribute for this item.",
                    primary_ink_attr_candidate.syntax(),
                ),
                range: primary_ink_attr_candidate.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: primary_attr_insert_offset_option().map(|insert_offset| {
                    vec![Action::move_item(
                        primary_ink_attr_candidate.syntax(),
                        insert_offset,
                        format!(
                            "Make `{}` the first ink! attribute for this item.",
                            primary_ink_attr_candidate.syntax(),
                        ),
                        None,
                    )]
                }),
            });
        }

        // For `namespace`, additional context is required to determine what do with
        // the primary attribute kind suggestions, because while namespace can be ambiguous,
        // it's also valid on its own. See its match pattern in
        // the [`utils::valid_sibling_ink_args`] function for the rationale and references.
        let is_namespace =
            *primary_ink_attr_candidate.kind() == InkAttributeKind::Arg(InkArgKind::Namespace);

        // If the primary ink! attribute candidate is complete and unambiguous (or it's `namespace` :-)),
        // and it's ink! attribute kind is an ink! attribute argument kind,
        // make sure that ink! attribute argument is also the first in the argument list.
        if primary_attribute_kind_suggestions.is_empty() || is_namespace {
            if let InkAttributeKind::Arg(arg_kind) = primary_ink_attr_candidate.kind() {
                let is_primary_arg_first =
                    if let Some(first_arg) = primary_ink_attr_candidate.args().first() {
                        first_arg.kind() == arg_kind
                    } else {
                        false
                    };
                if !is_primary_arg_first {
                    // Find the primary arg.
                    let primary_arg = primary_ink_attr_candidate
                        .args()
                        .iter()
                        .find(|arg| arg.kind() == arg_kind);
                    // Suggest that it should become the first argument.
                    results.push(Diagnostic {
                        message: format!(
                            "`{arg_kind}` should be the first argument for this ink! attribute: {}.",
                            primary_ink_attr_candidate.syntax()
                        ),
                        range: if let Some(arg) = primary_arg {
                            arg.text_range()
                        } else {
                            primary_ink_attr_candidate.syntax().text_range()
                        },
                        severity: Severity::Error,
                        quickfixes: primary_arg.and_then(|arg| {
                            // Determines the insertion offset and affixes for the quickfix.
                            utils::first_ink_arg_insert_offset_and_affixes(
                                &primary_ink_attr_candidate,
                            )
                            .map(
                                |(insert_offset, insert_prefix, insert_suffix)| {
                                    // Edit range for quickfix.
                                    let range = utils::ink_arg_and_delimiter_removal_range(
                                        arg,
                                        Some(&primary_ink_attr_candidate),
                                    );
                                    vec![Action {
                                        label: format!(
                                            "Make `{arg_kind}` the first argument for this ink! attribute.",
                                        ),
                                        kind: ActionKind::QuickFix,
                                        range,
                                        edits: vec![
                                            // Insert a copy of the item at the specified offset.
                                            TextEdit::insert(
                                                format!(
                                                    "{}{arg}{}",
                                                    insert_prefix.unwrap_or_default(),
                                                    insert_suffix.unwrap_or_default()
                                                ),
                                                insert_offset,
                                            ),
                                            // Delete the item from current location.
                                            TextEdit::delete(
                                                utils::ink_arg_and_delimiter_removal_range(
                                                    arg,
                                                    Some(&primary_ink_attr_candidate),
                                                ),
                                            ),
                                        ],
                                    }]
                                },
                            )
                        }),
                    });
                }
            }
        }

        // Suggests possible primary ink! attributes if the primary candidate is either incomplete or ambiguous or both
        // and it's also not `namespace` (which is valid on it's own).
        if !primary_attribute_kind_suggestions.is_empty() && !is_namespace {
            // Quickfix for adding a ink! attribute of the given kind as the primary attribute (if possible).
            let add_primary_ink_attribute = |attr_kind: &InkAttributeKind| {
                primary_attr_insert_offset_option().map(|insert_offset| {
                    let (insert_text, attr_desc, snippet) = match attr_kind {
                        InkAttributeKind::Arg(arg_kind) => {
                            let (edit, snippet) = utils::ink_arg_insert_text(*arg_kind, None, None);
                            (
                                format!("#[ink({edit})]"),
                                format!("ink! `{arg_kind}`"),
                                snippet.map(|snippet| format!("#[ink({snippet})]")),
                            )
                        }
                        InkAttributeKind::Macro(macro_kind) => (
                            format!("#[{}]", macro_kind.path_as_str()),
                            format!("ink! `{macro_kind}`"),
                            None,
                        ),
                    };
                    Action {
                        label: format!(
                            "Add an {attr_desc} as the first ink! attribute for this item."
                        ),
                        kind: ActionKind::QuickFix,
                        range: primary_ink_attr_candidate.syntax().text_range(),
                        edits: vec![TextEdit::insert_with_snippet(
                            insert_text,
                            insert_offset,
                            snippet,
                        )],
                    }
                })
            };

            // Computes possible quickfixes.
            let mut possible_quickfixes =
                primary_attribute_kind_suggestions
                    .iter()
                    .filter_map(|attr_kind| {
                        match attr_kind {
                            InkAttributeKind::Arg(arg_kind) => {
                                let (edit, snippet) =
                                    utils::ink_arg_insert_text(*arg_kind, None, None);
                                match utils::first_ink_arg_insert_offset_and_affixes(
                                    &primary_ink_attr_candidate,
                                ) {
                                    // Adds suggested primary ink! attribute argument as the first argument for the attribute.
                                    Some((insert_offset, prefix, suffix)) => Some(Action {
                                        label: format!(
                                            "Add an ink! `{arg_kind}` as the first argument for the `{}` attribute.",
                                            primary_ink_attr_candidate.syntax()
                                        ),
                                        kind: ActionKind::QuickFix,
                                        range: primary_ink_attr_candidate.syntax().text_range(),
                                        edits: vec![TextEdit::insert_with_snippet(
                                            format!(
                                                "{}{edit}{}",
                                                prefix.unwrap_or_default(),
                                                suffix.unwrap_or_default()
                                            ),
                                            insert_offset,
                                            snippet.as_ref().map(|snippet| {
                                                format!(
                                                    "{}{snippet}{}",
                                                    prefix.unwrap_or_default(),
                                                    suffix.unwrap_or_default()
                                                )
                                            }),
                                        )],
                                    }),
                                    // Defaults to adding the suggested ink! attribute argument as the first attribute.
                                    None => add_primary_ink_attribute(attr_kind),
                                }
                            }
                            InkAttributeKind::Macro(macro_kind) => {
                                match primary_ink_attr_candidate.kind() {
                                    // Adds the suggested ink! attribute macro to the existing ink! attribute arguments.
                                    InkAttributeKind::Arg(_) => Some(Action {
                                        label: format!(
                                            "Add an ink! {macro_kind} macro to the `{}` attribute.",
                                            primary_ink_attr_candidate.syntax()
                                        ),
                                        kind: ActionKind::QuickFix,
                                        range: primary_ink_attr_candidate.syntax().text_range(),
                                        edits: vec![TextEdit::replace(
                                            format!(
                                                "#[{}({})]",
                                                macro_kind.path_as_str(),
                                                primary_ink_attr_candidate
                                                    .args()
                                                    .iter()
                                                    .map(ToString::to_string)
                                                    .join(", ")
                                            ),
                                            primary_ink_attr_candidate.syntax().text_range(),
                                        )],
                                    }),
                                    // Adds the suggested ink! attribute macro as the first attribute.
                                    InkAttributeKind::Macro(_) => {
                                        add_primary_ink_attribute(attr_kind)
                                    }
                                }
                            }
                        }
                    });

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
                        .join(" or "), // It's never more than 2 suggestions at the moment.
                ),
                range: primary_ink_attr_candidate.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: possible_quickfixes
                    .next()
                    .map(|quickfix| [quickfix].into_iter().chain(possible_quickfixes).collect()),
            });
        }

        for (idx, attr) in attrs.iter().enumerate() {
            // Check for attribute kind level conflict.
            let is_conflicting_attribute =
                if *attr == primary_ink_attr_candidate || (idx == 0 && !primary_candidate_first) {
                    // Primary attribute can't conflict with itself
                    // and we ignore any conflict errors with the first attribute if it's not the primary attribute
                    // in favor of placing the error on the primary attribute candidate which was already done earlier.
                    false
                } else {
                    match primary_ink_attr_candidate.kind() {
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
                        "ink! attribute `{}` conflicts with the {} ink! attribute `{}` for this item.",
                        attr.syntax(),
                        if primary_candidate_first {
                            "first"
                        } else {
                            "primary"
                        },
                        primary_ink_attr_candidate.syntax()
                    ),
                    range: attr.syntax().text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![
                        match (primary_ink_attr_candidate.kind(), attr.kind()) {
                            // Removes only conflicting arguments (or entire attribute if necessary).
                            (InkAttributeKind::Arg(_), InkAttributeKind::Arg(_)) => {
                                let conflicting_args: Vec<&InkArg> = attr
                                    .args()
                                    .iter()
                                    .filter(|arg| !valid_sibling_args.contains(arg.kind()))
                                    .collect();
                                if conflicting_args.len() == attr.args().len() {
                                    Action::remove_attribute(attr)
                                } else {
                                    Action {
                                        label: format!(
                                            "Remove conflicting ink! attribute arguments: `{}`",
                                            conflicting_args
                                                .iter()
                                                .map(ToString::to_string)
                                                .join(", ")
                                        ),
                                        kind: ActionKind::QuickFix,
                                        range: attr.syntax().text_range(),
                                        edits: conflicting_args
                                            .iter()
                                            .map(|arg| {
                                                TextEdit::delete(
                                                    utils::ink_arg_and_delimiter_removal_range(
                                                        arg,
                                                        Some(attr),
                                                    ),
                                                )
                                            })
                                            .collect(),
                                    }
                                }
                            }
                            // Otherwise remove entire attribute.
                            _ => Action::remove_attribute(attr),
                        },
                    ]),
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
                        // Edit range for quickfix.
                        let range = utils::ink_arg_and_delimiter_removal_range(arg, Some(attr));
                        results.push(Diagnostic {
                            message: format!(
                                "ink! attribute argument `{}` conflicts with the {} for this item.",
                                arg.meta().name(),
                                if *attr == primary_ink_attr_candidate {
                                    match primary_ink_attr_candidate.kind() {
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
                                        primary_ink_attr_candidate.syntax()
                                    )
                                }
                            ),
                            range: arg.text_range(),
                            severity: Severity::Error,
                            quickfixes: Some(vec![Action {
                                label: format!(
                                    "Remove ink! `{}` attribute argument.",
                                    arg.meta().name()
                                ),
                                kind: ActionKind::QuickFix,
                                range,
                                edits: vec![TextEdit::delete(range)],
                            }]),
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
pub fn ensure_exactly_one_item<T>(
    results: &mut Vec<Diagnostic>,
    items: &[T],
    empty_diagnostic: Diagnostic,
    error_too_many: &str,
    severity_too_many: Severity,
) where
    T: InkEntity,
{
    if items.is_empty() {
        results.push(empty_diagnostic);
    } else {
        ensure_at_most_one_item(results, items, error_too_many, severity_too_many);
    }
}

/// Ensures that there are not multiple definitions of an item.
pub fn ensure_at_most_one_item<T>(
    results: &mut Vec<Diagnostic>,
    items: &[T],
    message: &str,
    severity: Severity,
) where
    T: InkEntity,
{
    if items.len() > 1 {
        for item in &items[1..] {
            results.push(Diagnostic {
                message: message.to_owned(),
                range: item.syntax().text_range(),
                severity,
                quickfixes: Some(
                    item.ink_attr()
                        .map(|attr| {
                            vec![
                                Action::remove_attribute(attr),
                                Action::remove_item(item.syntax()),
                            ]
                        })
                        .unwrap_or_else(|| vec![Action::remove_item(item.syntax())]),
                ),
            });
        }
    }
}

/// Ensures that ink! entity is a `struct` with `pub` visibility.
pub fn ensure_pub_struct<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: IsInkStruct,
{
    match item.struct_item() {
        Some(struct_item) => {
            let (has_pub_visibility, visibility) = match struct_item.visibility() {
                Some(visibility) => (visibility.to_string() == "pub", Some(visibility)),
                None => (false, None),
            };

            (!has_pub_visibility).then(|| Diagnostic {
                message: format!("ink! {ink_scope_name} must have `pub` visibility.",),
                range: visibility
                    .as_ref()
                    .map(AstNode::syntax)
                    .unwrap_or_else(|| item.syntax())
                    .text_range(),
                severity: Severity::Error,
                quickfixes: visibility
                    .as_ref()
                    .map(|vis| vis.syntax().text_range())
                    .or(struct_item
                        .struct_token()
                        .map(|it| TextRange::new(it.text_range().start(), it.text_range().start())))
                    .map(|range| {
                        vec![Action {
                            label: "Change visibility to `pub`.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range,
                            edits: vec![TextEdit::replace(
                                format!("pub{}", if visibility.is_none() { " " } else { "" }),
                                range,
                            )],
                        }]
                    }),
            })
        }
        None => Some(Diagnostic {
            message: format!("ink! {ink_scope_name} must be a `struct` item.",),
            range: item.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: item
                .ink_attr()
                .map(|attr| vec![Action::remove_attribute(attr)]),
        }),
    }
}

/// Ensures that ink! entity is an `fn` item.
pub fn ensure_fn<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: IsInkFn,
{
    item.fn_item().is_none().then(|| Diagnostic {
        message: format!("ink! {ink_scope_name} must be an `fn` item.",),
        range: item.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: item
            .ink_attr()
            .map(|attr| vec![Action::remove_attribute(attr)]),
    })
}

/// Ensures that ink! entity is a `trait` item.
pub fn ensure_trait<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: IsInkTrait,
{
    item.trait_item().is_none().then(|| Diagnostic {
        message: format!("ink! {ink_scope_name} must be a `trait` item.",),
        range: item.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: item
            .ink_attr()
            .map(|attr| vec![Action::remove_attribute(attr)]),
    })
}

/// Ensures that an `fn` item has no self receiver (i.e no `&self`, `&mut self`, self or mut self).
pub fn ensure_no_self_receiver(fn_item: &ast::Fn, ink_scope_name: &str) -> Option<Diagnostic> {
    fn_item.param_list()?.self_param().map(|self_param| {
        // Edit range for quickfix.
        let range = utils::node_and_delimiter_range(self_param.syntax(), SyntaxKind::COMMA);
        Diagnostic {
            message: format!("ink! {ink_scope_name} must not have a self receiver (i.e no `&self`, `&mut self`, self or mut self)."),
            range: self_param.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove self receiver.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
        }
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
        quickfixes: Some(vec![Action {
            label: "Remove generic parameters.".to_owned(),
            kind: ActionKind::QuickFix,
            range: generics.syntax().text_range(),
            edits: vec![TextEdit::delete(generics.syntax().text_range())],
        }]),
    })
}

/// Ensures that item has no trait bounds.
pub fn ensure_no_trait_bounds<T>(item: &T, message: &str) -> Option<Diagnostic>
where
    T: HasTypeBounds,
{
    item.type_bound_list().map(|type_bound_list| {
        // Determines the range of the type bound definition.
        let range = TextRange::new(
            item.colon_token()
                .as_ref()
                .map(SyntaxToken::text_range)
                .unwrap_or_else(|| type_bound_list.syntax().text_range())
                .start(),
            type_bound_list.syntax().text_range().end(),
        );
        Diagnostic {
            message: message.to_owned(),
            range,
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove type bounds.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
        }
    })
}

/// Ensures that `fn` item satisfies all common invariants of function and method-based ink! entities
/// (i.e `constructor`s, `message`s and `extension`s).
///
/// See reference below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/callable.rs#L355-L440>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L395-L465>.
pub fn ensure_fn_invariants(
    results: &mut Vec<Diagnostic>,
    fn_item: &ast::Fn,
    ink_scope_name: &str,
) {
    if let Some(diagnostic) = ensure_no_generics(fn_item, ink_scope_name) {
        results.push(diagnostic);
    }

    if let Some(const_token) = fn_item.const_token() {
        // Edit range for quickfix.
        let range = utils::token_and_trivia_range(&const_token);
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `const`."),
            range: const_token.text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove `const` keyword.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
        });
    }

    if let Some(async_token) = fn_item.async_token() {
        // Edit range for quickfix.
        let range = utils::token_and_trivia_range(&async_token);
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `async`."),
            range: async_token.text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove `async` keyword.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
        });
    }

    if let Some(unsafe_token) = fn_item.unsafe_token() {
        // Edit range for quickfix.
        let range = utils::token_and_trivia_range(&unsafe_token);
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `unsafe`."),
            range: unsafe_token.text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove `unsafe` keyword.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
        });
    }

    if let Some(abi) = fn_item.abi() {
        // Edit range for quickfix.
        let range = utils::node_and_trivia_range(abi.syntax());
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not have explicit ABI."),
            range: abi.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove explicit ABI.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
        });
    }

    if let Some(param_list) = fn_item.param_list() {
        results.append(
            &mut param_list
                .params()
                .filter_map(|param| {
                    param.dotdotdot_token().map(|dotdotdot| {
                        // Edit range for quickfix.
                        let range = utils::token_and_delimiter_range(&dotdotdot, SyntaxKind::COMMA);
                        Diagnostic {
                            message: format!("ink! {ink_scope_name} must not be variadic."),
                            range: dotdotdot.text_range(),
                            severity: Severity::Error,
                            quickfixes: Some(vec![Action {
                                label: "Make function un-variadic.".to_owned(),
                                kind: ActionKind::QuickFix,
                                range,
                                edits: vec![TextEdit::delete(range)],
                            }]),
                        }
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
            range: visibility
                .as_ref()
                .map(AstNode::syntax)
                .unwrap_or_else(|| fn_item.syntax())
                .text_range(),
            severity: Severity::Error,
            quickfixes: visibility
                .as_ref()
                .map(|vis| vis.syntax().text_range())
                .or(fn_item
                    .default_token()
                    .or(fn_item.const_token())
                    .or(fn_item.async_token())
                    .or(fn_item.unsafe_token())
                    .or(fn_item.abi().and_then(|abi| abi.syntax().first_token()))
                    .or(fn_item.fn_token())
                    .map(|it| TextRange::new(it.text_range().start(), it.text_range().start())))
                .map(|range| {
                    let remove_range = visibility
                        .as_ref()
                        .map_or(range, |vis| utils::node_and_trivia_range(vis.syntax()));
                    vec![
                        Action {
                            label: "Change visibility to `pub`.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range,
                            edits: vec![TextEdit::replace(
                                format!("pub{}", if visibility.is_none() { " " } else { "" }),
                                range,
                            )],
                        },
                        Action {
                            label: "Remove visibility.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range: remove_range,
                            edits: vec![TextEdit::delete(remove_range)],
                        },
                    ]
                }),
        });
    }

    // See `ensure_fn_invariants` doc.
    ensure_fn_invariants(results, fn_item, ink_scope_name);
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
        // Edit range for quickfix.
        let range = utils::token_and_trivia_range(&unsafe_token);
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `unsafe`."),
            range: unsafe_token.text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove `unsafe` keyword.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
        });
    }

    if let Some(auto_token) = trait_item.auto_token() {
        // Edit range for quickfix.
        let range = utils::token_and_trivia_range(&auto_token);
        results.push(Diagnostic {
            message: format!("ink! {ink_scope_name} must not be `auto` implemented."),
            range: auto_token.text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove `auto` keyword.".to_owned(),
                kind: ActionKind::QuickFix,
                range,
                edits: vec![TextEdit::delete(range)],
            }]),
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
            range: visibility
                .as_ref()
                .map(AstNode::syntax)
                .unwrap_or_else(|| trait_item.syntax())
                .text_range(),
            severity: Severity::Error,
            quickfixes: visibility
                .as_ref()
                .map(|vis| vis.syntax().text_range())
                .or(trait_item
                    .unsafe_token()
                    .or(trait_item.auto_token())
                    .or(trait_item.trait_token())
                    .map(|it| TextRange::new(it.text_range().start(), it.text_range().start())))
                .map(|range| {
                    vec![Action {
                        label: "Change visibility to `pub`.".to_owned(),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::replace(
                            format!("pub{}", if visibility.is_none() { " " } else { "" }),
                            range,
                        )],
                    }]
                }),
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
    mut assoc_fn_handler: F,
    mut assoc_type_handler: G,
) where
    F: FnMut(&mut Vec<Diagnostic>, &ast::Fn),
    G: FnMut(&mut Vec<Diagnostic>, &ast::TypeAlias),
{
    if let Some(assoc_item_list) = trait_item.assoc_item_list() {
        for assoc_item in assoc_item_list.assoc_items() {
            match assoc_item {
                ast::AssocItem::Const(const_item) => results.push(Diagnostic {
                    message: format!(
                        "Associated `const` items in an ink! {ink_scope_name} are not yet supported."
                    ),
                    range: const_item.syntax().text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![
                        Action {
                            label: "Remove `const` item.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range: const_item.syntax().text_range(),
                            edits: vec![TextEdit::delete(const_item.syntax().text_range())],
                        }
                    ]),
                }),
                ast::AssocItem::MacroCall(macro_call) => results.push(Diagnostic {
                    message: format!(
                        "Macros in an ink! {ink_scope_name} are not supported."
                    ),
                    range: macro_call.syntax().text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![
                        Action {
                            label: "Remove macro call.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range: macro_call.syntax().text_range(),
                            edits: vec![TextEdit::delete(macro_call.syntax().text_range())],
                        }
                    ]),
                }),
                ast::AssocItem::TypeAlias(type_alias) => assoc_type_handler(results, &type_alias),
                ast::AssocItem::Fn(fn_item) => {
                    // No default implementations.
                    if let Some(body) = fn_item.body() {
                        results.push(Diagnostic {
                            message: format!("ink! {ink_scope_name} functions with a default implementation are not currently supported."),
                            range: body.syntax().text_range(),
                            severity: Severity::Error,
                            quickfixes: Some(vec![
                                Action {
                                    label: "Remove function body.".to_owned(),
                                    kind: ActionKind::QuickFix,
                                    range: body.syntax().text_range(),
                                    edits: vec![TextEdit::delete(body.syntax().text_range())],
                                }
                            ]),
                        });
                    }

                    assoc_fn_handler(results, &fn_item);
                },
            }
        }
    }
}

/// Ensures that item is defined in the root of an ink! contract.
pub fn ensure_contract_parent<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: InkEntity,
{
    let has_contract_parent = ink_analyzer_ir::ink_parent::<Contract>(item.syntax()).is_some();
    (!has_contract_parent).then(|| Diagnostic {
        message: format!(
            "ink! {ink_scope_name} must be defined in the root of an ink! contract `mod`.",
        ),
        range: item.syntax().text_range(),
        severity: Severity::Error,
        // Moves the item to the root of the closest ink! contract's `mod` item.
        quickfixes: ink_analyzer_ir::ink_ancestors::<Contract>(item.syntax())
            .next()
            .as_ref()
            .and_then(Contract::module)
            .and_then(|mod_item| Some(mod_item).zip(mod_item.item_list()))
            .map(|(mod_item, item_list)| {
                vec![Action::move_item(
                    item.syntax(),
                    utils::item_insert_offset_by_scope_name(&item_list, ink_scope_name),
                    "Move item to the root of the closest ink! contract's `mod` item.".to_owned(),
                    Some(utils::item_children_indenting(mod_item.syntax()).as_str()),
                )]
            }),
    })
}

/// Ensures that item is defined in the root of an `impl` item.
pub fn ensure_impl_parent<T>(item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: HasInkImplParent,
{
    item.parent_impl_item().is_none().then(|| Diagnostic {
        message: format!("ink! {ink_scope_name} must be defined in the root of an `impl` block."),
        range: item.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: ink_analyzer_ir::closest_ancestor_ast_type::<SyntaxNode, ast::Impl>(
            item.syntax(),
        )
        .or(item
            .syntax()
            .siblings(ink_analyzer_ir::syntax::Direction::Prev)
            .find_map(ast::Impl::cast)
            .or(item
                .syntax()
                .siblings(ink_analyzer_ir::syntax::Direction::Next)
                .find_map(ast::Impl::cast)))
        .as_ref()
        .and_then(|impl_item| Some(impl_item).zip(impl_item.assoc_item_list()))
        .map(|(impl_item, assoc_item_list)| {
            // Moves the item to the root of the closest parent/ancestor or sibling `impl` block.
            vec![Action::move_item(
                item.syntax(),
                utils::assoc_item_insert_offset_end(&assoc_item_list),
                "Move item to the root of the closest `impl` block.".to_owned(),
                Some(utils::item_children_indenting(impl_item.syntax()).as_str()),
            )]
        })
        .or(ink_analyzer_ir::ink_ancestors::<Contract>(item.syntax())
            .next()
            .and_then(|contract| {
                // Moves the item to the first non-trait `impl` block or creates a new `impl` block if necessary for the parent ink! contract (if any).
                utils::callable_insert_offset_indent_and_affixes(&contract).map(
                    |(insert_offset, indent, prefix, suffix)| {
                        vec![Action::move_item_with_affixes(
                            item.syntax(),
                            insert_offset,
                            "Move item to the root of the closest `impl` block.".to_owned(),
                            Some(indent.as_str()),
                            prefix.as_deref().or(Some("")),
                            suffix.as_deref().or(Some("")),
                        )]
                    },
                )
            })),
    })
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
pub fn ensure_valid_quasi_direct_ink_descendants<T, F>(
    results: &mut Vec<Diagnostic>,
    item: &T,
    is_valid_quasi_direct_descendant: F,
) where
    T: InkEntity,
    F: Fn(&InkAttribute) -> bool,
{
    for attr in item.tree().ink_attrs_closest_descendants().filter(|it| {
        !matches!(
            it.kind(),
            InkAttributeKind::Macro(InkMacroKind::Unknown)
                | InkAttributeKind::Arg(InkArgKind::Unknown)
        )
    }) {
        // Only show ink! scope diagnostics for the "primary" ink! attribute for it's parent item.
        let is_primary_attribute = [attr.clone()]
            .into_iter()
            .chain(attr.siblings())
            // Sort in order of appearance first.
            .sorted_by_key(|attr| attr.syntax().text_range().start())
            // Then sort in order of attribute kind ranking.
            .sorted()
            .next()
            .is_some_and(|primary_attr| {
                primary_attr.syntax().text_range() == attr.syntax().text_range()
            });
        if is_primary_attribute && !is_valid_quasi_direct_descendant(&attr) {
            results.push(Diagnostic {
                message: format!("Invalid scope for an `{}` item.", attr.syntax()),
                range: attr.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: Some(
                    ink_analyzer_ir::parent_ast_item(attr.syntax())
                        .map(|item| {
                            vec![
                                Action::remove_attribute(&attr),
                                Action::remove_item(item.syntax()),
                            ]
                        })
                        .unwrap_or_else(|| vec![Action::remove_attribute(&attr)]),
                ),
            });
        }
    }
}

/// Ensures that no ink! descendants in the item's scope.
pub fn ensure_no_ink_descendants<T>(results: &mut Vec<Diagnostic>, item: &T, ink_scope_name: &str)
where
    T: InkEntity,
{
    for attr in item.tree().ink_attrs_descendants().filter(|it| {
        !matches!(
            it.kind(),
            InkAttributeKind::Macro(InkMacroKind::Unknown)
                | InkAttributeKind::Arg(InkArgKind::Unknown)
        )
    }) {
        results.push(Diagnostic {
            message: format!(
                "`{}` cannot be used inside an ink! {ink_scope_name}.",
                attr.syntax()
            ),
            range: attr.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: Some(
                ink_analyzer_ir::parent_ast_item(attr.syntax())
                    .map(|item| {
                        vec![
                            Action::remove_attribute(&attr),
                            Action::remove_item(item.syntax()),
                        ]
                    })
                    .unwrap_or_else(|| vec![Action::remove_attribute(&attr)]),
            ),
        });
    }
}

/// Ensures that the ADT item (i.e. struct, enum or union) implements the specified external trait.
pub fn ensure_external_trait_impl(
    adt: &ast::Adt,
    trait_info: (&str, &[&str], &SyntaxNode),
    message: String,
    label: String,
    fix_plain: String,
    fix_snippet_option: Option<String>,
) -> Option<Diagnostic> {
    // Only continue if the ADT has a name.
    let name = adt.name()?.to_string();

    // Finds external trait implementation (if any).
    let (trait_name, crate_qualifiers, ref_node) = trait_info;
    match resolution::external_trait_impl(trait_name, crate_qualifiers, ref_node, Some(&name)) {
        // Handles no external trait implementation.
        None => {
            let range = utils::ast_item_declaration_range(&match adt.clone() {
                ast::Adt::Enum(it) => ast::Item::Enum(it),
                ast::Adt::Struct(it) => ast::Item::Struct(it),
                ast::Adt::Union(it) => ast::Item::Union(it),
            })
            .unwrap_or(adt.syntax().text_range());
            let indent_option = utils::item_indenting(adt.syntax());

            Some(Diagnostic {
                message,
                range,
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label,
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::insert_with_snippet(
                        indent_option
                            .as_ref()
                            .map(|indent| utils::apply_indenting(&fix_plain, indent))
                            .unwrap_or(fix_plain),
                        adt.syntax().text_range().end(),
                        fix_snippet_option.map(|fix_snippet| {
                            indent_option
                                .as_ref()
                                .map(|indent| utils::apply_indenting(&fix_snippet, indent))
                                .unwrap_or(fix_snippet)
                        }),
                    )],
                }]),
            })
        }
        // Ignores resolved external trait implementation.
        Some(_) => None,
    }
}

/// Ensures that the ADT item (i.e. struct, enum or union) implements all SCALE codec traits.
///
/// Ref: <https://docs.substrate.io/reference/scale-codec/>.
pub fn ensure_impl_scale_codec_traits(adt: &ast::Adt, message_prefix: &str) -> Option<Diagnostic> {
    // Standalone derive attribute (if any).
    let standalone_derive_attr = adt.attrs().find(|attr| {
        attr.path()
            .is_some_and(|path| path.to_string().trim() == "derive")
    });

    // Utilities for extracting derive attribute meta items.
    let token_tree_to_non_delimited_string = |token_tree: &ast::TokenTree| {
        let r_paren_option = token_tree.r_paren_token();
        token_tree
            .syntax()
            .children_with_tokens()
            .skip(usize::from(token_tree.l_paren_token().is_some()))
            .take_while(|it| r_paren_option.is_none() || it.as_token() != r_paren_option.as_ref())
            .join("")
    };
    let meta_to_path_list = |meta: &str| {
        meta.replace(' ', "")
            .split(',')
            .filter_map(ink_analyzer_ir::path_from_str)
            .collect::<Vec<_>>()
    };

    // Standalone derive attribute meta item string (if any).
    let standalone_derive_meta = standalone_derive_attr
        .as_ref()
        .and_then(ast::Attr::token_tree)
        .as_ref()
        .map(token_tree_to_non_delimited_string);

    // Extracts derive item paths from both the standalone and conditional derive attributes.
    let standalone_derived_items = standalone_derive_meta.as_deref().map(meta_to_path_list);
    let conditional_derived_items = adt.attrs().find_map(|attr| {
        if attr
            .path()
            .is_some_and(|path| path.to_string().trim() == "cfg_attr")
        {
            attr.token_tree().map(|token_tree| {
                token_tree
                    .syntax()
                    .children()
                    .filter(|node| {
                        let is_after_derive = || {
                            node.first_token()
                                .and_then(|token| {
                                    ink_analyzer_ir::closest_non_trivia_token(
                                        &token,
                                        SyntaxToken::prev_token,
                                    )
                                })
                                .is_some_and(|token| token.text() == "derive")
                        };
                        ast::TokenTree::can_cast(node.kind()) && is_after_derive()
                    })
                    .filter_map(|node| {
                        ast::TokenTree::cast(node)
                            .as_ref()
                            .map(token_tree_to_non_delimited_string)
                    })
                    .flat_map(|meta| meta_to_path_list(&meta))
                    .collect::<Vec<_>>()
            })
        } else {
            None
        }
    });
    let derived_items = match (standalone_derived_items, conditional_derived_items) {
        (Some(standalone), Some(conditional)) => {
            let mut items = Vec::with_capacity(standalone.len() + conditional.len());
            items.extend(standalone);
            items.extend(conditional);
            Some(items)
        }
        (standalone_option, conditional_option) => standalone_option.or(conditional_option),
    };

    // Finds unimplemented SCALE codec traits.
    const SCALE_QUALIFIERS: [&str; 3] = ["scale", "ink::scale", "parity_scale_codec"];
    const SCALE_INFO_QUALIFIERS: [&str; 2] = ["scale_info", "ink::scale_info"];
    let unimplemented_traits: Vec<_> = ([
        ("Encode", &SCALE_QUALIFIERS, "scale::Encode"),
        ("Decode", &SCALE_QUALIFIERS, "scale::Decode"),
        ("TypeInfo", &SCALE_INFO_QUALIFIERS, "scale_info::TypeInfo"),
    ] as [(&str, &[&str], &str); 3])
        .into_iter()
        .filter_map(|(trait_name, qualifiers, trait_path)| {
            // Finds derived trait implementation for the custom type (if any).
            let is_derived = derived_items.as_ref().is_some_and(|item_paths| {
                item_paths.iter().any(|path| {
                    resolution::is_external_crate_item(trait_name, path, qualifiers, adt.syntax())
                })
            });
            // Finds trait implementation for the custom type (if any).
            let is_implemented = || {
                adt.name()
                    .as_ref()
                    .map(ToString::to_string)
                    .zip(adt.syntax().ancestors().last())
                    .and_then(|(error_code_name, ref_node)| {
                        resolution::external_trait_impl(
                            trait_name,
                            qualifiers,
                            &ref_node,
                            Some(&error_code_name),
                        )
                    })
                    .is_some()
            };

            (!is_derived && !is_implemented()).then_some(trait_path)
        })
        .collect();

    // Returns diagnostic for unimplemented SCALE codec traits (if any).
    (!unimplemented_traits.is_empty()).then(|| {
        // Determines the insert text, range and snippet for adding a derive implementation.
        let trait_paths_plain = unimplemented_traits.join(", ");
        let trait_paths_snippet = unimplemented_traits
            .iter()
            .enumerate()
            .map(|(idx, path)| format!("${{{}:{path}}}", idx + 1))
            .join(", ");
        let trait_paths_display = unimplemented_traits
            .iter()
            .map(|path| format!("`{path}`"))
            .join(", ");
        // Either updates an existing standalone derive attribute or creates a new one.
        let (insert_text, insert_range, insert_snippet) = standalone_derive_attr
            .as_ref()
            .map(|attr| {
                let meta_prefix = standalone_derive_meta.as_ref().map(|meta| {
                    format!(
                        "{meta}{}",
                        if meta.trim_end().ends_with(',') {
                            ""
                        } else {
                            ", "
                        }
                    )
                });
                (
                    format!(
                        "#[derive({}{trait_paths_plain})]",
                        meta_prefix.as_deref().unwrap_or_default()
                    ),
                    attr.syntax().text_range(),
                    format!(
                        "#[derive({}{trait_paths_snippet})]",
                        meta_prefix.as_deref().unwrap_or_default()
                    ),
                )
            })
            .unwrap_or_else(|| {
                (
                    format!("#[derive({trait_paths_plain})]"),
                    TextRange::empty(
                        adt.attrs()
                            .last()
                            .and_then(|attr| attr.syntax().last_token())
                            // Finds the first non-(attribute/rustdoc/trivia) token for the item.
                            .and_then(|it| {
                                ink_analyzer_ir::closest_non_trivia_token(
                                    &it,
                                    SyntaxToken::next_token,
                                )
                            })
                            .as_ref()
                            // Defaults to the start of the custom type.
                            .map_or(adt.syntax().text_range(), SyntaxToken::text_range)
                            .start(),
                    ),
                    format!("#[derive({trait_paths_snippet})]"),
                )
            });
        // Determines text range for item "declaration" (fallbacks to range of the entire item).
        let item_declaration_text_range = utils::ast_item_declaration_range(&match adt.clone() {
            ast::Adt::Enum(it) => ast::Item::Enum(it),
            ast::Adt::Struct(it) => ast::Item::Struct(it),
            ast::Adt::Union(it) => ast::Item::Union(it),
        })
        .unwrap_or(adt.syntax().text_range());

        Diagnostic {
            message: format!(
                "{message_prefix} must implement the {trait_paths_display} trait{}.",
                if unimplemented_traits.len() == 1 {
                    ""
                } else {
                    "s"
                }
            ),
            range: item_declaration_text_range,
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: format!(
                    "Derive {trait_paths_display} trait implementation{}{}.",
                    if unimplemented_traits.len() == 1 {
                        ""
                    } else {
                        "s"
                    },
                    adt.name()
                        .as_ref()
                        .map(ToString::to_string)
                        .map(|name| format!(" for `{name}`"))
                        .unwrap_or_default()
                ),
                kind: ActionKind::QuickFix,
                range: item_declaration_text_range,
                edits: vec![TextEdit::new(
                    insert_text,
                    insert_range,
                    Some(insert_snippet),
                )],
            }]),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::verify_actions;
    use ink_analyzer_ir::InkFile;
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

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
    macro_rules! valid_attributes_versioned {
        (v4) => {
            []
        };
        (v5) => {
            [
                quote_as_str! {
                    #[ink(message, selector=@)] // message is required, otherwise this would be incomplete.
                },
                quote_as_str! {
                    #[ink(function = 1)]
                },
                quote_as_str! {
                    #[ink(function = 0x1)]
                },
            ]
        };
    }
    macro_rules! valid_attributes {
        ($version: tt) => {
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
                quote_as_str! {
                    #[ink_e2e::test]
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
                quote_as_str! {
                    #[ink(constructor, selector=1)] // constructor is required, otherwise this would be incomplete.
                },
                quote_as_str! {
                    #[ink(constructor, selector=0xA)] // constructor is required, otherwise this would be incomplete.
                },
                // Arguments that can have a wildcard/underscore value.
                quote_as_str! {
                    #[ink(message, selector=_)] // message is required, otherwise this would be incomplete.
                },
                quote_as_str! {
                    #[ink(constructor, selector=_)] // constructor is required, otherwise this would be incomplete.
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
                quote_as_str! {
                    #[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml")]
                },
                quote_as_str! {
                    #[ink_e2e::test(keep_attr="foo,bar")]
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
                quote_as_str! {
                    #[ink_e2e::test(environment=my::env::Types)]
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
                    #[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=my::env::Types, keep_attr="foo,bar")]
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
            .into_iter()
            .chain(valid_attributes_versioned!($version))
        };
        () => {
            valid_attributes!(v4)
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
        let code = quote_as_pretty_string! {
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
        };
        let file = InkFile::parse(&code);

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
        // Verifies quickfixes.
        let expected_quickfixes = vec![
            vec![TestResultAction {
                label: "Rename identifier",
                edits: vec![TestResultTextRange {
                    text: "example",
                    start_pat: Some("<-__ink_example"),
                    end_pat: Some("__ink_example"),
                }],
            }],
            vec![TestResultAction {
                label: "Rename identifier",
                edits: vec![TestResultTextRange {
                    text: "Example",
                    start_pat: Some("<-__ink_Example"),
                    end_pat: Some("struct __ink_Example"),
                }],
            }],
            vec![TestResultAction {
                label: "Rename identifier",
                edits: vec![TestResultTextRange {
                    text: "Example",
                    start_pat: Some("<-__ink_Example->"),
                    end_pat: Some("impl __ink_Example"),
                }],
            }],
            vec![TestResultAction {
                label: "Rename identifier",
                edits: vec![TestResultTextRange {
                    text: "new",
                    start_pat: Some("<-__ink_new"),
                    end_pat: Some("__ink_new"),
                }],
            }],
            vec![TestResultAction {
                label: "Rename identifier",
                edits: vec![TestResultTextRange {
                    text: "init_value",
                    start_pat: Some("<-__ink_init_value: bool"),
                    end_pat: Some("(__ink_init_value"),
                }],
            }],
            vec![TestResultAction {
                label: "Rename identifier",
                edits: vec![TestResultTextRange {
                    text: "init_value",
                    start_pat: Some("<-__ink_init_value }"),
                    end_pat: Some("value: __ink_init_value"),
                }],
            }],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
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
        for (code, start_pat, end_pat) in [
            ("#[ink::xyz]", Some("<-#[ink::xyz]"), Some("#[ink::xyz]")),
            (
                "#[ink::abc::xyz]",
                Some("<-#[ink::abc::xyz]"),
                Some("#[ink::abc::xyz]"),
            ),
            ("#[ink(xyz)]", Some("<-#[ink(xyz)]"), Some("#[ink(xyz)]")),
        ] {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_unknown_ink_attributes(&mut results, &attrs);

            // Verifies diagnostics.
            assert_eq!(results.len(), 1);
            assert_eq!(results[0].severity, Severity::Warning);
            // Verifies quickfixes.
            let quickfixes = results[0].quickfixes.as_ref().unwrap();
            let expected_quickfixes = vec![TestResultAction {
                label: "Remove",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat,
                    end_pat,
                }],
            }];
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn valid_attribute_argument_format_and_value_type_works() {
        // NOTE: This test only cares about ink! attribute arguments not macros,
        // See `ensure_valid_attribute_arguments` doc.
        for (version, attributes) in versioned_fixtures!(valid_attributes) {
            for code in attributes {
                let attr = parse_first_ink_attr(code);

                let mut results = Vec::new();
                ensure_valid_attribute_arguments(&mut results, &attr, version);
                assert!(
                    results.is_empty(),
                    "attribute: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn invalid_attribute_argument_format_and_value_type_fails() {
        // NOTE: This test only cares about ink! attribute arguments not macros,
        // See `ensure_valid_attribute_arguments` doc.
        for (code, expected_quickfixes) in [
            // Arguments that should have no value.
            (
                "#[ink(storage=1)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "storage",
                        start_pat: Some("<-storage=1"),
                        end_pat: Some("storage=1"),
                    }],
                }],
            ),
            (
                "#[ink(constructor=)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "constructor",
                        start_pat: Some("<-constructor="),
                        end_pat: Some("constructor="),
                    }],
                }],
            ),
            (
                r#"#[ink(default="hello")]"#,
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "default",
                        start_pat: Some(r#"<-default="hello""#),
                        end_pat: Some(r#"default="hello""#),
                    }],
                }],
            ),
            (
                "#[ink(message=true)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "message",
                        start_pat: Some("<-message=true"),
                        end_pat: Some("message=true"),
                    }],
                }],
            ),
            (
                "#[ink(event='a')]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "event",
                        start_pat: Some("<-event='a'"),
                        end_pat: Some("event='a'"),
                    }],
                }],
            ),
            (
                "#[ink(anonymous=0x1)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "anonymous",
                        start_pat: Some("<-anonymous=0x1"),
                        end_pat: Some("anonymous=0x1"),
                    }],
                }],
            ),
            (
                r#"#[ink(topic=b"")]"#,
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "topic",
                        start_pat: Some(r#"<-topic=b"""#),
                        end_pat: Some(r#"topic=b"""#),
                    }],
                }],
            ),
            (
                "#[ink(payable=3.2)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "payable",
                        start_pat: Some("<-payable=3.2"),
                        end_pat: Some("payable=3.2"),
                    }],
                }],
            ),
            (
                "#[ink(impl=my::path::item)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "impl",
                        start_pat: Some("<-impl=my::path::item"),
                        end_pat: Some("impl=my::path::item"),
                    }],
                }],
            ),
            // Arguments that should have an integer (`u32` to be specific) value.
            (
                "#[ink(selector)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some("<-selector"),
                        end_pat: Some("selector"),
                    }],
                }],
            ),
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L1200-L1211>.
            (
                "#[ink(selector=-1)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some("<-selector=-1"),
                        end_pat: Some("selector=-1"),
                    }],
                }],
            ),
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L1213-L1224>.
            (
                "#[ink(selector=0xFFFF_FFFF_FFFF_FFFF)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some("<-selector=0xFFFF_FFFF_FFFF_FFFF"),
                        end_pat: Some("selector=0xFFFF_FFFF_FFFF_FFFF"),
                    }],
                }],
            ),
            (
                r#"#[ink(selector="hello")]"#,
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some(r#"<-selector="hello""#),
                        end_pat: Some(r#"selector="hello""#),
                    }],
                }],
            ),
            (
                "#[ink(selector='a')]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some("<-selector='a'"),
                        end_pat: Some("selector='a'"),
                    }],
                }],
            ),
            (
                "#[ink(selector=false)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some("<-selector=false"),
                        end_pat: Some("selector=false"),
                    }],
                }],
            ),
            // Arguments that can have a wildcard/underscore value.
            (
                "#[ink(selector=*)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some("<-selector=*"),
                        end_pat: Some("selector=*"),
                    }],
                }],
            ),
            (
                r#"#[ink(selector="_")]"#, // should be an underscore expression, not a string
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "selector = 1",
                        start_pat: Some(r#"<-selector="_""#),
                        end_pat: Some(r#"selector="_""#),
                    }],
                }],
            ),
            // Arguments that should have a string value.
            (
                "#[ink::contract(keep_attr=my::path::item)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: r#"keep_attr = """#,
                        start_pat: Some("<-keep_attr=my::path::item"),
                        end_pat: Some("keep_attr=my::path::item"),
                    }],
                }],
            ),
            (
                "#[ink(namespace=0x1)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: r#"namespace = "my_namespace""#,
                        start_pat: Some("<-namespace=0x1"),
                        end_pat: Some("namespace=0x1"),
                    }],
                }],
            ),
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L1248-L1256>.
            (
                r#"#[ink(namespace="::invalid_identifier")]"#,
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: r#"namespace = "my_namespace""#,
                        start_pat: Some(r#"<-namespace="::invalid_identifier""#),
                        end_pat: Some(r#"namespace="::invalid_identifier""#),
                    }],
                }],
            ),
            // Arguments that should have a boolean value.
            (
                "#[ink(handle_status=1)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "handle_status = true",
                        start_pat: Some("<-handle_status=1"),
                        end_pat: Some("handle_status=1"),
                    }],
                }],
            ),
            (
                "#[ink::storage_item(derive)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "derive = true",
                        start_pat: Some("<-derive"),
                        end_pat: Some("derive"),
                    }],
                }],
            ),
            // Arguments that should have a path value.
            (
                r#"#[ink::contract(env="my::env::Types")]"#,
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "env = crate::",
                        start_pat: Some(r#"<-env="my::env::Types""#),
                        end_pat: Some(r#"env="my::env::Types""#),
                    }],
                }],
            ),
            (
                r#"#[ink_e2e::test(environment="my::env::Types")]"#,
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "environment = crate::",
                        start_pat: Some(r#"<-environment="my::env::Types""#),
                        end_pat: Some(r#"environment="my::env::Types""#),
                    }],
                }],
            ),
            (
                "#[ink::contract(env=2.4)]",
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "env = crate::",
                        start_pat: Some("<-env=2.4"),
                        end_pat: Some("env=2.4"),
                    }],
                }],
            ),
            // Compound arguments.
            (
                "#[ink::contract(env=my::env::Types, keep_attr)]", // Bad keep_attr.
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: r#"keep_attr = """#,
                        start_pat: Some("<-keep_attr"),
                        end_pat: Some("keep_attr"),
                    }],
                }],
            ),
            (
                "#[ink(constructor, payable=1, default, selector=1)]", // Bad payable.
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "payable",
                        start_pat: Some("<-payable=1"),
                        end_pat: Some("payable=1"),
                    }],
                }],
            ),
            (
                r#"#[ink(message="hello", payable, selector=2)]"#, // message.
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "message",
                        start_pat: Some(r#"<-message="hello""#),
                        end_pat: Some(r#"message="hello""#),
                    }],
                }],
            ),
            (
                "#[ink(event=0x1, anonymous)]", // bad event.
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "event",
                        start_pat: Some("<-event=0x1"),
                        end_pat: Some("event=0x1"),
                    }],
                }],
            ),
            (
                "#[ink(extension, handle_status=true)]", // bad extension.
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "extension = 1",
                        start_pat: Some("<-extension"),
                        end_pat: Some("extension"),
                    }],
                }],
            ),
            (
                r#"#[ink_e2e::contract(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment)]"#, // Bad environment.
                vec![TestResultAction {
                    label: "argument value",
                    edits: vec![TestResultTextRange {
                        text: "environment = crate::",
                        start_pat: Some("<-environment"),
                        end_pat: Some("environment"),
                    }],
                }],
            ),
        ] {
            let attr = parse_first_ink_attr(code);

            for version in [Version::V4, Version::V5] {
                let mut results = Vec::new();
                ensure_valid_attribute_arguments(&mut results, &attr, version);

                // Verifies diagnostics.
                assert_eq!(
                    results.len(),
                    1,
                    "attribute: {code}, version: {:?}",
                    version
                );
                assert_eq!(
                    results[0].severity,
                    Severity::Error,
                    "attribute: {code}, version: {:?}",
                    version
                );
                // Verifies quickfixes.
                verify_actions(
                    code,
                    results[0].quickfixes.as_ref().unwrap(),
                    &expected_quickfixes,
                );
            }
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
        for (code, expected_quickfixes) in [
            (
                r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar", keep_attr="hello")]"#, // duplicate `keep_attr`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-, keep_attr="hello""#),
                        end_pat: Some(r#"keep_attr="hello""#),
                    }],
                }],
            ),
            (
                r#"
                #[ink::contract(env=my::env::Types)]
                #[ink::contract(keep_attr="foo,bar")]
                "#, // duplicate `contract`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-#[ink::contract(keep_attr="foo,bar")]"#),
                        end_pat: Some(r#"#[ink::contract(keep_attr="foo,bar")]"#),
                    }],
                }],
            ),
            (
                r#"
                #[ink::contract]
                #[ink::contract(keep_attr="foo,bar")]
                "#, // duplicate `contract`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-#[ink::contract(keep_attr="foo,bar")]"#),
                        end_pat: Some(r#"#[ink::contract(keep_attr="foo,bar")]"#),
                    }],
                }],
            ),
            (
                r#"
                #[ink(constructor, payable, default, selector=1)]
                #[ink(constructor)]
                "#, // duplicate `constructor`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                    }],
                }],
            ),
            (
                r#"
                #[ink(message)]
                #[ink(payable)]
                #[ink(selector=2)]
                #[ink(selector=0xA)]
                "#, // duplicate `selector`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(selector=0xA)]"),
                        end_pat: Some("#[ink(selector=0xA)]"),
                    }],
                }],
            ),
        ] {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_duplicate_attributes_and_arguments(&mut results, &attrs);

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "attributes: {code}");
            assert_eq!(results[0].severity, Severity::Error, "attributes: {code}");
            // Verifies quickfixes.
            verify_actions(
                code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn no_conflicting_attributes_and_arguments_works() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        for code in valid_attributes!() {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_conflicting_attributes_and_arguments(&mut results, &attrs, Version::V4);
            assert!(results.is_empty(), "attributes: {code}");
        }
    }

    #[test]
    fn conflicting_attributes_and_arguments_fails() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        for (code, expected_quickfixes) in [
            // Single attributes.
            (
                "#[ink::chain_extension(env=my::env::Types)]", // conflicting `env`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-(env"),
                        end_pat: Some("my::env::Types)"),
                    }],
                }],
            ),
            (
                r#"#[ink::contract(namespace="my_namespace")]"#, // conflicting `namespace`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-(namespace"),
                        end_pat: Some(r#""my_namespace")"#),
                    }],
                }],
            ),
            (
                r#"#[ink::storage_item(keep_attr="foo,bar")]"#, // conflicting `keep_attr`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-(keep_attr"),
                        end_pat: Some(r#""foo,bar")"#),
                    }],
                }],
            ),
            (
                "#[ink::test(payable)]", // conflicting `payable`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-(payable"),
                        end_pat: Some("payable)"),
                    }],
                }],
            ),
            (
                "#[ink::trait_definition(derive=false)]", // conflicting `derive`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-(derive"),
                        end_pat: Some("false)"),
                    }],
                }],
            ),
            (
                "#[ink(storage, anonymous)]", // conflicting `anonymous`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, anonymous"),
                        end_pat: Some("anonymous"),
                    }],
                }],
            ),
            (
                "#[ink(event, default)]", // conflicting `default`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, default"),
                        end_pat: Some("default"),
                    }],
                }],
            ),
            (
                "#[ink(topic, selector=1)]", // conflicting `selector`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, selector"),
                        end_pat: Some("selector=1"),
                    }],
                }],
            ),
            (
                "#[ink(constructor, derive=true)]", // conflicting `derive`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, derive"),
                        end_pat: Some("derive=true"),
                    }],
                }],
            ),
            (
                r#"#[ink(message, namespace="my_namespace")]"#, // conflicting `namespace`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, namespace"),
                        end_pat: Some(r#"namespace="my_namespace""#),
                    }],
                }],
            ),
            (
                "#[ink(extension=1, env=my::env::Types)]", // conflicting `env`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, env"),
                        end_pat: Some("env=my::env::Types"),
                    }],
                }],
            ),
            // Multiple attributes.
            (
                r#"
                #[ink::contract(env=my::env::Types)]
                #[ink::trait_definition(namespace="my_namespace")]
                "#, // conflicting `contract`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-#[ink::trait_definition(namespace="my_namespace")]"#),
                        end_pat: Some(r#"#[ink::trait_definition(namespace="my_namespace")]"#),
                    }],
                }],
            ),
            (
                r#"
                #[ink::contract(env=my::env::Types)]
                #[ink_e2e::test(environment=my::env::Types)]
                "#, // conflicting `contract`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink_e2e::test(environment=my::env::Types)]"),
                        end_pat: Some("#[ink_e2e::test(environment=my::env::Types)]"),
                    }],
                }],
            ),
            (
                r#"
                #[ink::contract]
                #[ink(message)]
                "#, // conflicting `contract`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                    }],
                }],
            ),
            (
                r#"
                #[ink(constructor, payable, default, selector=1)]
                #[ink(event)]
                "#, // conflicting `event`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                    }],
                }],
            ),
            (
                r#"
                #[ink(message)]
                #[ink(payable, selector=2, topic)]
                "#, // `topic` conflicts with `message`.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, topic"),
                        end_pat: Some("topic"),
                    }],
                }],
            ),
            // Wrong order of attributes and/or arguments.
            (
                "#[ink(anonymous, event)]", // `event` should come first.
                vec![TestResultAction {
                    label: "first argument",
                    // Makes `event` the first argument.
                    edits: vec![
                        TestResultTextRange {
                            text: "event",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-, event"),
                            end_pat: Some("event"),
                        },
                    ],
                }],
            ),
            (
                r#"
                #[ink(anonymous)]
                #[ink(event)]
                "#, // `event` should come first.
                vec![TestResultAction {
                    label: "first ink! attribute",
                    edits: vec![
                        TestResultTextRange {
                            text: "#[ink(event)]",
                            start_pat: Some("<-#[ink(anonymous)]"),
                            end_pat: Some("<-#[ink(anonymous)]"),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                        },
                    ],
                }],
            ),
            (
                "#[ink(handle_status=true, extension=1)]", // `extension` should come first.
                vec![TestResultAction {
                    label: "first argument",
                    edits: vec![
                        TestResultTextRange {
                            text: "extension",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-, extension"),
                            end_pat: Some("extension=1"),
                        },
                    ],
                }],
            ),
            (
                r#"
                #[ink(handle_status=true)]
                #[ink(extension=1)]
                "#, // `extension` should come first.
                vec![TestResultAction {
                    label: "first ink! attribute",
                    edits: vec![
                        TestResultTextRange {
                            text: "#[ink(extension=1)]",
                            start_pat: Some("<-#[ink(handle_status=true)]"),
                            end_pat: Some("<-#[ink(handle_status=true)]"),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(extension=1)]"),
                            end_pat: Some("#[ink(extension=1)]"),
                        },
                    ],
                }],
            ),
            (
                "#[ink(payable, message, default, selector=1)]", // `message` should come first.
                vec![TestResultAction {
                    label: "first argument",
                    edits: vec![
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-message"),
                            end_pat: Some("message,"),
                        },
                    ],
                }],
            ),
            (
                r#"
                #[ink(payable, default, selector=1)]
                #[ink(message)]
                "#, // `message` should come first.
                vec![TestResultAction {
                    label: "first ink! attribute",
                    edits: vec![
                        TestResultTextRange {
                            text: "#[ink(message)]",
                            start_pat: Some("<-#[ink(payable, default, selector=1)]"),
                            end_pat: Some("<-#[ink(payable, default, selector=1)]"),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                        },
                    ],
                }],
            ),
            // Macro arguments as standalone attributes.
            (
                r#"
                #[ink::contract]
                #[ink(env=my::env::Types)]
                "#, // conflicts with `contract`, should be an argument.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(env=my::env::Types)]"),
                        end_pat: Some("#[ink(env=my::env::Types)]"),
                    }],
                }],
            ),
            (
                r#"
                #[ink::trait_definition]
                #[ink(keep_attr="foo,bar")]
                "#, // conflicts with `trait_definition`, should be an argument.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-#[ink(keep_attr="foo,bar")]"#),
                        end_pat: Some(r#"#[ink(keep_attr="foo,bar")]"#),
                    }],
                }],
            ),
            (
                r#"
                #[ink::storage_item]
                #[ink(derive=false)]
                "#, // conflicts with `storage_item`, should be an argument.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(derive=false)]"),
                        end_pat: Some("#[ink(derive=false)]"),
                    }],
                }],
            ),
            (
                r#"
                #[ink_e2e::test]
                #[ink(environment=my::env::Types)]
                "#, // conflicts with `e2e test`, should be an argument.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(environment=my::env::Types)]"),
                        end_pat: Some("#[ink(environment=my::env::Types)]"),
                    }],
                }],
            ),
            // Incomplete and/or ambiguous.
            (
                "#[ink(anonymous)]", // missing `event`.
                vec![TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "event, ",
                        start_pat: Some("#[ink("),
                        end_pat: Some("#[ink("),
                    }],
                }],
            ),
            (
                "#[ink(handle_status=true)]", // missing `extension`.
                vec![TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "extension = 1, ",
                        start_pat: Some("#[ink("),
                        end_pat: Some("#[ink("),
                    }],
                }],
            ),
            (
                "#[ink(payable, default, selector=1)]", // incomplete and ambiguous.
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "constructor, ",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "message, ",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        }],
                    },
                ],
            ),
            (
                r#"
                #[ink(payable, default)]
                #[ink(selector=1)]
                "#, // incomplete and ambiguous.
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "constructor, ",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "message, ",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        }],
                    },
                ],
            ),
            (
                r#"#[ink(keep_attr="foo,bar")]"#, // incomplete and ambiguous.
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: r#"#[ink::contract(keep_attr = "foo,bar")]"#,
                            start_pat: Some(r#"<-#[ink(keep_attr="foo,bar")]"#),
                            end_pat: Some(r#"#[ink(keep_attr="foo,bar")]"#),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: r#"#[ink::trait_definition(keep_attr = "foo,bar")]"#,
                            start_pat: Some(r#"<-#[ink(keep_attr="foo,bar")]"#),
                            end_pat: Some(r#"#[ink(keep_attr="foo,bar")]"#),
                        }],
                    },
                ],
            ),
            // Namespace :-).
            (
                r#"
                #[ink(namespace="my_namespace")]
                #[ink::trait_definition]
                "#, // `trait_definition` should come first, and namespace should be an argument.
                vec![TestResultAction {
                    label: "first ink! attribute",
                    edits: vec![
                        TestResultTextRange {
                            text: "#[ink::trait_definition]",
                            start_pat: Some(r#"<-#[ink(namespace="my_namespace")]"#),
                            end_pat: Some(r#"<-#[ink(namespace="my_namespace")]"#),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink::trait_definition]"),
                            end_pat: Some("#[ink::trait_definition]"),
                        },
                    ],
                }],
            ),
            (
                r#"
                #[ink::trait_definition]
                #[ink(namespace="my_namespace")]
                "#, // conflicts with `trait_definition`, it should be an argument.
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-#[ink(namespace="my_namespace")]"#),
                        end_pat: Some(r#"#[ink(namespace="my_namespace")]"#),
                    }],
                }],
            ),
            (
                r#"
                #[ink(namespace="my_namespace")]
                #[ink(impl)]
                "#, // `impl` should come first.
                vec![TestResultAction {
                    label: "first ink! attribute",
                    edits: vec![
                        TestResultTextRange {
                            text: "#[ink(impl)]",
                            start_pat: Some(r#"<-#[ink(namespace="my_namespace")]"#),
                            end_pat: Some(r#"<-#[ink(namespace="my_namespace")]"#),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(impl)]"),
                            end_pat: Some("#[ink(impl)]"),
                        },
                    ],
                }],
            ),
        ] {
            let attrs = parse_all_ink_attrs(code);

            let mut results = Vec::new();
            ensure_no_conflicting_attributes_and_arguments(&mut results, &attrs, Version::V4);

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "attributes: {code}");
            assert_eq!(results[0].severity, Severity::Error, "attributes: {code}");
            // Verifies quickfixes.
            verify_actions(
                code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }
}
