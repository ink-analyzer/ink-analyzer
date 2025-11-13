//! Utilities for ink! diagnostics.

use std::collections::HashSet;

use ink_analyzer_ir::ast::{
    AstNode, AstToken, HasAttrs, HasGenericParams, HasName, HasTypeBounds, HasVisibility,
};
use ink_analyzer_ir::meta::MetaValue;
use ink_analyzer_ir::syntax::{SyntaxKind, SyntaxNode, SyntaxToken, TextRange};
use ink_analyzer_ir::{
    ast, ChainExtension, Contract, HasInkImplParent, InkArg, InkArgKind, InkArgValueKind,
    InkArgValueStringKind, InkAttribute, InkAttributeKind, InkEntity, InkMacroKind, IsInkFn,
    IsInkStruct, IsInkTrait,
};
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils;
use crate::{resolution, Action, ActionKind, Diagnostic, Severity, Version};

/// Runs generic diagnostics that apply to all ink! entities.
/// (e.g. `ensure_no_unknown_ink_attributes`, `ensure_no_ink_identifiers`,
/// `ensure_no_duplicate_attributes_and_arguments`, `validate_arg`).
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
    // See `validate_arg` doc.
    for attr in item.tree().ink_attrs_in_scope() {
        for arg in attr.args() {
            validate_arg(results, arg, version, Some(&attr));
        }
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
            // see `validate_entity_attributes` doc.
            validate_entity_attributes(results, &attrs, version);
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
/// Handles both ink! attribute macros (e.g. `#[ink::xyz]`)
/// and ink! attribute arguments (e.g. `#[ink(xyz)]`).
/// Relies on the ink! attribute kind, so while it catches all unknown ink! attribute macros (e.g. `#[ink::xyz]`),
/// It only catches unknown ink! attribute arguments if they're the only annotation for the attribute (e.g. `#[ink(xyz)]`),
/// It doesn't catch unknown arguments appearing in combination with valid ink! attribute macros and arguments
/// (e.g. `#[ink::contract(xyz)]` or `#[ink(storage, xyz)]`).
/// Those are handled by `validate_arg`.
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
                    .or_else(|| {
                        attr.ink_arg_name()
                            .map(|ink_arg| ink_arg.syntax().text_range())
                    })
                    .unwrap_or(attr.syntax().text_range()),
                // warning because it's possible ink! analyzer is just outdated.
                severity: Severity::Warning,
                quickfixes: Some(vec![Action::remove_attribute(attr)]),
            });
        }
    }
}

/// Ensures that ink! attribute arguments are of the right format and have values (if any)
/// of the correct type.
///
/// This utility only cares about ink! attribute arguments, not ink! attribute macros.
/// So `#[ink(env=my::env::Types)]` will pass with no complaints.
/// Unknown ink! attribute macros are handled by `ensure_no_unknown_ink_attributes`
/// while `validate_entity_attributes` is responsible for entity-level validation
/// across a group of ink! attribute macros and ink! attribute arguments
/// (e.g. flagging conflicts, incompleteness, ambiguity e.t.c).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L879-L1023>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/utils.rs#L92-L107>.
fn validate_arg(
    results: &mut Vec<Diagnostic>,
    arg: &InkArg,
    version: Version,
    attr: Option<&InkAttribute>,
) {
    let arg_name_text = arg.meta().name().to_string();
    match arg.kind() {
        // Handle unknown argument.
        InkArgKind::Unknown => {
            // Edit range for quickfix.
            let range = utils::ink_arg_and_delimiter_removal_range(arg, attr);
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
                    label: format!("Remove unknown ink! attribute argument: '{arg_name_text}'."),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::delete(range)],
                }]),
            });
        }
        arg_kind => {
            let arg_value_kind = if version.is_legacy() {
                InkArgValueKind::from(*arg_kind)
            } else {
                InkArgValueKind::from_v5(
                    *arg_kind,
                    attr.map(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Constructor)),
                )
            };
            let mut add_diagnostic = |message: String| {
                let (text, snippet) = utils::ink_arg_insert_text(*arg_kind, version, None, attr);
                results.push(Diagnostic {
                    message,
                    range: arg.text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action {
                        label: format!("Add `{arg_name_text}` argument value"),
                        kind: ActionKind::QuickFix,
                        range: arg.text_range(),
                        edits: vec![TextEdit::replace_with_snippet(
                            text,
                            arg.text_range(),
                            snippet,
                        )],
                    }]),
                });
            };
            match arg_value_kind {
                // Arguments that must have no value.
                InkArgValueKind::None => {
                    if arg.meta().eq().is_some() || arg.meta().value().is_some() {
                        results.push(Diagnostic {
                            message: format!("`{arg_name_text}` argument shouldn't have a value."),
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
                        add_diagnostic(format!(
                            "`{arg_name_text}` argument should have an `integer` (`u16`) value.",
                        ));
                    }
                }
                // `u32` values and wildcards (i.e `_` and `@` for selectors).
                InkArgValueKind::U32
                | InkArgValueKind::U32OrWildcard
                | InkArgValueKind::U32OrWildcardOrComplement => {
                    let can_be_wildcard = matches!(
                        arg_value_kind,
                        InkArgValueKind::U32OrWildcard | InkArgValueKind::U32OrWildcardOrComplement
                    );
                    let can_be_wildcard_complement =
                        arg_value_kind == InkArgValueKind::U32OrWildcardOrComplement;
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
                        add_diagnostic(format!(
                            "`{arg_name_text}` argument should have an `integer` (`u32`) {} value.",
                            if can_be_wildcard_complement {
                                ", wildcard/underscore (`_`) or wildcard complement (`@`) symbol"
                            } else if can_be_wildcard {
                                "or wildcard/underscore (`_`)"
                            } else {
                                ""
                            }
                        ));
                    }
                }
                // Arguments that should have a string value.
                InkArgValueKind::String(str_kind) => {
                    let is_valid_string = arg.value().is_some_and(|meta_value| {
                        if let Some(str_value) = meta_value.as_string() {
                            match str_kind {
                                InkArgValueStringKind::Abi if version.is_gte_v6() => {
                                    matches!(str_value.as_str(), "ink" | "sol")
                                }
                                InkArgValueStringKind::Hex => meta_value.as_hex_32().is_some(),
                                InkArgValueStringKind::Identifier => {
                                    meta_value.as_ident().is_some()
                                }
                                InkArgValueStringKind::IdentifierLike if version.is_gte_v6() => {
                                    is_ident_like(&str_value)
                                }
                                InkArgValueStringKind::Url => is_url_like(&str_value),
                                _ => true,
                            }
                        } else {
                            false
                        }
                    });
                    if !is_valid_string {
                        let value_kind_description = arg_value_kind.detail();
                        add_diagnostic(format!(
                            "`{arg_name_text}` argument should have a `string` (`&str`) value{}",
                            if !value_kind_description.is_empty() {
                                format!(" - {value_kind_description}")
                            } else {
                                ".".to_owned()
                            },
                        ));
                    }
                }
                // Arguments that should have a boolean value.
                InkArgValueKind::Bool => {
                    if arg.value().and_then(MetaValue::as_bool).is_none() {
                        add_diagnostic(format!(
                            "`{arg_name_text}` argument should have a `boolean` (`bool`) value."
                        ));
                    }
                }
                // Arguments that should have a path value.
                InkArgValueKind::Path(_) => {
                    let is_path = arg
                        .value()
                        .is_some_and(|value| value.as_path_with_inaccurate_text_range().is_some());
                    if !is_path {
                        add_diagnostic(format!(
                            "`{arg_name_text}` argument should have a `path` (e.g. `foo::bar::Baz`) value."
                        ));
                    }
                }
                // Nested arguments.
                InkArgValueKind::Arg(kind, required) => {
                    nested_arg_validator(results, arg, &kind, &[kind], required, version, attr);
                }
                InkArgValueKind::Choice(kind_1, kind_2, required) => {
                    nested_arg_validator(
                        results,
                        arg,
                        &kind_1,
                        &[kind_1, kind_2],
                        required,
                        version,
                        attr,
                    );
                }
            }
        }
    };

    fn nested_arg_validator(
        results: &mut Vec<Diagnostic>,
        arg: &InkArg,
        default_kind: &InkArgKind,
        options: &[InkArgKind],
        required: bool,
        version: Version,
        attr: Option<&InkAttribute>,
    ) {
        let name = arg.meta().name().to_string();
        let range = arg.text_range();
        if let Some(value) = arg.value() {
            results.push(Diagnostic {
                message: format!(
                    "`{name}` argument should use nested meta-item syntax (e.g. `{name}({default_kind})`)."
                ),
                range,
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: format!("Replace with `{name}({value})`"),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::replace_with_snippet(
                        format!("{name}({value})"),
                        range,
                        Some(format!("{name}(${{1:{value}}})")),
                    )],
                }]),
            });
        } else if required && arg.nested().is_none() {
            let quickfixes: Vec<_> = options
                .iter()
                .map(|option| Action {
                    label: format!("Add `({option})`"),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::replace_with_snippet(
                        format!("{name}({option})"),
                        range,
                        Some(format!("{name}(${{1:{option}}})")),
                    )],
                })
                .collect();
            results.push(Diagnostic {
                message: format!(
                    "`{name}` argument should have a nested meta-item{}.",
                    if let Some(default_kind) = options.first() {
                        format!(" (e.g. `{name}({default_kind})`)")
                    } else {
                        "".to_owned()
                    }
                ),
                range,
                severity: Severity::Error,
                quickfixes: (!quickfixes.is_empty()).then_some(quickfixes),
            });
        } else if let Some(nested_arg) = arg.nested() {
            if options.contains(nested_arg.kind()) {
                // Validate nested args.
                validate_arg(results, &nested_arg, version, attr);
            } else {
                let edit_range = nested_arg.text_range();
                let quickfixes: Vec<_> = options
                    .iter()
                    .map(|option| Action {
                        label: format!("Replace with `{option}`"),
                        kind: ActionKind::QuickFix,
                        range: edit_range,
                        edits: vec![TextEdit::replace_with_snippet(
                            format!("{option}"),
                            edit_range,
                            Some(format!("${{1:{option}}}")),
                        )],
                    })
                    .collect();
                results.push(Diagnostic {
                    message: format!(
                        "Unknown option `{nested_arg}` for `{name}` argument{}.",
                        match options.len() {
                            0 => "".to_owned(),
                            1 => format!(", expected: `{default_kind}`"),
                            _ => format!(
                                ", expected one of: {}",
                                options.iter().map(|kind| format!("`{kind}`")).join(", ")
                            ),
                        }
                    ),
                    range: edit_range,
                    severity: Severity::Error,
                    quickfixes: (!quickfixes.is_empty()).then_some(quickfixes),
                });
            }
        }
    }
}

/// Checks whether the given string includes the `://` character sequence.
fn is_url_like(value: &str) -> bool {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"://").unwrap());
    RE.is_match(value)
}

/// Checks whether the given string is an "identifier-like" string.
///
/// # Note
///
/// The string is considered to be "identifier-like" if:
/// - It begins with an alphabetic character, underscore or dollar sign
/// - It only contains alphanumeric characters, underscores and dollar signs
///
/// References:
/// - <https://github.com/use-ink/ink/blob/5174b68007c91bd44440c73a98a15c5009a0143b/crates/ink/ir/src/ir/utils.rs#L196-L199>
/// - <https://doc.rust-lang.org/reference/identifiers.html>
/// - <https://docs.soliditylang.org/en/latest/grammar.html#a4.SolidityLexer.Identifier>
fn is_ident_like(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|c| c.is_alphabetic() || c == '$' || c == '_')
        && name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '$' || c == '_')
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
            if *macro_kind != InkMacroKind::Unknown && seen_macros.contains(macro_kind) {
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
            if *arg_kind != InkArgKind::Unknown && seen_args.contains(arg_kind) {
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

/// Ensures that a group of sibling ink! attributes and/or arguments declare a valid ink! entity.
///
/// In addition to straight forward validation of conflicts
/// (e.g. both `contract` and `trait_definition` applied to the same item,
/// `anonymous` combined with `message`  or `derive` on a `contract` attribute),
/// 3 more cases are validated:
///
/// 1. Wrong order of otherwise valid attributes and/or arguments
///    (e.g. `payable` before `message` or `anonymous` before `event`).
///
/// 2. Incompleteness (e.g. `anonymous` without `event` or `derive` without `storage_item`
///    attribute macro, or a `chain_extension` macro without an `extension` argument in ink! v5).
///
/// 3. Ambiguity (e.g. `payable` with neither `constructor` nor `message` or
///    `keep_attr` with neither `contract` nor `trait_definition` attribute macros).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L613-L658>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L829-L872>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L154-L167>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L829-L873>.
fn validate_entity_attributes(
    results: &mut Vec<Diagnostic>,
    attrs: &[InkAttribute],
    version: Version,
) {
    let has_chain_extension_parent = |node: &SyntaxNode| {
        ink_analyzer_ir::ink_closest_ancestors::<ChainExtension>(node)
            .next()
            .is_some()
    };

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
        let primary_attribute_kind_suggestions = utils::primary_ink_attribute_kind_suggestions(
            *primary_ink_attr_candidate.kind(),
            version,
        );

        let find_arg = |arg_kind: InkArgKind| {
            attrs
                .iter()
                .find_map(|attr| attr.args().iter().find(|arg| *arg.kind() == arg_kind))
        };

        // ink! >= 5.x-only pedantic validation (i.e. validation that cannot be properly expressed/declared
        // using the existing generic utilities).
        // NOTE: It's intentionally performed based on the primary attribute to keep diagnostics less noisy.
        if version.is_gte_v5() {
            // `anonymous` and `signature_topic` arguments conflict.
            // Ref: <https://paritytech.github.io/ink/ink/attr.event.html>
            if matches!(
                primary_ink_attr_candidate.kind(),
                InkAttributeKind::Macro(InkMacroKind::Event)
                    | InkAttributeKind::Arg(
                        InkArgKind::Event | InkArgKind::Anonymous | InkArgKind::SignatureTopic
                    )
            ) {
                if let (Some(anonymous_arg), Some(signature_topic_arg)) = (
                    find_arg(InkArgKind::Anonymous),
                    find_arg(InkArgKind::SignatureTopic),
                ) {
                    let (first_arg, second_arg) = if anonymous_arg.text_range().start()
                        < signature_topic_arg.text_range().start()
                    {
                        (anonymous_arg, signature_topic_arg)
                    } else {
                        (signature_topic_arg, anonymous_arg)
                    };
                    // Edit range for quickfix.
                    let range = utils::ink_arg_and_delimiter_removal_range(second_arg, None);
                    results.push(Diagnostic {
                        message: format!(
                            "ink! attribute argument `{}` conflicts with the ink! attribute argument `{}` for this item.",
                            second_arg.kind(),
                            first_arg.kind(),
                        ),
                        range: second_arg.text_range(),
                        severity: Severity::Error,
                        quickfixes: Some(vec![Action {
                            label: format!(
                                "Remove ink! `{}` attribute argument.",
                                second_arg.kind()
                            ),
                            kind: ActionKind::QuickFix,
                            range,
                            edits: vec![TextEdit::delete(range)],
                        }]),
                    });
                }
            }

            // For ink! >= 6.x, chain extensions are deprecated.
            // Ref: <https://github.com/use-ink/ink/pull/2621>
            if version.is_gte_v6()
                && matches!(
                    primary_ink_attr_candidate.kind(),
                    InkAttributeKind::Macro(InkMacroKind::ChainExtension)
                        | InkAttributeKind::Arg(
                            InkArgKind::Extension | InkArgKind::Function | InkArgKind::HandleStatus
                        )
                )
            {
                match primary_ink_attr_candidate.kind() {
                    InkAttributeKind::Macro(_) => {
                        results.push(Diagnostic {
                            message: "ink! chain extensions are deprecated. \
                                See https://github.com/use-ink/ink/pull/2621 for details."
                                .to_owned(),
                            range: primary_ink_attr_candidate.syntax().text_range(),
                            severity: Severity::Error,
                            quickfixes: Some(
                                ink_analyzer_ir::parent_ast_item(
                                    primary_ink_attr_candidate.syntax(),
                                )
                                .map(|item| {
                                    vec![
                                        Action::remove_attribute_with_label(
                                            &primary_ink_attr_candidate,
                                            "Remove deprecated ink! `chain extension` attribute."
                                                .to_owned(),
                                        ),
                                        Action::remove_item_with_label(
                                            item.syntax(),
                                            "Remove deprecated ink! `chain extension` item."
                                                .to_owned(),
                                        ),
                                    ]
                                })
                                .unwrap_or_else(|| {
                                    vec![Action::remove_attribute_with_label(
                                        &primary_ink_attr_candidate,
                                        "Remove deprecated ink! `chain extension` attribute."
                                            .to_owned(),
                                    )]
                                }),
                            ),
                        });
                    }
                    InkAttributeKind::Arg(arg_kind) => {
                        if let Some(ext_arg) = find_arg(*arg_kind) {
                            // Edit range for quickfix.
                            let edit_range =
                                utils::ink_arg_and_delimiter_removal_range(ext_arg, None);
                            results.push(Diagnostic {
                                message: "ink! chain extensions are deprecated. \
                                See https://github.com/use-ink/ink/pull/2621 for details."
                                    .to_owned(),
                                range: ext_arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!(
                                        "Remove deprecated ink! `{}` attribute argument.",
                                        ext_arg.kind()
                                    ),
                                    kind: ActionKind::QuickFix,
                                    range: ext_arg.text_range(),
                                    edits: vec![TextEdit::delete(edit_range)],
                                }]),
                            });
                        }
                    }
                }

                // Bail if the primary attribute is deprecated.
                return;
            }

            // `chain_extension` macro without an `extension` argument is incomplete.
            // Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html#macro-attributes>
            // Ref: <https://github.com/paritytech/ink/pull/1958>
            if *primary_ink_attr_candidate.kind()
                == InkAttributeKind::Macro(InkMacroKind::ChainExtension)
                && find_arg(InkArgKind::Extension).is_none()
            {
                let range = primary_ink_attr_candidate.syntax().text_range();
                results.push(Diagnostic {
                    message: "Missing required ink! `extension` argument.".to_owned(),
                    range,
                    severity: Severity::Error,
                    quickfixes: utils::first_ink_arg_insert_offset_and_affixes(
                        &primary_ink_attr_candidate,
                    )
                    .map(|(insert_offset, prefix, suffix)| {
                        let (edit, snippet) =
                            utils::ink_arg_insert_text(InkArgKind::Extension, version, None, None);
                        vec![Action {
                            label: "Add ink! `extension` argument.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range,
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
                        }]
                    }),
                });
            }

            // For ink! v5 only, `extension` argument for chain extension functions was replaced by `function`.
            // Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html#method-attributes>
            // Ref: <https://github.com/paritytech/ink/pull/1958>
            // NOTE: This is different from the new `extension` argument now use with the `chain_extension` attribute macro.
            if version.is_v5()
                && *primary_ink_attr_candidate.kind()
                    == InkAttributeKind::Arg(InkArgKind::Extension)
                && has_chain_extension_parent(primary_ink_attr_candidate.syntax())
            {
                if let Some(extension_arg) = find_arg(InkArgKind::Extension) {
                    results.push(Diagnostic {
                        message: "ink! attribute argument `extension` is deprecated. \
                        Use the new `function` attribute argument instead. \
                        See https://use.ink/faq/migrating-from-ink-4-to-5/#chain-extension-api-changed--support-for-multiple-chain-extensions for details.".to_owned(),
                        range: extension_arg.text_range(),
                        severity: Severity::Error,
                        quickfixes: extension_arg.name().map(|name| name.syntax().text_range()).map(|range| {
                            vec![Action {
                                label: "Replace deprecated ink! `extension` attribute argument with `function`.".to_owned(),
                                kind: ActionKind::QuickFix,
                                range: extension_arg.text_range(),
                                edits: vec![TextEdit::replace("function".to_owned(), range)],
                            }]
                        }),
                    });

                    // Bail if the primary attribute is deprecated.
                    return;
                }
            }

            // `additional_contracts` attribute argument is deprecated.
            // Ref: <https://github.com/paritytech/ink/pull/2098>
            if matches!(
                primary_ink_attr_candidate.kind(),
                InkAttributeKind::Macro(InkMacroKind::E2ETest)
                    | InkAttributeKind::Arg(InkArgKind::AdditionalContracts)
            ) {
                if let Some(additional_contracts_arg) = find_arg(InkArgKind::AdditionalContracts) {
                    // Edit range for quickfix.
                    let range =
                        utils::ink_arg_and_delimiter_removal_range(additional_contracts_arg, None);
                    results.push(Diagnostic {
                        message: "ink! attribute argument `additional_contracts` is deprecated. See https://github.com/paritytech/ink/pull/2098 for details.".to_owned(),
                        range,
                        severity: Severity::Error,
                        quickfixes: Some(vec![Action {
                            label: "Remove ink! `additional_contracts` attribute argument.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range,
                            edits: vec![TextEdit::delete(range)],
                        }]),
                    });

                    if *primary_ink_attr_candidate.kind()
                        == InkAttributeKind::Arg(InkArgKind::AdditionalContracts)
                    {
                        // Bail if the primary attribute is deprecated.
                        return;
                    }
                }
            }

            // `keep_attr` is deprecated when used as an argument for the `ink_e2e::test` macro.
            // Ref: <https://github.com/paritytech/ink/pull/1830>
            if *primary_ink_attr_candidate.kind() == InkAttributeKind::Macro(InkMacroKind::E2ETest)
            {
                if let Some(keep_attr) = find_arg(InkArgKind::KeepAttr) {
                    // Edit range for quickfix.
                    let range = utils::ink_arg_and_delimiter_removal_range(keep_attr, None);
                    results.push(Diagnostic {
                        message: "ink! attribute argument `keep_attr` is deprecated for the ink! e2e test attribute macro. See https://github.com/paritytech/ink/pull/1830 for details.".to_owned(),
                        range,
                        severity: Severity::Error,
                        quickfixes: Some(vec![Action {
                            label: "Remove ink! `keep_attr` attribute argument.".to_owned(),
                            kind: ActionKind::QuickFix,
                            range,
                            edits: vec![TextEdit::delete(range)],
                        }]),
                    });
                }
            }
        }

        // For ink! version <= 5.x, we add diagnostics for attributes that were introduced in ink! >= 6.x
        if version.is_lte_v5() {
            // Handles `contract_ref` attribute and it's `abi` arg.
            // Ref: <https://github.com/use-ink/ink/pull/2648>
            // Note: `env` arg is used by other attributes (e.g. `contract`),
            // so we don't complain about it.
            if matches!(
                primary_ink_attr_candidate.kind(),
                InkAttributeKind::Macro(InkMacroKind::ContractRef)
                    | InkAttributeKind::Arg(InkArgKind::Abi)
            ) {
                match primary_ink_attr_candidate.kind() {
                    InkAttributeKind::Macro(_) => {
                        results.push(Diagnostic {
                            message: "ink! `contract_ref` attribute requires ink! >= 6.x"
                                .to_owned(),
                            range: primary_ink_attr_candidate.syntax().text_range(),
                            severity: Severity::Error,
                            quickfixes: Some(
                                ink_analyzer_ir::parent_ast_item(
                                    primary_ink_attr_candidate.syntax(),
                                )
                                .map(|item| {
                                    vec![
                                        Action::remove_attribute_with_label(
                                            &primary_ink_attr_candidate,
                                            "Remove unsupported ink! `contract_ref` attribute."
                                                .to_owned(),
                                        ),
                                        Action::remove_item_with_label(
                                            item.syntax(),
                                            "Remove unsupported ink! `contract_ref` item."
                                                .to_owned(),
                                        ),
                                    ]
                                })
                                .unwrap_or_else(|| {
                                    vec![Action::remove_attribute_with_label(
                                        &primary_ink_attr_candidate,
                                        "Remove unsupported ink! `contract_ref` item.".to_owned(),
                                    )]
                                }),
                            ),
                        });
                    }
                    InkAttributeKind::Arg(arg_kind) => {
                        if let Some(ext_arg) = find_arg(*arg_kind) {
                            // Edit range for quickfix.
                            let edit_range =
                                utils::ink_arg_and_delimiter_removal_range(ext_arg, None);
                            results.push(Diagnostic {
                                message: "ink! `abi` attribute requires ink! >= 6.x".to_owned(),
                                range: ext_arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: "Remove unsupported ink! `abi` attribute argument."
                                        .to_owned(),
                                    kind: ActionKind::QuickFix,
                                    range: ext_arg.text_range(),
                                    edits: vec![TextEdit::delete(edit_range)],
                                }]),
                            });
                        }
                    }
                }

                // Bail if the primary attribute is deprecated.
                return;
            }

            // Handles `error` attribute.
            // Ref: <https://github.com/use-ink/ink/pull/2585>
            if *primary_ink_attr_candidate.kind() == InkAttributeKind::Macro(InkMacroKind::Error) {
                results.push(Diagnostic {
                    message: "ink! `error` attribute requires ink! >= 6.x".to_owned(),
                    range: primary_ink_attr_candidate.syntax().text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(
                        ink_analyzer_ir::parent_ast_item(primary_ink_attr_candidate.syntax())
                            .map(|item| {
                                vec![
                                    Action::remove_attribute_with_label(
                                        &primary_ink_attr_candidate,
                                        "Remove unsupported ink! `error` attribute.".to_owned(),
                                    ),
                                    Action::remove_item_with_label(
                                        item.syntax(),
                                        "Remove unsupported ink! `error` item.".to_owned(),
                                    ),
                                ]
                            })
                            .unwrap_or_else(|| {
                                vec![Action::remove_attribute_with_label(
                                    &primary_ink_attr_candidate,
                                    "Remove unsupported ink! `error` item.".to_owned(),
                                )]
                            }),
                    ),
                });

                // Bail if the primary attribute is deprecated.
                return;
            }

            // Handles `name` arg.
            if let Some(name_arg) = find_arg(InkArgKind::Name) {
                // Edit range for quickfix.
                let edit_range = utils::ink_arg_and_delimiter_removal_range(name_arg, None);
                results.push(Diagnostic {
                    message: "ink! `name` attribute requires ink! >= 6.x".to_owned(),
                    range: name_arg.text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action {
                        label: "Remove unsupported ink! `name` attribute argument.".to_owned(),
                        kind: ActionKind::QuickFix,
                        range: name_arg.text_range(),
                        edits: vec![TextEdit::delete(edit_range)],
                    }]),
                });
            }
        }

        // Used to suppress conflict errors for deprecated attributes/arguments,
        // that should already be reported as such at this point.
        let is_already_reported_as_deprecated = |arg: &InkArg| {
            // For ink! v5, deprecated `extension` on chain extension functions is typically
            // the "primary" attribute, so it early returns and doesn't need to be checked here.
            // For ink! >= 6.x, we only get here with chain extension args
            // (i.e. `extension`, `function` and `handle_status`) if they're not "primary" attribute.
            (version.is_gte_v5()
                && *primary_ink_attr_candidate.kind()
                    == InkAttributeKind::Macro(InkMacroKind::E2ETest)
                && matches!(
                    arg.kind(),
                    InkArgKind::AdditionalContracts | InkArgKind::KeepAttr
                ))
                || (version.is_gte_v6()
                    && matches!(
                        arg.kind(),
                        InkArgKind::Extension | InkArgKind::Function | InkArgKind::HandleStatus
                    ))
                || (version.is_lte_v5() && *arg.kind() == InkArgKind::Name)
        };

        // Determines the insertion offset for creating a valid "primary" attribute as the first attribute.
        let primary_attr_insert_offset_option = || {
            ink_analyzer_ir::parent_ast_item(primary_ink_attr_candidate.syntax())
                .as_ref()
                // Determines the insertion offset for the quickfix.
                .map(ast::Item::syntax)
                .map(utils::first_ink_attribute_insert_offset)
                // Defaults to inserting before the first ink! attribute if the passed list of attributes.
                .or_else(|| attrs.first().map(|it| it.syntax().text_range().start()))
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

        // Suggests possible primary ink! attributes if the primary candidate is either incomplete
        // or ambiguous or both, and its also not `namespace` (which is valid on its own).
        if !primary_attribute_kind_suggestions.is_empty() && !is_namespace {
            // Quickfix for adding an ink! attribute of the given kind as the primary attribute (if possible).
            let add_primary_ink_attribute = |attr_kind: &InkAttributeKind| {
                primary_attr_insert_offset_option().map(|insert_offset| {
                    let (insert_text, attr_desc, snippet) = match attr_kind {
                        InkAttributeKind::Arg(arg_kind) => {
                            let (edit, snippet) =
                                utils::ink_arg_insert_text(*arg_kind, version, None, None);
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
                                    utils::ink_arg_insert_text(*arg_kind, version, None, None);
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
            let is_valid_sibling_attr = || match primary_ink_attr_candidate.kind() {
                // For v4, any additional ink! attributes mixed with a "primary" ink! attribute macro
                // is always a conflict.
                InkAttributeKind::Macro(_) if version.is_legacy() => false,
                // Generally, ink! attribute macros are never mixed with other ink! attributes,
                // except for `scale_derive` and `storage_item` but only in ink! >= 5.x.
                InkAttributeKind::Macro(primary_macro_kind) => {
                    // Duplicates are handled by `ensure_no_duplicate_attributes_and_arguments`, so we don't need that logic here.
                    matches!(
                        primary_macro_kind,
                        InkMacroKind::ScaleDerive | InkMacroKind::StorageItem
                    ) && matches!(
                        attr.kind(),
                        InkAttributeKind::Macro(
                            InkMacroKind::ScaleDerive | InkMacroKind::StorageItem
                        )
                    )
                }
                // ink! attribute arguments can be mixed with other ink! attributes.
                InkAttributeKind::Arg(_) => {
                    match attr.kind() {
                        // Additional ink! attribute macros have to be
                        // potential primary attributes inorder not to conflict,
                        // in which case our we let incompleteness and ambiguity above checks take care of that case.
                        InkAttributeKind::Macro(_) => {
                            primary_attribute_kind_suggestions.contains(attr.kind())
                        }
                        // Additional ink! attribute arguments have to be valid siblings.
                        InkAttributeKind::Arg(arg_kind) => valid_sibling_args.contains(arg_kind),
                    }
                }
            };
            // Primary attribute can't conflict with itself,
            // and we ignore any conflict errors with the first attribute if it's not the primary attribute
            // in favor of placing the error (and quickfix) on the primary attribute candidate which was already done earlier.
            let is_primary_or_first_or_valid_sibling_attr = *attr == primary_ink_attr_candidate
                || (idx == 0 && !primary_candidate_first)
                || is_valid_sibling_attr();

            // Handle attribute kind level conflict.
            if !is_primary_or_first_or_valid_sibling_attr {
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
                    // Checks if this arg kind is the same as the one being used by the attribute,
                    // which is already known to not be conflicting (or at least not deserving of flagging)
                    // at this point.
                    let is_attr_kind = *attr.kind() == InkAttributeKind::Arg(*arg.kind());

                    // Ignore arg if it's already been handled at attribute level or
                    // reported as deprecated.
                    if !is_attr_kind
                        && !valid_sibling_args.contains(arg.kind())
                        && !is_already_reported_as_deprecated(arg)
                    {
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
                    .or_else(|| {
                        struct_item.struct_token().map(|it| {
                            TextRange::new(it.text_range().start(), it.text_range().start())
                        })
                    })
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

/// Ensures that item has no generic parameters.
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
                .or_else(|| {
                    fn_item
                        .default_token()
                        .or_else(|| fn_item.const_token())
                        .or_else(|| fn_item.async_token())
                        .or_else(|| fn_item.unsafe_token())
                        .or_else(|| fn_item.abi().and_then(|abi| abi.syntax().first_token()))
                        .or_else(|| fn_item.fn_token())
                        .map(|it| TextRange::new(it.text_range().start(), it.text_range().start()))
                })
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
                .or_else(|| {
                    trait_item
                        .unsafe_token()
                        .or_else(|| trait_item.auto_token())
                        .or_else(|| trait_item.trait_token())
                        .map(|it| TextRange::new(it.text_range().start(), it.text_range().start()))
                })
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
        .or_else(|| {
            item.syntax()
                .siblings(ink_analyzer_ir::syntax::Direction::Prev)
                .find_map(ast::Impl::cast)
                .or_else(|| {
                    item.syntax()
                        .siblings(ink_analyzer_ir::syntax::Direction::Next)
                        .find_map(ast::Impl::cast)
                })
        })
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
        .or_else(|| {
            ink_analyzer_ir::ink_ancestors::<Contract>(item.syntax())
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
                                prefix.as_deref(),
                                suffix.as_deref(),
                            )]
                        },
                    )
                })
        }),
    })
}

/// Ensures that only valid quasi-direct ink! attribute descendants
/// (i.e. ink! descendants without any ink! ancestors).
pub fn ensure_valid_quasi_direct_ink_descendants_by_kind<T>(
    results: &mut Vec<Diagnostic>,
    item: &T,
    attr_kind: InkAttributeKind,
    version: Version,
    ink_scope_name: &str,
) where
    T: InkEntity,
{
    let valid_direct_descendants: Vec<_> =
        utils::valid_quasi_direct_descendant_ink_macros(attr_kind, version)
            .into_iter()
            .map(InkAttributeKind::Macro)
            .chain(
                utils::valid_quasi_direct_descendant_ink_args(attr_kind, version)
                    .into_iter()
                    .map(InkAttributeKind::Arg),
            )
            .collect();
    let allows_none = valid_direct_descendants.is_empty();
    let allows_only_scale_derive = !allows_none
        && !valid_direct_descendants
            .iter()
            .any(|attr_kind| *attr_kind != InkAttributeKind::Macro(InkMacroKind::ScaleDerive));
    if allows_none || allows_only_scale_derive {
        ensure_no_ink_descendants(results, item, ink_scope_name, allows_only_scale_derive)
    } else {
        let is_valid_quasi_direct_descendant = |attr: &InkAttribute| {
            valid_direct_descendants.contains(attr.kind())
                // Suppress scope warnings for deprecated `extension` args on `chain extension`
                // associated functions to reduce noise, because there will be a deprecation warning
                // (and quickfix) added by `validate_entity_attributes`.
                || (version.is_v5()
                    && attr_kind == InkAttributeKind::Macro(InkMacroKind::ChainExtension)
                    && *attr.kind() == InkAttributeKind::Arg(InkArgKind::Extension))
        };
        ensure_valid_quasi_direct_ink_descendants(results, item, is_valid_quasi_direct_descendant)
    }
}

/// Ensures that only valid quasi-direct ink! attribute descendants
/// (i.e. ink! descendants without any ink! ancestors).
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
        // Only show ink! scope diagnostics for the "primary" ink! attribute for its parent item.
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
pub fn ensure_no_ink_descendants<T>(
    results: &mut Vec<Diagnostic>,
    item: &T,
    ink_scope_name: &str,
    allow_scale_derive: bool,
) where
    T: InkEntity,
{
    for attr in item.tree().ink_attrs_descendants().filter(|it| {
        !matches!(
            it.kind(),
            InkAttributeKind::Macro(InkMacroKind::Unknown)
                | InkAttributeKind::Arg(InkArgKind::Unknown)
        ) && (!allow_scale_derive
            || *it.kind() != InkAttributeKind::Macro(InkMacroKind::ScaleDerive))
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
    let trait_impl =
        resolution::external_trait_impl_by_name(&name, trait_name, crate_qualifiers, ref_node);
    match trait_impl {
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
pub fn ensure_impl_scale_codec_traits(
    adt: &ast::Adt,
    message_prefix: &str,
    version: Version,
) -> Option<Diagnostic> {
    // `scale_derive` attribute for ink! >= 5.x (if any).
    let scale_derive_attr = if version.is_legacy() {
        None
    } else {
        ink_analyzer_ir::ink_attrs(adt.syntax())
            .find(|attr| *attr.kind() == InkAttributeKind::Macro(InkMacroKind::ScaleDerive))
    };
    // Standalone derive attribute (if any).
    let standalone_derive_attr = adt.attrs().find(|attr| {
        attr.path()
            .is_some_and(|path| path.to_string().trim() == "derive")
    });

    // Utilities for extracting derive attribute meta items.
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
        .map(utils::token_tree_to_non_delimited_meta_string);

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
                    .filter_map(|node| {
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
                        if ast::TokenTree::can_cast(node.kind()) && is_after_derive() {
                            ast::TokenTree::cast(node)
                                .as_ref()
                                .map(utils::token_tree_to_non_delimited_meta_string)
                        } else {
                            None
                        }
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
    let unimplemented_traits: Vec<_> = ([
        (
            "Encode",
            &resolution::SCALE_QUALIFIERS,
            "scale::Encode",
            InkArgKind::Encode,
        ),
        (
            "Decode",
            &resolution::SCALE_QUALIFIERS,
            "scale::Decode",
            InkArgKind::Decode,
        ),
        (
            "TypeInfo",
            &resolution::SCALE_INFO_QUALIFIERS,
            "scale_info::TypeInfo",
            InkArgKind::TypeInfo,
        ),
    ] as [(&str, &[&str], &str, InkArgKind); 3])
        .into_iter()
        .filter_map(|(trait_name, qualifiers, trait_path, arg_kind)| {
            // Finds derived trait implementation for the custom type (if any).
            let is_gte_v5_derived = version.is_gte_v5()
                && scale_derive_attr
                    .as_ref()
                    .is_some_and(|attr| attr.args().iter().any(|arg| *arg.kind() == arg_kind));
            let is_derived = || {
                derived_items.as_ref().is_some_and(|item_paths| {
                    item_paths.iter().any(|path| {
                        resolution::is_external_crate_item(
                            trait_name,
                            path,
                            qualifiers,
                            adt.syntax(),
                        )
                    })
                })
            };
            // Finds trait implementation for the custom type (if any).
            let is_implemented = || {
                adt.name()
                    .as_ref()
                    .map(ToString::to_string)
                    .zip(adt.syntax().ancestors().last())
                    .and_then(|(error_code_name, ref_node)| {
                        resolution::external_trait_impl_by_name(
                            &error_code_name,
                            trait_name,
                            qualifiers,
                            &ref_node,
                        )
                    })
                    .is_some()
            };

            (!is_gte_v5_derived && !is_derived() && !is_implemented())
                .then_some((trait_path, arg_kind))
        })
        .collect();

    // Returns diagnostic for unimplemented SCALE codec traits (if any).
    (!unimplemented_traits.is_empty()).then(|| {
        // Determines the insert text, range and snippet for adding a derive implementation.
        let trait_paths_plain = unimplemented_traits
            .iter()
            .map(|(path, arg_kind)| {
                if version.is_legacy() {
                    path.to_string()
                } else {
                    arg_kind.to_string()
                }
            })
            .join(", ");
        let trait_paths_snippet = unimplemented_traits
            .iter()
            .enumerate()
            .map(|(idx, (path, arg_kind))| {
                format!(
                    "${{{}:{}}}",
                    idx + 1,
                    if version.is_legacy() {
                        path.to_string()
                    } else {
                        arg_kind.to_string()
                    }
                )
            })
            .join(", ");
        let trait_paths_display = unimplemented_traits
            .iter()
            .map(|(path, _)| format!("`{path}`"))
            .join(", ");

        // Either updates an existing `scale_derive` attribute (for ink! >= 5.x) or
        // standalone `derive` attribute (for ink! <= 4.x), or creates a new one.
        let (target_attr, attr_path) = if version.is_legacy() {
            (standalone_derive_attr.as_ref(), "derive")
        } else {
            (
                scale_derive_attr.as_ref().map(|attr| attr.ast()),
                "ink::scale_derive",
            )
        };

        let (insert_text, insert_range, insert_snippet) = target_attr
            .as_ref()
            .map(|attr| {
                let meta_prefix = if version.is_legacy() {
                    standalone_derive_meta.as_ref().map(|meta| {
                        format!(
                            "{meta}{}",
                            if meta.trim_end().ends_with(',') {
                                ""
                            } else {
                                ", "
                            }
                        )
                    })
                } else {
                    None
                };
                (
                    format!(
                        "#[{attr_path}({}{trait_paths_plain})]",
                        meta_prefix.as_deref().unwrap_or_default()
                    ),
                    attr.syntax().text_range(),
                    format!(
                        "#[{attr_path}({}{trait_paths_snippet})]",
                        meta_prefix.as_deref().unwrap_or_default()
                    ),
                )
            })
            .unwrap_or_else(|| {
                (
                    format!("#[{attr_path}({trait_paths_plain})]"),
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
                    format!("#[{attr_path}({trait_paths_snippet})]"),
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
    use ink_analyzer_ir::{syntax::Direction, InkFile, MinorVersion};
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

    fn parse_first_ink_attr(code: &str) -> InkAttribute {
        InkFile::parse(code)
            .tree()
            .ink_attrs_in_scope()
            .next()
            .unwrap()
    }

    fn parse_all_ink_attrs(code: &str) -> Vec<InkAttribute> {
        InkFile::parse(code)
            .tree()
            .ink_attrs_in_scope()
            // Filter out attributes marked with `// ink-test-skip`.
            .filter(|attr| {
                let next_comment = attr
                    .syntax()
                    .siblings_with_tokens(Direction::Next)
                    .find(|elem| elem.kind() == SyntaxKind::COMMENT);
                next_comment.map_or(true, |comment| {
                    !comment.to_string().contains("ink-test-skip")
                })
            })
            .collect()
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
            [
                // Changed in v5.
                // Ref: <https://github.com/paritytech/ink/pull/1958>
                // `chain_extension` requires an `extension` argument in v5.
                quote_as_str! {
                    #[ink::chain_extension]
                },
                // `extension` was replaced with `function`.
                quote_as_str! {
                    #[ink(extension=1, handle_status=true)]
                },
                // Deprecated in v5.
                // `additional_contracts` is deprecated.
                // Ref: <https://github.com/paritytech/ink/pull/2098>
                quote_as_str! {
                    #[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml")]
                },
                quote_as_str! {
                    #[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=my::env::Types, keep_attr="foo,bar")]
                },
                // `keep_attr` for `ink_e2e::test` is deprecated.
                // Ref: <https://github.com/paritytech/ink/pull/1830>
                quote_as_str! {
                    #[ink_e2e::test(keep_attr="foo,bar")]
                },
            ]
            .into_iter()
            .chain(valid_attributes_versioned!(@lte v5))
        };
        (v5) => {
            [
                // Chain extension API changes.
                // Ref: <https://github.com/paritytech/ink/pull/1958>
                quote_as_str! {
                    #[ink::chain_extension(extension = 1)]
                },
                quote_as_str! {
                    #[ink::chain_extension(extension = 0x1)]
                },
                quote_as_str! {
                    #[ink(function = 1)]
                },
                quote_as_str! {
                    #[ink(function = 0x1)]
                },
                quote_as_str! {
                    #[ink(function=1, handle_status=true)] // `handle_status` is incomplete without `function`.
                },

            ]
            .into_iter()
            .chain(valid_attributes_versioned!(@lte v5))
            .chain(valid_attributes_versioned!(@gte v5))
        };
        (v6) => {
            [
                // `contract_ref` attribute macro.
                // Ref: <https://github.com/use-ink/ink/pull/2648>
                quote_as_str! {
                    #[ink::contract_ref]
                },
                quote_as_str! {
                    #[ink::contract_ref(abi="ink")]
                },
                quote_as_str! {
                    #[ink::contract_ref(abi="sol")]
                },
                quote_as_str! {
                    #[ink::contract_ref(abi="sol", env=my::env::Types)]
                },
                // `name` attribute argument.
                // Ref: <https://github.com/use-ink/ink/pull/2577/>
                quote_as_str! {
                    #[ink(message, name="name")]
                },
                quote_as_str! {
                    #[ink(message, name="$my_Name1")]
                },
                quote_as_str! {
                    #[ink(constructor, name="name")]
                },
                quote_as_str! {
                    #[ink(event, name="name")]
                },
                quote_as_str! {
                    #[ink::event(name="name")]
                },
                // `error` attribute macro.
                // Ref: <https://github.com/use-ink/ink/pull/2585>
                quote_as_str! {
                    #[ink::error]
                },
            ]
            .into_iter()
            .chain(valid_attributes_versioned!(@gte v5))
        };
        (@lte v5) => {
            [
                // Deprecated in v6.
                // Chain extensions are deprecated.
                // Ref: <https://github.com/use-ink/ink/pull/2621>
                quote_as_str! {
                    #[ink(extension=1)]
                },
                quote_as_str! {
                    #[ink(extension=0xA)] // hex encoded.
                },
            ]
        };
        (@gte v5) => {
            [
                // Wildcard complement selector.
                // Ref: <https://github.com/paritytech/ink/pull/1708>
                quote_as_str! {
                    #[ink(message, selector=@)] // message is required, otherwise this would be incomplete/invalid.
                },
                // Events 2.0
                // Ref: <https://github.com/paritytech/ink/pull/1827>
                // Ref: <https://github.com/paritytech/ink/pull/2031>
                quote_as_str! {
                    #[ink::event]
                },
                quote_as_str! {
                    #[ink::event(anonymous)]
                },
                quote_as_str! {
                    #[ink::event(signature_topic = "1111111111111111111111111111111111111111111111111111111111111111")]
                },
                quote_as_str! {
                    #[ink(event, signature_topic = "1111111111111111111111111111111111111111111111111111111111111111")]
                },
                quote_as_str! {
                    #[ink::event(signature_topic = "0x1111111111111111111111111111111111111111111111111111111111111111")]
                },
                // E2E testing backends.
                quote_as_str! {
                    #[ink_e2e::test(backend(node))]
                },
                quote_as_str! {
                    #[ink_e2e::test(backend(node(url = "ws://127.0.0.1:9000")))]
                },
                quote_as_str! {
                    #[ink_e2e::test(backend(runtime_only))]
                },
                quote_as_str! {
                    #[ink_e2e::test(backend(runtime_only(sandbox = ink_e2e::MinimalSandbox)))]
                },
                quote_as_str! {
                    #[ink_e2e::test(environment = ink::env::DefaultEnvironment, backend(node))]
                },
            ]
        };
    }
    macro_rules! valid_attributes {
        ($version: tt) => {
            [
                // ink! attribute macros.
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
                // Arguments that should have a boolean value.
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
            .into_iter()
            .chain(valid_attributes_versioned!($version))
        };
        () => {
            valid_attributes!(v6)
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
        let expected_quickfixes = [
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
            verify_actions(code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn valid_argument_works() {
        // NOTE: This test only cares about ink! attribute arguments not macros,
        // See `validate_arg` doc.
        for (version, attributes) in versioned_fixtures!(valid_attributes) {
            for code in attributes {
                let attr = parse_first_ink_attr(code);

                let mut results = Vec::new();
                for arg in attr.args() {
                    validate_arg(&mut results, arg, version, Some(&attr));
                }
                assert!(
                    results.is_empty(),
                    "attribute: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn invalid_argument_fails() {
        // NOTE: This test only cares about ink! attribute arguments not macros,
        // See `validate_arg` doc.
        for (version, fixtures) in [
            (
                Version::Legacy,
                vec![
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
                        // bad environment.
                        r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment)]"#,
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "environment = ink::env::DefaultEnvironment",
                                start_pat: Some("<-environment"),
                                end_pat: Some("environment"),
                            }],
                        }],
                    ),
                ],
            ),
            (
                Version::V5(MinorVersion::Base),
                vec![
                    (
                        // wildcard complement selector is invalid for constructors.
                        "#[ink(constructor, selector=@)]",
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "selector = 1",
                                start_pat: Some("<-selector=@"),
                                end_pat: Some("selector=@"),
                            }],
                        }],
                    ),
                    (
                        "#[ink::event(signature_topic)]", // missing signature_topic.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "signature_topic = ",
                                start_pat: Some("<-signature_topic"),
                                end_pat: Some("signature_topic"),
                            }],
                        }],
                    ),
                    (
                        "#[ink::event(signature_topic = 1)]", // bad signature_topic.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "signature_topic = ",
                                start_pat: Some("<-signature_topic"),
                                end_pat: Some("signature_topic = 1"),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink::event(signature_topic = "1111111111111111")]"#, // too short.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "signature_topic = ",
                                start_pat: Some("<-signature_topic"),
                                end_pat: Some(r#"signature_topic = "1111111111111111""#),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink::event(signature_topic = "0x1111111111111111")]"#, // too short.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "signature_topic = ",
                                start_pat: Some("<-signature_topic"),
                                end_pat: Some(r#"signature_topic = "0x1111111111111111""#),
                            }],
                        }],
                    ),
                    (
                        "#[ink(function, handle_status=true)]", // bad `function` value.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "function = 1",
                                start_pat: Some("<-function"),
                                end_pat: Some("function"),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink_e2e::test(environment, backend(node))]"#, // bad environment.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "environment = ink::env::DefaultEnvironment",
                                start_pat: Some("<-environment"),
                                end_pat: Some("environment"),
                            }],
                        }],
                    ),
                    (
                        "#[ink_e2e::test(backend)]", // missing nested arg (i.e. node|runtime_only).
                        vec![
                            TestResultAction {
                                label: "Add `(node)`",
                                edits: vec![TestResultTextRange {
                                    text: "(node)",
                                    start_pat: Some("<-backend"),
                                    end_pat: Some("backend"),
                                }],
                            },
                            TestResultAction {
                                label: "Add `(runtime_only)`",
                                edits: vec![TestResultTextRange {
                                    text: "(runtime_only)",
                                    start_pat: Some("<-backend"),
                                    end_pat: Some("backend"),
                                }],
                            },
                        ],
                    ),
                    (
                        "#[ink_e2e::test(backend = node)]", // node should be a nested meta-item (i.e. `backend(node)`).
                        vec![TestResultAction {
                            label: "Replace with",
                            edits: vec![TestResultTextRange {
                                text: "backend(node)",
                                start_pat: Some("<-backend"),
                                end_pat: Some("node"),
                            }],
                        }],
                    ),
                    (
                        "#[ink_e2e::test(backend(xyz))]", // bad value for backend.
                        vec![
                            TestResultAction {
                                label: "Replace with",
                                edits: vec![TestResultTextRange {
                                    text: "node",
                                    start_pat: Some("<-xyz"),
                                    end_pat: Some("xyz"),
                                }],
                            },
                            TestResultAction {
                                label: "Replace with",
                                edits: vec![TestResultTextRange {
                                    text: "runtime_only",
                                    start_pat: Some("<-xyz"),
                                    end_pat: Some("xyz"),
                                }],
                            },
                        ],
                    ),
                    (
                        "#[ink_e2e::test(backend(node(url)))]", // missing value.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: r#"url = "ws://127.0.0.1:9000""#,
                                start_pat: Some("<-url"),
                                end_pat: Some("url"),
                            }],
                        }],
                    ),
                    (
                        "#[ink_e2e::test(backend(node(url = true)))]", // bad value.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: r#"url = "ws://127.0.0.1:9000""#,
                                start_pat: Some("<-url"),
                                end_pat: Some("url = true"),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink_e2e::test(backend(node(url = "localhost")))]"#, // not a valid url.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: r#"url = "ws://127.0.0.1:9000""#,
                                start_pat: Some("<-url"),
                                end_pat: Some(r#"url = "localhost""#),
                            }],
                        }],
                    ),
                    (
                        "#[ink_e2e::test(backend(runtime_only(sandbox)))]", // missing value.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "sandbox = ink_e2e::MinimalSandbox",
                                start_pat: Some("<-sandbox"),
                                end_pat: Some("sandbox"),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink_e2e::test(backend(runtime_only(sandbox = "ink_e2e::MinimalSandbox")))]"#, // bad value, should be a path.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "sandbox = ink_e2e::MinimalSandbox",
                                start_pat: Some("<-sandbox"),
                                end_pat: Some(r#""ink_e2e::MinimalSandbox""#),
                            }],
                        }],
                    ),
                ],
            ),
            (
                Version::V6,
                vec![
                    (
                        r#"#[ink::event(abi = "move")]"#, // invalid ABI.
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: "abi = ",
                                start_pat: Some("<-abi"),
                                end_pat: Some(r#"abi = "move""#),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink(name = "")]"#, // not "identifier-like".
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: r#"name = "name""#,
                                start_pat: Some("<-name"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink(name = "::name")]"#, // not "identifier-like".
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: r#"name = "name""#,
                                start_pat: Some("<-name"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink(name = "my name")]"#, // not "identifier-like".
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: r#"name = "name""#,
                                start_pat: Some("<-name"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"#[ink(name = "1name")]"#, // not "identifier-like".
                        vec![TestResultAction {
                            label: "argument value",
                            edits: vec![TestResultTextRange {
                                text: r#"name = "name""#,
                                start_pat: Some("<-name"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                ],
            ),
        ] {
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
                            text: "env = ink::env::DefaultEnvironment",
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
                            text: "environment = ink::env::DefaultEnvironment",
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
                            text: "env = ink::env::DefaultEnvironment",
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
            ]
            .into_iter()
            .chain(fixtures)
            {
                let attr = parse_first_ink_attr(code);

                let mut results = Vec::new();
                for arg in attr.args() {
                    validate_arg(&mut results, arg, version, Some(&attr));
                }

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
    fn valid_entity_attributes_works() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        for (version, attributes) in versioned_fixtures!(valid_attributes) {
            for code in attributes {
                let attrs = parse_all_ink_attrs(code);

                let mut results = Vec::new();
                validate_entity_attributes(&mut results, &attrs, version);
                assert!(
                    results.is_empty(),
                    "attributes: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn invalid_entity_attributes_fails() {
        // NOTE: Unknown attributes are ignored by this test,
        // See `ensure_no_duplicate_attributes_and_arguments` doc.
        let fixtures_lte_v5 = [
            (
                // `contract_ref` is unsupported in ink! <= 5.x.
                "#[ink::contract_ref]",
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(""),
                        end_pat: Some("#[ink::contract_ref]"),
                    }],
                }],
            ),
            (
                // `contract_ref` is unsupported in ink! <= 5.x.
                r#"#[ink::contract_ref(abi="sol")]
                pub trait Callee {}
                "#,
                vec![
                    TestResultAction {
                        label: "Remove unsupported",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some(""),
                            end_pat: Some(")]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove unsupported",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some(""),
                            end_pat: Some("pub trait Callee {}"),
                        }],
                    },
                ],
            ),
            (
                // `abi` is unsupported in ink! <= 5.x.
                r#"#[ink(abi="sol")]"#,
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(""),
                        end_pat: Some(")]"),
                    }],
                }],
            ),
            (
                // `error` is unsupported in ink! <= 5.x.
                "#[ink::error]",
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(""),
                        end_pat: Some("#[ink::error]"),
                    }],
                }],
            ),
            (
                // `error` is unsupported in ink! <= 5.x.
                r"#[ink::error]
                pub struct Error;
                ",
                vec![
                    TestResultAction {
                        label: "Remove unsupported",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some(""),
                            end_pat: Some("#[ink::error]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove unsupported",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some(""),
                            end_pat: Some("pub struct Error;"),
                        }],
                    },
                ],
            ),
            (
                // `name` is unsupported in ink! <= 5.x.
                r#"#[ink(name="myName")]"#,
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(""),
                        end_pat: Some(")]"),
                    }],
                }],
            ),
            (
                // `name` is unsupported in ink! <= 5.x.
                r#"#[ink(constructor, name="myName")]"#,
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, name"),
                        end_pat: Some(r#"name="myName""#),
                    }],
                }],
            ),
            (
                // `name` is unsupported in ink! <= 5.x.
                r#"#[ink(message, name="myName")]"#,
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, name"),
                        end_pat: Some(r#"name="myName""#),
                    }],
                }],
            ),
            (
                // `name` is unsupported in ink! <= 5.x.
                r#"#[ink(event, name="myName")]"#,
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, name"),
                        end_pat: Some(r#"name="myName""#),
                    }],
                }],
            ),
            (
                // `name` is unsupported in ink! <= 5.x.
                r#"#[ink::event(name="myName")]"#,
                vec![TestResultAction {
                    label: "Remove unsupported",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-(name="myName")"#),
                        end_pat: Some(r#"(name="myName")"#),
                    }],
                }],
            ),
        ];
        let fixtures_v5_only = [
            (
                // missing `event`.
                "#[ink(anonymous)]",
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "#[ink::event(anonymous)]",
                            start_pat: Some("<-#[ink(anonymous)]"),
                            end_pat: Some("#[ink(anonymous)]"),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "event, ",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        }],
                    },
                ],
            ),
            (
                // missing `event`.
                r#"#[ink(signature_topic="1111111111111111111111111111111111111111111111111111111111111111")]"#,
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: r#"#[ink::event(signature_topic = "1111111111111111111111111111111111111111111111111111111111111111")]"#,
                            start_pat: Some("<-#[ink("),
                            end_pat: Some(")]"),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "event, ",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        }],
                    },
                ],
            ),
            (
                // `anonymous` and `signature_topic` conflict.
                r#"#[ink::event(anonymous, signature_topic="1111111111111111111111111111111111111111111111111111111111111111")]"#,
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("#[ink::event(anonymous"),
                        end_pat: Some("<-)]"),
                    }],
                }],
            ),
            (
                // `anonymous` and `signature_topic` conflict.
                r#"
                #[ink(event)]
                #[ink(signature_topic="1111111111111111111111111111111111111111111111111111111111111111")]
                #[ink(anonymous)]
                "#,
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(anonymous)]"),
                        end_pat: Some("#[ink(anonymous)]"),
                    }],
                }],
            ),
            (
                // `function` should come first.
                "#[ink(handle_status=true, function=1)]",
                vec![TestResultAction {
                    label: "first argument",
                    edits: vec![
                        TestResultTextRange {
                            text: "function",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-, function"),
                            end_pat: Some("function=1"),
                        },
                    ],
                }],
            ),
            (
                // `function` should come first.
                r#"
                #[ink(handle_status=true)]
                #[ink(function=1)]
                "#,
                vec![TestResultAction {
                    label: "first ink! attribute",
                    edits: vec![
                        TestResultTextRange {
                            text: "#[ink(function=1)]",
                            start_pat: Some("<-#[ink(handle_status=true)]"),
                            end_pat: Some("<-#[ink(handle_status=true)]"),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(function=1)]"),
                            end_pat: Some("#[ink(function=1)]"),
                        },
                    ],
                }],
            ),
            (
                // `chain_extension` macro requires an `extension` argument in v5.
                "#[ink::chain_extension]",
                vec![TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "(extension = 1)",
                        start_pat: Some("#[ink::chain_extension"),
                        end_pat: Some("#[ink::chain_extension"),
                    }],
                }],
            ),
            (
                // conflicting `env`.
                "#[ink::chain_extension(extension=1, env=my::env::Types)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, env"),
                        end_pat: Some("my::env::Types"),
                    }],
                }],
            ),
            (
                // `extension` was replaced with `function` for chain extension `fn`s in v5.
                r#"
                #[ink::chain_extension] // ink-test-skip
                pub trait MyExtension {
                    #[ink(extension=1)]
                    fn my_extension();
                }
                "#,
                vec![TestResultAction {
                    label: "Replace",
                    edits: vec![TestResultTextRange {
                        text: "function",
                        start_pat: Some("<-extension=1"),
                        end_pat: Some("ink(extension"),
                    }],
                }],
            ),
            (
                // `extension` was replaced with `function` for chain extension `fn`s in v5,
                // so `handle_status` shouldn't be reported as an issue in this context.
                r#"
                #[ink::chain_extension] // ink-test-skip
                pub trait MyExtension {
                    #[ink(extension=1, handle_status=true)]
                    fn my_extension();
                }
                "#,
                vec![TestResultAction {
                    label: "Replace",
                    edits: vec![TestResultTextRange {
                        text: "function",
                        start_pat: Some("<-extension=1"),
                        end_pat: Some("ink(extension"),
                    }],
                }],
            ),
            (
                // `extension` was replaced with `function` for chain extension `fn`s in v5,
                // so `handle_status` shouldn't be reported as an issue in this context.
                r#"
                #[ink::chain_extension] // ink-test-skip
                pub trait MyExtension {
                    #[ink(extension=1)]
                    #[ink(handle_status=true)]
                    fn my_extension();
                }
                "#,
                vec![TestResultAction {
                    label: "Replace",
                    edits: vec![TestResultTextRange {
                        text: "function",
                        start_pat: Some("<-extension=1"),
                        end_pat: Some("ink(extension"),
                    }],
                }],
            ),
            (
                // `extension` was replaced with `function` for chain extension `fn`s in v5,
                // so `handle_status` shouldn't be reported as an issue in this context.
                r#"
                #[ink::chain_extension] // ink-test-skip
                pub trait MyExtension {
                    #[ink(handle_status=true)]
                    #[ink(extension=1)]
                    fn my_extension();
                }
                "#,
                vec![TestResultAction {
                    label: "Replace",
                    edits: vec![TestResultTextRange {
                        text: "function",
                        start_pat: Some("<-extension=1"),
                        end_pat: Some("ink(extension"),
                    }],
                }],
            ),
            (
                // `extension` was replaced with `function` in v5, but it's still
                // a valid attribute for the `chain_extension` macro,
                // so without additional context, `handle_status` is reported as a conflict here.
                "#[ink(extension=1, handle_status=true)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, handle_status=true"),
                        end_pat: Some("handle_status=true"),
                    }],
                }],
            ),
            (
                // `additional_contracts` is deprecated.
                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml")]"#,
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("#[ink_e2e::test"),
                        end_pat: Some("<-]"),
                    }],
                }],
            ),
            (
                // `keep_attr` for `ink_e2e::test` macro is deprecated.
                r#"#[ink_e2e::test(keep_attr="foo, bar")]"#,
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("#[ink_e2e::test"),
                        end_pat: Some("<-]"),
                    }],
                }],
            ),
            (
                // `additional_contracts` is deprecated.
                r#"#[ink(additional_contracts="adder/Cargo.toml flipper/Cargo.toml")]"#,
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#["),
                        end_pat: Some(")]"),
                    }],
                }],
            ),
            (
                // conflicting `env`.
                "#[ink(extension=1, env=my::env::Types)]",
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, env"),
                        end_pat: Some("env=my::env::Types"),
                    }],
                }],
            ),
            (
                // missing `function`.
                "#[ink(handle_status=true)]",
                vec![TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "function = 1, ",
                        start_pat: Some("#[ink("),
                        end_pat: Some("#[ink("),
                    }],
                }],
            ),
        ];
        for (version, fixtures) in [
            (
                Version::Legacy,
                [
                    (
                        // `extension` should come first.
                        "#[ink(handle_status=true, extension=1)]",
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
                        // `extension` should come first.
                        r#"
                        #[ink(handle_status=true)]
                        #[ink(extension=1)]
                        "#,
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
                        // missing `event`.
                        "#[ink(anonymous)]",
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
                        // conflicting `env`.
                        "#[ink::chain_extension(env=my::env::Types)]",
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
                        // conflicting `env`.
                        "#[ink(extension=1, env=my::env::Types)]",
                        vec![TestResultAction {
                            label: "Remove",
                            edits: vec![TestResultTextRange {
                                text: "",
                                start_pat: Some("<-, env"),
                                end_pat: Some("env=my::env::Types"),
                            }],
                        }],
                    ),
                    (
                        // missing `extension`.
                        "#[ink(handle_status=true)]",
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "extension = 1, ",
                                start_pat: Some("#[ink("),
                                end_pat: Some("#[ink("),
                            }],
                        }],
                    ),
                ]
                .into_iter()
                .chain(fixtures_lte_v5.clone())
                .collect(),
            ),
            (
                Version::V5(MinorVersion::Base),
                fixtures_v5_only
                    .clone()
                    .into_iter()
                    .chain(fixtures_lte_v5.clone())
                    .collect(),
            ),
            (
                Version::V5(MinorVersion::Latest),
                fixtures_v5_only
                    .into_iter()
                    .chain(fixtures_lte_v5)
                    .collect(),
            ),
            (
                Version::V6,
                vec![
                    (
                        // `chain_extension` is deprecated.
                        "#[ink::chain_extension]",
                        vec![TestResultAction {
                            label: "Remove deprecated",
                            edits: vec![TestResultTextRange {
                                text: "",
                                start_pat: Some(""),
                                end_pat: Some("#[ink::chain_extension]"),
                            }],
                        }],
                    ),
                    (
                        // `chain_extension` is deprecated.
                        r"#[ink::chain_extension]
                        pub trait MyExtension {}
                        ",
                        vec![
                            TestResultAction {
                                label: "Remove deprecated",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(""),
                                    end_pat: Some("#[ink::chain_extension]"),
                                }],
                            },
                            TestResultAction {
                                label: "Remove deprecated",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(""),
                                    end_pat: Some("pub trait MyExtension {}"),
                                }],
                            },
                        ],
                    ),
                    (
                        // `extension` is deprecated.
                        "#[ink(extension=1)]",
                        vec![TestResultAction {
                            label: "Remove deprecated",
                            edits: vec![TestResultTextRange {
                                text: "",
                                start_pat: Some(""),
                                end_pat: Some("#[ink(extension=1)]"),
                            }],
                        }],
                    ),
                    (
                        // `function` is deprecated.
                        "#[ink(function=1)]",
                        vec![TestResultAction {
                            label: "Remove deprecated",
                            edits: vec![TestResultTextRange {
                                text: "",
                                start_pat: Some(""),
                                end_pat: Some("#[ink(function=1)]"),
                            }],
                        }],
                    ),
                    (
                        // `handle_status` is deprecated.
                        "#[ink(handle_status=true)]",
                        vec![TestResultAction {
                            label: "Remove deprecated",
                            edits: vec![TestResultTextRange {
                                text: "",
                                start_pat: Some(""),
                                end_pat: Some("#[ink(handle_status=true)]"),
                            }],
                        }],
                    ),
                    (
                        // `contract_ref` should come first.
                        r#"
                        #[ink(abi="sol")]
                        #[ink::contract_ref]
                        "#,
                        vec![TestResultAction {
                            label: "first ink! attribute",
                            edits: vec![
                                TestResultTextRange {
                                    text: "#[ink::contract_ref]",
                                    start_pat: Some("<-#[ink("),
                                    end_pat: Some("<-#[ink("),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-#[ink::contract_ref]"),
                                    end_pat: Some("#[ink::contract_ref]"),
                                },
                            ],
                        }],
                    ),
                    (
                        // `contract_ref` should come first.
                        r#"
                        #[ink(env=my::env::Types)]
                        #[ink::contract_ref]
                        "#,
                        vec![TestResultAction {
                            label: "first ink! attribute",
                            edits: vec![
                                TestResultTextRange {
                                    text: "#[ink::contract_ref]",
                                    start_pat: Some("<-#[ink("),
                                    end_pat: Some("<-#[ink("),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-#[ink::contract_ref]"),
                                    end_pat: Some("#[ink::contract_ref]"),
                                },
                            ],
                        }],
                    ),
                    (
                        // ambiguous.
                        r#"#[ink(name="name")]"#,
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
                                    text: r#"#[ink::event(name = "name")]"#,
                                    start_pat: Some(""),
                                    end_pat: Some(")]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "event, ",
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
                        // `constructor` should come first.
                        r#"#[ink(name="name", constructor)]"#,
                        vec![TestResultAction {
                            label: "first argument",
                            edits: vec![
                                TestResultTextRange {
                                    text: "constructor, ",
                                    start_pat: Some("#[ink("),
                                    end_pat: Some("#[ink("),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-, constructor"),
                                    end_pat: Some(", constructor"),
                                },
                            ],
                        }],
                    ),
                    (
                        // `message` should come first.
                        r#"#[ink(name="name", message)]"#,
                        vec![TestResultAction {
                            label: "first argument",
                            edits: vec![
                                TestResultTextRange {
                                    text: "message, ",
                                    start_pat: Some("#[ink("),
                                    end_pat: Some("#[ink("),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-, message"),
                                    end_pat: Some(", message"),
                                },
                            ],
                        }],
                    ),
                    (
                        // `event` should come first.
                        r#"#[ink(name="name", event)]"#,
                        vec![TestResultAction {
                            label: "first argument",
                            edits: vec![
                                TestResultTextRange {
                                    text: "event, ",
                                    start_pat: Some("#[ink("),
                                    end_pat: Some("#[ink("),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-, event"),
                                    end_pat: Some(", event"),
                                },
                            ],
                        }],
                    ),
                    (
                        // `event` should come first.
                        r#"
                        #[ink(name="name")]
                        #[ink::event]
                        "#,
                        vec![TestResultAction {
                            label: "first ink! attribute",
                            edits: vec![
                                TestResultTextRange {
                                    text: "#[ink::event]",
                                    start_pat: Some("<-#[ink("),
                                    end_pat: Some("<-#[ink("),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-#[ink::event]"),
                                    end_pat: Some("#[ink::event]"),
                                },
                            ],
                        }],
                    ),
                ],
            ),
        ] {
            for (code, expected_quickfixes) in [
                // Single attributes.
                (
                    // conflicting `namespace`.
                    r#"#[ink::contract(namespace="my_namespace")]"#,
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
                    // conflicting `keep_attr`.
                    r#"#[ink::storage_item(keep_attr="foo,bar")]"#,
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
                    // conflicting `payable`.
                    "#[ink::test(payable)]",
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
                    // conflicting `derive`.
                    "#[ink::trait_definition(derive=false)]",
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
                    // conflicting `anonymous`.
                    "#[ink(storage, anonymous)]",
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
                    // conflicting `default`.
                    "#[ink(event, default)]",
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
                    // conflicting `selector`.
                    "#[ink(topic, selector=1)]",
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
                    // conflicting `derive`.
                    "#[ink(constructor, derive=true)]",
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
                    // conflicting `namespace`.
                    r#"#[ink(message, namespace="my_namespace")]"#,
                    vec![TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-, namespace"),
                            end_pat: Some(r#"namespace="my_namespace""#),
                        }],
                    }],
                ),
                // Multiple attributes.
                (
                    // conflicting `contract`.
                    r#"
                    #[ink::contract(env=my::env::Types)]
                    #[ink::trait_definition(namespace="my_namespace")]
                    "#,
                    vec![TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some(
                                r#"<-#[ink::trait_definition(namespace="my_namespace")]"#,
                            ),
                            end_pat: Some(r#"#[ink::trait_definition(namespace="my_namespace")]"#),
                        }],
                    }],
                ),
                (
                    // conflicting `contract`.
                    r#"
                    #[ink::contract(env=my::env::Types)]
                    #[ink_e2e::test(environment=my::env::Types)]
                    "#,
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
                    // conflicting `contract`.
                    r#"
                    #[ink::contract]
                    #[ink(message)]
                    "#,
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
                    // conflicting `event`.
                    r#"
                    #[ink(constructor, payable, default, selector=1)]
                    #[ink(event)]
                    "#,
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
                    // `topic` conflicts with `message`.
                    r#"
                    #[ink(message)]
                    #[ink(payable, selector=2, topic)]
                    "#,
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
                    // `event` should come first.
                    "#[ink(anonymous, event)]",
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
                    // `event` should come first.
                    r#"
                    #[ink(anonymous)]
                    #[ink(event)]
                    "#,
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
                    // `message` should come first.
                    "#[ink(payable, message, default, selector=1)]",
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
                    // `message` should come first.
                    r#"
                    #[ink(payable, default, selector=1)]
                    #[ink(message)]
                    "#,
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
                    // conflicts with `contract`, should be an argument.
                    r#"
                    #[ink::contract]
                    #[ink(env=my::env::Types)]
                    "#,
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
                    // conflicts with `trait_definition`, should be an argument.
                    r#"
                    #[ink::trait_definition]
                    #[ink(keep_attr="foo,bar")]
                    "#,
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
                    // conflicts with `storage_item`, should be an argument.
                    r#"
                    #[ink::storage_item]
                    #[ink(derive=false)]
                    "#,
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
                    // conflicts with `e2e test`, should be an argument.
                    r#"
                    #[ink_e2e::test]
                    #[ink(environment=my::env::Types)]
                    "#,
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
                    // incomplete and ambiguous.
                    "#[ink(payable, default, selector=1)]",
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
                    // incomplete and ambiguous.
                    r#"
                    #[ink(payable, default)]
                    #[ink(selector=1)]
                    "#,
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
                    // incomplete and ambiguous.
                    r#"#[ink(keep_attr="foo,bar")]"#,
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#"#[ink_e2e::test(keep_attr = "foo,bar")]"#,
                                start_pat: Some(r#"<-#[ink(keep_attr="foo,bar")]"#),
                                end_pat: Some(r#"#[ink(keep_attr="foo,bar")]"#),
                            }],
                        },
                    ],
                ),
                // Namespace.
                (
                    // `trait_definition` should come first, and namespace should be an argument.
                    r#"
                    #[ink(namespace="my_namespace")]
                    #[ink::trait_definition]
                    "#,
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
                    // conflicts with `trait_definition`, it should be an argument.
                    r#"
                    #[ink::trait_definition]
                    #[ink(namespace="my_namespace")]
                    "#,
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
                    // `impl` should come first.
                    r#"
                    #[ink(namespace="my_namespace")]
                    #[ink(impl)]
                    "#,
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
            ]
            .into_iter()
            .chain(fixtures)
            {
                let attrs = parse_all_ink_attrs(code);

                let mut results = Vec::new();
                validate_entity_attributes(&mut results, &attrs, version);

                // Verifies diagnostics.
                assert_eq!(
                    results.len(),
                    1,
                    "attributes: {code}, version: {:?}",
                    version
                );
                assert_eq!(
                    results[0].severity,
                    Severity::Error,
                    "attributes: {code}, version: {:?}",
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
}
