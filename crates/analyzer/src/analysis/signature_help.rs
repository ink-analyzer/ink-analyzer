//! ink! attribute signature help.

use ink_analyzer_ir::syntax::{AstNode, TextRange, TextSize};
use ink_analyzer_ir::{InkArg, InkArgKind, InkArgValueKind, InkAttributeKind, InkEntity, InkFile};
use itertools::Itertools;

use crate::analysis::utils;
use crate::Version;

/// An ink! attribute signature help.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelp {
    /// Text of the signature.
    pub label: String,
    /// Range where the signature applies.
    pub range: TextRange,
    /// Parameters of the signature.
    pub parameters: Vec<SignatureParameter>,
    /// Index of the active parameter.
    pub active_parameter: Option<usize>,
    /// Extra details about the signature.
    pub detail: Option<String>,
}

/// An ink! attribute signature parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureParameter {
    /// Range of the parameter.
    pub range: TextRange,
    /// Extra details about the parameter.
    pub detail: Option<String>,
}

/// Computes ink! attribute signature help for the given offset.
pub fn signature_help(file: &InkFile, offset: TextSize, version: Version) -> Vec<SignatureHelp> {
    let mut results = Vec::new();
    let item_at_offset = file.item_at_offset(offset);

    // Only computes signature help for ink! attributes.
    if let Some((ink_attr, ..)) = item_at_offset.normalized_parent_ink_attr() {
        // Only computes signature help if the cursor is positioned inside a token tree,
        // after the opening parenthesis (i.e. `(`) and before the closing parenthesis (i.e. `)`) (if any).
        if let Some(token_tree) = ink_attr.ast().token_tree() {
            // Opening parenthesis is required.
            let is_after_left_paren = token_tree
                .l_paren_token()
                .is_some_and(|l_paren| l_paren.text_range().end() <= offset);
            // Closing parenthesis is not required.
            let is_before_right_paren = token_tree
                .r_paren_token()
                .map_or(true, |r_paren| offset <= r_paren.text_range().start());
            let is_focused_on_arguments = token_tree.syntax().text_range().contains(offset)
                && is_after_left_paren
                && is_before_right_paren;
            if is_focused_on_arguments {
                // Determines the range where the signature help applies.
                let range = TextRange::new(
                    token_tree
                        .l_paren_token()
                        .map(|l_paren| l_paren.text_range().end())
                        .unwrap_or_else(|| token_tree.syntax().text_range().start()),
                    token_tree
                        .r_paren_token()
                        .map(|r_paren| r_paren.text_range().start())
                        .unwrap_or_else(|| token_tree.syntax().text_range().end()),
                );

                // Determines the current argument in focus if any.
                let focused_arg = ink_attr
                    .args()
                    .iter()
                    .find(|arg| arg.text_range().contains_inclusive(offset));

                // Computes signatures based on the attribute kind.
                match ink_attr.kind() {
                    // Computes signatures based on attribute arguments.
                    InkAttributeKind::Arg(arg_kind) => {
                        if arg_kind.is_entity_type() {
                            // Computes signature based on primary argument.
                            anchor_signature(&mut results, arg_kind, focused_arg, range, version);
                        } else if let Some(separate_entity_arg_kind) = item_at_offset
                            .parent_ast_item()
                            // Finds separate primary ink! attribute.
                            .and_then(|item| {
                                utils::primary_ink_attribute_candidate(ink_analyzer_ir::ink_attrs(
                                    item.syntax(),
                                ))
                            })
                            // Ignores separate ink! attribute macros and non-entity level arguments.
                            .and_then(|(attr, _)| match attr.kind() {
                                InkAttributeKind::Arg(arg_kind) => {
                                    arg_kind.is_entity_type().then_some(*arg_kind)
                                }
                                InkAttributeKind::Macro(_) => None,
                            })
                        {
                            // Computes signature based on separate primary ink! argument.
                            anchor_signature(
                                &mut results,
                                &separate_entity_arg_kind,
                                focused_arg,
                                range,
                                version,
                            );
                        } else if *arg_kind != InkArgKind::Unknown {
                            // Computes signature based on complementary argument.
                            complementary_signature(
                                &mut results,
                                arg_kind,
                                focused_arg,
                                range,
                                version,
                            );
                        } else if let Some(parent_item_kind) =
                            item_at_offset.normalized_parent_item_syntax_kind()
                        {
                            // Determines possible args by prefix or parent item kind
                            // (also accounts for parent scope).
                            let mut possible_args =
                                utils::valid_ink_args_by_syntax_kind(parent_item_kind, version);
                            if let Some(attr_parent) = ink_attr
                                .syntax()
                                .parent()
                                .filter(|it| it.kind() == parent_item_kind)
                            {
                                utils::remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
                                    &mut possible_args,
                                    &attr_parent,
                                    version,
                                );
                            }
                            for possible_arg_kind in possible_args
                                .iter()
                                .filter(|arg_kind| {
                                    focused_arg.map_or(true, |arg| {
                                        arg.name().map_or(true, |arg_name| {
                                            let name = arg_name.to_string();
                                            name.is_empty()
                                                || arg_kind.to_string().starts_with(&name)
                                        })
                                    })
                                })
                                // Replaces complementary arguments with primary arguments (if possible).
                                // Useful for easier deduplication.
                                .flat_map(|arg_kind| {
                                    if arg_kind.is_entity_type()
                                        || *arg_kind == InkArgKind::Namespace
                                    {
                                        // Namespace is special (see `complementary_signature` inline docs).
                                        vec![*arg_kind]
                                    } else {
                                        let primary_args: Vec<InkArgKind> = [*arg_kind]
                                            .into_iter()
                                            .chain(utils::valid_sibling_ink_args(
                                                InkAttributeKind::Arg(*arg_kind),
                                                version,
                                            ))
                                            .filter(|arg_kind| arg_kind.is_entity_type())
                                            .collect();
                                        if primary_args.is_empty() {
                                            vec![*arg_kind]
                                        } else {
                                            primary_args
                                        }
                                    }
                                })
                                .sorted()
                                // Deduplicates arguments by lite signature equivalence.
                                .unique_by(|arg_kind| {
                                    utils::valid_sibling_ink_args(
                                        InkAttributeKind::Arg(*arg_kind),
                                        version,
                                    )
                                    .iter()
                                    .chain([arg_kind])
                                    .map(ToString::to_string)
                                    .sorted()
                                    .join(",")
                                })
                            {
                                if possible_arg_kind.is_entity_type() {
                                    // Computes signature based on possible primary argument.
                                    anchor_signature(
                                        &mut results,
                                        &possible_arg_kind,
                                        focused_arg,
                                        range,
                                        version,
                                    );
                                } else if possible_arg_kind.is_complementary() {
                                    // Computes signature based on possible complementary argument.
                                    complementary_signature(
                                        &mut results,
                                        &possible_arg_kind,
                                        focused_arg,
                                        range,
                                        version,
                                    );
                                }
                            }
                        }
                    }
                    // Computes signatures based on attribute macros.
                    InkAttributeKind::Macro(_) => {
                        let optional_args =
                            utils::valid_sibling_ink_args(*ink_attr.kind(), version);
                        if !optional_args.is_empty() {
                            add_signature(&mut results, &optional_args, focused_arg, range);
                        }
                    }
                }
            }
        }
    }

    results
        .into_iter()
        // Deduplicate by label and range.
        .unique_by(|item| (item.label.clone(), item.range))
        .collect()
}

/// Computes signature and updates the accumulator given a list of arguments.
fn add_signature(
    results: &mut Vec<SignatureHelp>,
    args: &[InkArgKind],
    focused_arg: Option<&InkArg>,
    range: TextRange,
) {
    let mut signature = String::new();
    let mut params = Vec::new();
    let mut active_param = None;
    let mut active_param_by_prefix = None;
    let param_separator = ", ";

    // Adds arguments to signature.
    for arg_kind in args.iter().sorted() {
        let arg_value_kind = InkArgValueKind::from(*arg_kind);
        let param = format!(
            "{arg_kind}{}{arg_value_kind}",
            if arg_value_kind == InkArgValueKind::None {
                ""
            } else {
                ": "
            }
        );

        let mut start_offset = signature.len() as u32;
        if !signature.is_empty() {
            // Accounts the separator applied before the parameter.
            start_offset += param_separator.len() as u32;
        }

        // Adds parameter to signature (including the parameter separator if necessary).
        signature.push_str(&format!(
            "{}{param}",
            if !signature.is_empty() {
                param_separator
            } else {
                ""
            }
        ));

        let doc = [arg_value_kind.detail(), arg_kind.detail()]
            .iter()
            .filter(|it| !it.is_empty())
            .join("\n\n");
        params.push(SignatureParameter {
            range: TextRange::new(
                TextSize::from(start_offset),
                TextSize::from(start_offset + param.len() as u32),
            ),
            detail: (!doc.is_empty()).then_some(doc),
        });

        if active_param.is_none() {
            let idx = params.len() - 1;

            if focused_arg.is_some_and(|arg| arg.kind() == arg_kind) {
                active_param = Some(idx);
            } else if active_param_by_prefix.is_none()
                && focused_arg.is_some_and(|arg| {
                    arg.name().is_some_and(|arg_name| {
                        let name = arg_name.to_string();
                        !name.is_empty() && arg_kind.to_string().starts_with(&name)
                    })
                })
            {
                active_param_by_prefix = Some(idx);
            }
        }
    }

    // Adds signature to results (if non-empty).
    if !signature.is_empty() && !params.is_empty() {
        active_param = active_param.or(active_param_by_prefix).or(Some(0));
        results.push(SignatureHelp {
            label: signature,
            range,
            parameters: params,
            active_parameter: active_param,
            detail: None,
        });
    }
}

/// Computes signature based on a single "anchor" argument.
fn anchor_signature(
    results: &mut Vec<SignatureHelp>,
    anchor_arg: &InkArgKind,
    focused_arg: Option<&InkArg>,
    range: TextRange,
    version: Version,
) {
    // Adds valid sibling arguments and computes signature help.
    let args: Vec<InkArgKind> = [*anchor_arg]
        .into_iter()
        .chain(utils::valid_sibling_ink_args(
            InkAttributeKind::Arg(*anchor_arg),
            version,
        ))
        .collect();
    add_signature(results, &args, focused_arg, range);
}

/// Computes signature based on a single complementary argument.
fn complementary_signature(
    results: &mut Vec<SignatureHelp>,
    arg_kind: &InkArgKind,
    focused_arg: Option<&InkArg>,
    range: TextRange,
    version: Version,
) {
    // Namespace is not entity-level but it can be used alone,
    // or be used with ink! impl argument and ink! trait definition macro.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L301-L315>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/config.rs#L60-L85>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
    if *arg_kind == InkArgKind::Namespace {
        add_signature(results, &[*arg_kind], focused_arg, range);
    }

    // Determines the complementary argument's related primary arguments (if any).
    let mut primary_args = [*arg_kind]
        .into_iter()
        .chain(utils::valid_sibling_ink_args(
            InkAttributeKind::Arg(*arg_kind),
            version,
        ))
        .filter(|arg_kind| arg_kind.is_entity_type());
    if let Some(first_arg) = primary_args.next() {
        // Computes signature based on the complementary argument's related primary argument(s).
        for primary_arg in [first_arg].into_iter().chain(primary_args) {
            anchor_signature(results, &primary_arg, focused_arg, range, version);
        }
    } else {
        // Computes signature directly based on only the complementary argument.
        anchor_signature(results, arg_kind, focused_arg, range, version);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::parse_offset_at;

    #[test]
    fn signature_help_works() {
        for (code, pat, expected_results) in [
            // (code, Option<pat>, [(label, (pat_start, pat_end), [(param_pat_start, param_pat_end)])]) where:
            // code = source code,
            // pat = substring used to find the cursor offset for the signature help (see `test_utils::parse_offset_at` doc),
            // label = the label text for the signature help,
            // pat_start = substring used to find the start of the range the signature help applies to (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the range the signature help applies to (see `test_utils::parse_offset_at` doc).
            // param_pat_start = substring used to find the start of the range for the signature help parameter (see `test_utils::parse_offset_at` doc),
            // param_pat_end = substring used to find the end of the range for the signature help parameter (see `test_utils::parse_offset_at` doc).

            // Control tests.
            ("// Nothing", None, vec![]),
            ("mod my_mod {}", None, vec![]),
            // ink! attribute macros.
            ("#[ink::contract]", None, vec![]),
            ("#[ink::trait_definition]", None, vec![]),
            ("#[ink::chain_extension]", None, vec![]),
            ("#[ink::storage_item]", None, vec![]),
            ("#[ink::test]", None, vec![]),
            // ink! attribute macro complementary arguments.
            (
                "#[ink::contract()]",
                Some("contract("),
                vec![(
                    "env: impl Environment, keep_attr: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-env"), Some("Environment")),
                        (Some("<-keep_attr"), Some("&str")),
                    ],
                    0,
                )],
            ),
            (
                r#"#[ink::contract(env=my::env::Types)]"#,
                Some("contract("),
                vec![(
                    "env: impl Environment, keep_attr: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-env"), Some("Environment")),
                        (Some("<-keep_attr"), Some("&str")),
                    ],
                    0,
                )],
            ),
            (
                r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#,
                Some("keep_attr"),
                vec![(
                    "env: impl Environment, keep_attr: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-env"), Some("Environment")),
                        (Some("<-keep_attr"), Some("&str")),
                    ],
                    1,
                )],
            ),
            (
                r#"#[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar"))]"#,
                Some("namespace"),
                vec![(
                    "keep_attr: &str, namespace: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-keep_attr"), Some("keep_attr: &str")),
                        (Some("<-namespace"), Some("namespace: &str")),
                    ],
                    1,
                )],
            ),
            (
                r#"#[ink::trait_definition(namespace="my_namespace"))]"#,
                Some("trait_definition("),
                vec![(
                    "keep_attr: &str, namespace: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-keep_attr"), Some("keep_attr: &str")),
                        (Some("<-namespace"), Some("namespace: &str")),
                    ],
                    1,
                )],
            ),
            (
                "#[ink::chain_extension()]",
                Some("chain_extension("),
                vec![],
            ),
            (
                "#[ink::storage_item(derive=true)]",
                Some("#[ink::storage_item("),
                vec![(
                    "derive: bool",
                    (Some("("), Some("<-)")),
                    vec![(Some("<-derive"), Some("bool"))],
                    0,
                )],
            ),
            ("#[ink::test()]", Some("test("), vec![]),
            (
                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=my::env::Types, keep_attr="foo,bar")]"#,
                Some("environment"),
                vec![(
                    "additional_contracts: &str, environment: impl Environment, keep_attr: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (
                            Some("<-additional_contracts"),
                            Some("additional_contracts: &str"),
                        ),
                        (Some("<-environment"), Some("Environment")),
                        (Some("<-keep_attr"), Some("keep_attr: &str")),
                    ],
                    1,
                )],
            ),
            // ink! attribute arguments.
            (
                "#[ink(storage)]",
                Some("ink("),
                vec![(
                    "storage",
                    (Some("("), Some("<-)")),
                    vec![(Some("<-storage"), Some("storage"))],
                    0,
                )],
            ),
            (
                "#[ink(event, anonymous)]",
                Some("ink("),
                vec![(
                    "event, anonymous",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-event"), Some("event")),
                        (Some("<-anonymous"), Some("anonymous")),
                    ],
                    0,
                )],
            ),
            (
                "#[ink(anonymous)]",
                Some("ink("),
                vec![(
                    "event, anonymous",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-event"), Some("event")),
                        (Some("<-anonymous"), Some("anonymous")),
                    ],
                    1,
                )],
            ),
            (
                "#[ink(topic)]",
                Some("ink("),
                vec![(
                    "topic",
                    (Some("("), Some("<-)")),
                    vec![(Some("<-topic"), Some("topic"))],
                    0,
                )],
            ),
            (
                "#[ink(constructor)]",
                Some("ink("),
                vec![(
                    "constructor, default, payable, selector: u32 | _",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-constructor"), Some("constructor")),
                        (Some("<-default"), Some("default")),
                        (Some("<-payable"), Some("payable")),
                        (Some("<-selector"), Some("u32 | _")),
                    ],
                    0,
                )],
            ),
            (
                "#[ink(message)]",
                Some("ink("),
                vec![(
                    "message, default, payable, selector: u32 | _",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-message"), Some("message")),
                        (Some("<-default"), Some("default")),
                        (Some("<-payable"), Some("payable")),
                        (Some("<-selector"), Some("u32 | _")),
                    ],
                    0,
                )],
            ),
            (
                "#[ink(selector=1)]",
                Some("ink("),
                vec![
                    (
                        "constructor, default, payable, selector: u32 | _",
                        (Some("("), Some("<-)")),
                        vec![
                            (Some("<-constructor"), Some("constructor")),
                            (Some("<-default"), Some("default")),
                            (Some("<-payable"), Some("payable")),
                            (Some("<-selector"), Some("u32 | _")),
                        ],
                        3,
                    ),
                    (
                        "message, default, payable, selector: u32 | _",
                        (Some("("), Some("<-)")),
                        vec![
                            (Some("<-message"), Some("message")),
                            (Some("<-default"), Some("default")),
                            (Some("<-payable"), Some("payable")),
                            (Some("<-selector"), Some("u32 | _")),
                        ],
                        3,
                    ),
                ],
            ),
            (
                "#[ink(extension=1)]",
                Some("ink("),
                vec![(
                    "extension: u32, handle_status: bool",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-extension"), Some("u32")),
                        (Some("<-handle_status"), Some("bool")),
                    ],
                    0,
                )],
            ),
            (
                "#[ink(handle_status=true)]",
                Some("ink("),
                vec![(
                    "extension: u32, handle_status: bool",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-extension"), Some("u32")),
                        (Some("<-handle_status"), Some("bool")),
                    ],
                    1,
                )],
            ),
            (
                r#"#[ink(impl, namespace="my_namespace")]"#,
                Some("ink("),
                vec![(
                    "impl, namespace: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-impl"), Some("impl")),
                        (Some("<-namespace"), Some("&str")),
                    ],
                    0,
                )],
            ),
            (
                r#"#[ink(namespace="my_namespace")]"#,
                Some("ink("),
                vec![
                    (
                        "namespace: &str",
                        (Some("("), Some("<-)")),
                        vec![(Some("<-namespace"), Some("&str"))],
                        0,
                    ),
                    (
                        "impl, namespace: &str",
                        (Some("("), Some("<-)")),
                        vec![
                            (Some("<-impl"), Some("impl")),
                            (Some("<-namespace"), Some("&str")),
                        ],
                        1,
                    ),
                ],
            ),
            // multiple ink! attributes.
            (
                r#"
                #[ink(event)]
                #[ink(anonymous)]
                struct MyStruct {}
                "#,
                Some("ink(->"),
                vec![(
                    "event, anonymous",
                    (Some("(->"), Some("<-)->")),
                    vec![
                        (Some("<-event"), Some("event")),
                        (Some("<-anonymous"), Some("anonymous")),
                    ],
                    1,
                )],
            ),
            (
                r#"
                #[ink(constructor)]
                #[ink(selector=1)]
                fn my_fn() {}
                "#,
                Some("ink(->"),
                vec![(
                    "constructor, default, payable, selector: u32 | _",
                    (Some("ink(->"), Some("<-)]->")),
                    vec![
                        (Some("<-constructor"), Some("constructor")),
                        (Some("<-default"), Some("default")),
                        (Some("<-payable"), Some("payable")),
                        (Some("<-selector"), Some("u32 | _")),
                    ],
                    3,
                )],
            ),
            (
                r#"
                #[ink(message)]
                #[ink(payable)]
                fn my_fn() {}
                "#,
                Some("ink(->"),
                vec![(
                    "message, default, payable, selector: u32 | _",
                    (Some("ink(->"), Some("<-)]->")),
                    vec![
                        (Some("<-message"), Some("message")),
                        (Some("<-default"), Some("default")),
                        (Some("<-payable"), Some("payable")),
                        (Some("<-selector"), Some("u32 | _")),
                    ],
                    2,
                )],
            ),
            (
                r#"
                #[ink(extension=1)]
                #[ink(handle_status=true)]
                fn my_fn() {}
                "#,
                Some("ink(->"),
                vec![(
                    "extension: u32, handle_status: bool",
                    (Some("ink(->"), Some("<-)]->")),
                    vec![
                        (Some("<-extension"), Some("u32")),
                        (Some("<-handle_status"), Some("bool")),
                    ],
                    1,
                )],
            ),
            // incomplete ink! attribute arguments.
            (
                r#"
                #[ink()]
                struct MyStruct {}
                "#,
                Some("ink("),
                vec![
                    (
                        "event, anonymous",
                        (Some("("), Some("<-)")),
                        vec![
                            (Some("<-event"), Some("event")),
                            (Some("<-anonymous"), Some("anonymous")),
                        ],
                        0,
                    ),
                    (
                        "storage",
                        (Some("("), Some("<-)")),
                        vec![(Some("<-storage"), Some("storage"))],
                        0,
                    ),
                ],
            ),
            (
                r#"
                struct MyStruct {
                    #[ink()]
                    field: bool,
                }
                "#,
                Some("ink("),
                vec![(
                    "topic",
                    (Some("("), Some("<-)")),
                    vec![(Some("<-topic"), Some("topic"))],
                    0,
                )],
            ),
            (
                r#"
                #[ink()]
                fn my_fn() {}
                "#,
                Some("ink("),
                vec![
                    (
                        "constructor, default, payable, selector: u32 | _",
                        (Some("("), Some("<-)")),
                        vec![
                            (Some("<-constructor"), Some("constructor")),
                            (Some("<-default"), Some("default")),
                            (Some("<-payable"), Some("payable")),
                            (Some("<-selector"), Some("u32 | _")),
                        ],
                        0,
                    ),
                    (
                        "message, default, payable, selector: u32 | _",
                        (Some("("), Some("<-)")),
                        vec![
                            (Some("<-message"), Some("message")),
                            (Some("<-default"), Some("default")),
                            (Some("<-payable"), Some("payable")),
                            (Some("<-selector"), Some("u32 | _")),
                        ],
                        0,
                    ),
                    (
                        "extension: u32, handle_status: bool",
                        (Some("("), Some("<-)")),
                        vec![
                            (Some("<-extension"), Some("u32")),
                            (Some("<-handle_status"), Some("bool")),
                        ],
                        0,
                    ),
                ],
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
            let results = signature_help(&InkFile::parse(code), offset, Version::V4);

            // Verifies expected results.
            assert_eq!(results.len(), expected_results.len(), "code: {code}");
            for (idx, result) in results.iter().enumerate() {
                let (expected_label, (start_pat, end_pat), ..) = expected_results[idx];

                // Verified signature label.
                assert_eq!(result.label, expected_label, "code: {code}");
                // Verifies signature range.
                assert_eq!(
                    result.range,
                    TextRange::new(
                        TextSize::from(parse_offset_at(code, start_pat).unwrap() as u32),
                        TextSize::from(parse_offset_at(code, end_pat).unwrap() as u32)
                    ),
                    "code: {code}"
                );
                // Verifies parameter ranges (and extra details - if any).
                let expected_params = &expected_results[idx].2;
                let expected_active_param = &expected_results[idx].3;
                assert_eq!(result.parameters.len(), expected_params.len());
                for (idx, param) in result.parameters.iter().enumerate() {
                    let (start_pat, end_pat) = expected_params[idx];
                    // Verifies parameter range.
                    assert_eq!(
                        param.range,
                        TextRange::new(
                            TextSize::from(
                                parse_offset_at(expected_label, start_pat).unwrap() as u32
                            ),
                            TextSize::from(parse_offset_at(expected_label, end_pat).unwrap() as u32)
                        ),
                        "code: {code}"
                    );
                }
                // Verifies active parameter (if any).
                assert_eq!(
                    result.active_parameter,
                    Some(*expected_active_param),
                    "code: {code}"
                );
            }
        }
    }
}
