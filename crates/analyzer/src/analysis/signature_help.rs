//! ink! attribute signature help.

use ink_analyzer_ir::syntax::{AstNode, TextRange, TextSize};
use ink_analyzer_ir::{
    FromAST, InkArgKind, InkArgValueKind, InkAttributeKind, InkFile, IsInkEntity,
};
use itertools::{Either, Itertools};

use crate::analysis::utils;

/// An ink! attribute signature help.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelp {
    /// Text of the signature.
    pub label: String,
    /// Range where the signature applies.
    pub range: TextRange,
    /// Parameters of the signature.
    pub parameters: Option<Vec<SignatureParameter>>,
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
pub fn signature_help(file: &InkFile, offset: TextSize) -> Vec<SignatureHelp> {
    let mut results = Vec::new();
    let item_at_offset = file.item_at_offset(offset);

    // Only computes signature help for ink! attributes.
    if let Some((attr, ..)) = item_at_offset.normalized_parent_ink_attr() {
        // Only computes signature help if the cursor is positioned inside a token tree,
        // after the opening parenthesis (i.e. `(`) and before the closing parenthesis (i.e. `)`) (if any).
        if let Some(token_tree) = attr.ast().token_tree() {
            // Opening parenthesis is required.
            let is_after_left_paren = token_tree
                .l_paren_token()
                .map_or(false, |l_paren| l_paren.text_range().end() <= offset);
            // Closing parenthesis is not required.
            let is_before_right_paren = token_tree
                .r_paren_token()
                .map_or(true, |r_paren| offset <= r_paren.text_range().start());
            let is_focused_on_arguments = token_tree.syntax().text_range().contains(offset)
                && is_after_left_paren
                && is_before_right_paren;
            if is_focused_on_arguments {
                // Determines the required and optional args for the attribute (if any).
                let (required_args, optional_args): (Vec<InkArgKind>, Vec<InkArgKind>) =
                    utils::valid_sibling_ink_args(*attr.kind())
                        .into_iter()
                        .chain(match attr.kind() {
                            InkAttributeKind::Arg(arg_kind) => Some(*arg_kind),
                            InkAttributeKind::Macro(_) => None,
                        })
                        .sorted()
                        .partition_map(|arg_kind| {
                            if arg_kind.is_entity_type() {
                                Either::Left(arg_kind)
                            } else {
                                Either::Right(arg_kind)
                            }
                        });

                // Determines the range where the signature help applies.
                let range = TextRange::new(
                    token_tree
                        .l_paren_token()
                        .map_or(token_tree.syntax().text_range().start(), |l_paren| {
                            l_paren.text_range().end()
                        }),
                    token_tree
                        .r_paren_token()
                        .map_or(token_tree.syntax().text_range().end(), |r_paren| {
                            r_paren.text_range().start()
                        }),
                );

                // Determines the current argument in focus if any.
                let focused_arg = attr
                    .args()
                    .iter()
                    .find(|arg| arg.text_range().contains(offset));

                // Computes signature help.
                let mut impl_signature_help =
                    |primary_arg_option: Option<&InkArgKind>, optional_args: &[InkArgKind]| {
                        let mut signature = String::new();
                        let mut params = Vec::new();
                        let mut active_parameter = None;
                        let param_separator = ", ";

                        // Adds a parameter to the signature.
                        let mut add_param_to_signature = |arg_kind: &InkArgKind| {
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

                            params.push(SignatureParameter {
                                range: TextRange::new(
                                    TextSize::from(start_offset),
                                    TextSize::from(start_offset + param.len() as u32),
                                ),
                                detail: arg_value_kind.detail(),
                            });

                            if active_parameter.is_none()
                                && focused_arg.map_or(false, |arg| arg.kind() == arg_kind)
                            {
                                active_parameter = Some(params.len() - 1);
                            }
                        };

                        // Adds primary argument.
                        if let Some(primary_arg) = primary_arg_option {
                            add_param_to_signature(primary_arg);
                        }

                        // Adds valid optional argument.
                        let valid_siblings_options = primary_arg_option.map(|arg_kind| {
                            utils::valid_sibling_ink_args(InkAttributeKind::Arg(*arg_kind))
                        });
                        for optional_arg in optional_args.iter().filter(|arg_kind| {
                            // Filters out invalid optional arguments for the given primary argument (if any).
                            match valid_siblings_options.as_ref() {
                                Some(valid_args) => valid_args.contains(arg_kind),
                                None => true,
                            }
                        }) {
                            add_param_to_signature(optional_arg);
                        }

                        // Adds signature to results (if non-empty).
                        if !signature.is_empty() {
                            active_parameter =
                                active_parameter.or((!params.is_empty()).then_some(0));
                            results.push(SignatureHelp {
                                label: signature,
                                range,
                                parameters: (!params.is_empty()).then_some(params),
                                active_parameter,
                                detail: None,
                            });
                        }
                    };
                if !required_args.is_empty() {
                    for required_arg in required_args {
                        impl_signature_help(Some(&required_arg), &optional_args);
                    }
                } else if !optional_args.is_empty() {
                    impl_signature_help(None, &optional_args);
                }
            }
        }
    }

    results
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
                vec![(
                    "impl, namespace: &str",
                    (Some("("), Some("<-)")),
                    vec![
                        (Some("<-impl"), Some("impl")),
                        (Some("<-namespace"), Some("&str")),
                    ],
                    1,
                )],
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
            let results = signature_help(&InkFile::parse(code), offset);

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
                assert_eq!(
                    result.parameters.as_ref().map_or(0, |params| params.len()),
                    expected_params.len()
                );
                if let Some(params) = result.parameters.as_ref() {
                    for (idx, param) in params.iter().enumerate() {
                        let (start_pat, end_pat) = expected_params[idx];
                        // Verifies parameter range.
                        assert_eq!(
                            param.range,
                            TextRange::new(
                                TextSize::from(
                                    parse_offset_at(expected_label, start_pat).unwrap() as u32
                                ),
                                TextSize::from(
                                    parse_offset_at(expected_label, end_pat).unwrap() as u32
                                )
                            ),
                            "code: {code}"
                        );
                    }
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
