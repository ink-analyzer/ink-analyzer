//! ink! attribute argument inlay hints.

use ink_analyzer_ir::syntax::{AstToken, TextRange, TextSize};
use ink_analyzer_ir::{InkArg, InkArgKind, InkArgValueKind, InkAttributeKind, InkEntity, InkFile};

use crate::Version;

/// An ink! attribute argument inlay hint.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlayHint {
    /// Text of the inlay hint.
    pub label: String,
    /// Position of the inlay hint.
    pub position: TextSize,
    /// Range to which the inlay hint applies.
    pub range: TextRange,
    /// Extra details about the inlay hint.
    pub detail: Option<String>,
}

/// Computes ink! attribute argument inlay hints for the given text range (if any).
pub fn inlay_hints(file: &InkFile, range: Option<TextRange>, version: Version) -> Vec<InlayHint> {
    let mut results = Vec::new();

    let mut process_inlay_hint = |arg: &InkArg, is_constructor: bool| {
        // Filters out ink! attribute arguments that aren't in the selection range.
        // Note that range of `None` means entire file is in range.
        if range.map_or(true, |it| it.contains_range(arg.text_range())) {
            // Creates inlay hint if a non-empty label is defined for the ink! attribute argument.
            let arg_value_kind = if version == Version::V5 {
                InkArgValueKind::from_v5(*arg.kind(), Some(is_constructor))
            } else {
                InkArgValueKind::from(*arg.kind())
            };

            let label = arg_value_kind.to_string();
            if !label.is_empty() {
                let doc = arg_value_kind.detail();
                results.push(InlayHint {
                    label,
                    position: arg
                        .name()
                        .map(|name| name.syntax().text_range().end())
                        .unwrap_or_else(|| arg.text_range().end()),
                    range: arg
                        .name()
                        .map(|name| name.syntax().text_range())
                        .unwrap_or_else(|| arg.text_range()),
                    detail: (!doc.is_empty()).then(|| doc.to_owned()),
                })
            }
        }
    };

    // Iterates over all ink! attributes in the file.
    for attr in file.tree().ink_attrs_in_scope() {
        // Returns inlay hints for all ink! attribute arguments with values in the selection range.
        for arg in attr.args() {
            let is_constructor = *attr.kind() == InkAttributeKind::Arg(InkArgKind::Constructor);
            process_inlay_hint(arg, is_constructor);

            let mut nested_arg = None;
            while let Some(arg) = nested_arg.as_ref().unwrap_or(arg).nested() {
                process_inlay_hint(&arg, is_constructor);
                nested_arg = Some(arg);
            }
        }
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Version;
    use test_utils::parse_offset_at;

    #[test]
    fn inlay_hints_works() {
        for (version, fixtures) in [
            (
                Version::V4,
                vec![
                    (
                        "#[ink(message, default, payable, selector=1)]",
                        None,
                        vec![(
                            "u32 | _",
                            Some("selector"),
                            (Some("<-selector"), Some("selector")),
                        )],
                    ),
                    (
                        "#[ink(extension=1, handle_status=true)]",
                        None,
                        vec![
                            (
                                "u32",
                                Some("extension"),
                                (Some("<-extension"), Some("extension")),
                            ),
                            (
                                "bool",
                                Some("handle_status"),
                                (Some("<-handle_status"), Some("handle_status")),
                            ),
                        ],
                    ),
                    (
                        r#"#[ink_e2e::test(
                        additional_contracts="adder/Cargo.toml flipper/Cargo.toml",
                        environment=ink::env::DefaultEnvironment,
                        keep_attr="foo,bar"
                        )]"#,
                        None,
                        vec![
                            (
                                "&str",
                                Some("additional_contracts"),
                                (Some("<-additional_contracts"), Some("additional_contracts")),
                            ),
                            (
                                "impl Environment",
                                Some("environment"),
                                (Some("<-environment"), Some("environment")),
                            ),
                            (
                                "&str",
                                Some("keep_attr"),
                                (Some("<-keep_attr"), Some("keep_attr")),
                            ),
                        ],
                    ),
                ],
            ),
            (
                Version::V5,
                vec![
                    (
                        "#[ink(message, default, payable, selector=1)]",
                        None,
                        vec![(
                            "u32 | _ | @",
                            Some("selector"),
                            (Some("<-selector"), Some("selector")),
                        )],
                    ),
                    (
                        "#[ink(constructor, default, payable, selector=1)]",
                        None,
                        vec![(
                            "u32 | _",
                            Some("selector"),
                            (Some("<-selector"), Some("selector")),
                        )],
                    ),
                    (
                        r#"#[ink::event(signature_topic="1111111111111111111111111111111111111111111111111111111111111111")]"#,
                        None,
                        vec![(
                            "&str",
                            Some("signature_topic"),
                            (Some("<-signature_topic"), Some("signature_topic")),
                        )],
                    ),
                    (
                        r#"#[ink(event, signature_topic="1111111111111111111111111111111111111111111111111111111111111111")]"#,
                        None,
                        vec![(
                            "&str",
                            Some("signature_topic"),
                            (Some("<-signature_topic"), Some("signature_topic")),
                        )],
                    ),
                    (
                        "#[ink::chain_extension(extension=1)]",
                        None,
                        vec![(
                            "u16",
                            Some("extension->"),
                            (Some("<-extension->"), Some("extension->")),
                        )],
                    ),
                    (
                        "#[ink(function=1, handle_status=true)]",
                        None,
                        vec![
                            (
                                "u16",
                                Some("function"),
                                (Some("<-function"), Some("function")),
                            ),
                            (
                                "bool",
                                Some("handle_status"),
                                (Some("<-handle_status"), Some("handle_status")),
                            ),
                        ],
                    ),
                    (
                        r#"#[ink_e2e::test(
                        environment=ink::env::DefaultEnvironment,
                        backend(node(url="ws://127.0.0.1:9000"))
                        )]"#,
                        None,
                        vec![
                            (
                                "impl Environment",
                                Some("environment"),
                                (Some("<-environment"), Some("environment")),
                            ),
                            (
                                "node | runtime_only",
                                Some("backend"),
                                (Some("<-backend"), Some("backend")),
                            ),
                            ("&str", Some("url"), (Some("<-url"), Some("url"))),
                        ],
                    ),
                    (
                        r#"#[ink_e2e::test(
                        environment=ink::env::DefaultEnvironment,
                        backend(runtime_only(runtime=ink_e2e::MinimalRuntime))
                        )]"#,
                        None,
                        vec![
                            (
                                "impl Environment",
                                Some("environment"),
                                (Some("<-environment"), Some("environment")),
                            ),
                            (
                                "node | runtime_only",
                                Some("backend"),
                                (Some("<-backend"), Some("backend")),
                            ),
                            (
                                "impl drink::SandboxConfig",
                                Some("runtime->"),
                                (Some("<-runtime->"), Some("runtime->")),
                            ),
                        ],
                    ),
                ],
            ),
        ] {
            for (code, selection_range_pat, expected_results) in [
                // (code, Option<(selection_pat_start, selection_pat_end)>, [(label, detail, pos_pat, (range_pat_start, range_pat_end))]) where:
                // code = source code,
                // selection_pat_start = substring used to find the start of the selection range (see `test_utils::parse_offset_at` doc),
                // selection_pat_end = substring used to find the end of the range the selection range (see `test_utils::parse_offset_at` doc).
                // label = the label text for the inlay hint,
                // detail = the optional detail text for the inlay hint,
                // pos_pat = substring used to find the cursor offset for the inlay hint (see `test_utils::parse_offset_at` doc),
                // range_pat_start = substring used to find the start of the range the inlay hint applies to (see `test_utils::parse_offset_at` doc),
                // range_pat_end = substring used to find the end of the range the inlay hint applies to (see `test_utils::parse_offset_at` doc).

                // Control tests.
                ("// Nothing", None, vec![]),
                (
                    r#"
                    mod my_mod {
                        fn my_fn(a: bool, b: u8) {
                        }
                    }
                "#,
                    None,
                    vec![],
                ),
                // ink! attribute macros.
                ("#[ink::contract]", None, vec![]),
                ("#[ink::trait_definition]", None, vec![]),
                ("#[ink::chain_extension]", None, vec![]),
                ("#[ink::storage_item]", None, vec![]),
                ("#[ink::test]", None, vec![]),
                (
                    r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#,
                    None,
                    vec![
                        (
                            "impl Environment",
                            Some("env"),
                            (Some("<-env"), Some("env")),
                        ),
                        (
                            "&str",
                            Some("keep_attr"),
                            (Some("<-keep_attr"), Some("keep_attr")),
                        ),
                    ],
                ),
                (
                    r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#,
                    Some((Some("<-"), Some("->"))),
                    vec![
                        (
                            "impl Environment",
                            Some("env"),
                            (Some("<-env"), Some("env")),
                        ),
                        (
                            "&str",
                            Some("keep_attr"),
                            (Some("<-keep_attr"), Some("keep_attr")),
                        ),
                    ],
                ),
                (
                    r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#,
                    Some((Some("<-"), Some("my::env::Types"))),
                    vec![(
                        "impl Environment",
                        Some("env"),
                        (Some("<-env"), Some("env")),
                    )],
                ),
                (
                    r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#,
                    Some((Some("<-keep_attr"), Some("->"))),
                    vec![(
                        "&str",
                        Some("keep_attr"),
                        (Some("<-keep_attr"), Some("keep_attr")),
                    )],
                ),
                (
                    r#"#[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar")]"#,
                    None,
                    vec![
                        (
                            "&str",
                            Some("namespace"),
                            (Some("<-namespace"), Some("namespace")),
                        ),
                        (
                            "&str",
                            Some("keep_attr"),
                            (Some("<-keep_attr"), Some("keep_attr")),
                        ),
                    ],
                ),
                (
                    "#[ink::storage_item(derive=true)]",
                    None,
                    vec![("bool", Some("derive"), (Some("<-derive"), Some("derive")))],
                ),
                // ink! attribute arguments.
                ("#[ink(storage)]", None, vec![]),
                ("#[ink(event, anonymous)]", None, vec![]),
                (
                    "#[ink(constructor, default, selector=1)]",
                    None,
                    vec![(
                        "u32 | _",
                        Some("selector"),
                        (Some("<-selector"), Some("selector")),
                    )],
                ),
                (
                    r#"#[ink(impl, namespace="my_namespace")]"#,
                    None,
                    vec![(
                        "&str",
                        Some("namespace"),
                        (Some("<-namespace"), Some("namespace")),
                    )],
                ),
            ]
            .into_iter()
            .chain(fixtures)
            {
                let range = selection_range_pat.map(|(pat_start, pat_end)| {
                    TextRange::new(
                        TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                        TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32),
                    )
                });
                let results = inlay_hints(&InkFile::parse(code), range, version);

                assert_eq!(
                    results
                        .into_iter()
                        .map(|item| (item.label, item.position, item.range))
                        .collect::<Vec<(String, TextSize, TextRange)>>(),
                    expected_results
                        .into_iter()
                        .map(|(label, pos_pat_start, (range_pat_start, range_pat_end))| (
                            label.to_owned(),
                            TextSize::from(parse_offset_at(code, pos_pat_start).unwrap() as u32),
                            TextRange::new(
                                TextSize::from(
                                    parse_offset_at(code, range_pat_start).unwrap() as u32
                                ),
                                TextSize::from(parse_offset_at(code, range_pat_end).unwrap() as u32)
                            )
                        ))
                        .collect::<Vec<(String, TextSize, TextRange)>>(),
                    "code: {code}, version: {:?}",
                    version
                );
            }
        }
    }
}
