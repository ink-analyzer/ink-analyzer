//! ink! attribute argument inlay hints.

use ink_analyzer_ir::syntax::{AstToken, TextRange, TextSize};
use ink_analyzer_ir::{
    InkArgValueKind, InkArgValuePathKind, InkArgValueStringKind, InkFile, IsInkEntity,
};

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
pub fn inlay_hints(file: &InkFile, range: Option<TextRange>) -> Vec<InlayHint> {
    // Iterates over all ink! attributes in the file.
    file.tree()
        .ink_attrs_in_scope()
        .flat_map(|attr| {
            // Returns inlay hints for all ink! attribute arguments with values in the visible range.
            attr.args()
                .iter()
                .filter_map(|arg| {
                    // Filters out ink! attribute arguments that aren't in the visible range.
                    (range.is_none()
                        || matches!(
                            range.as_ref().map(|it| it.contains_range(arg.text_range())),
                            Some(true)
                        ))
                    .then(|| {
                        // Creates inlay hint labels for ink! attribute arguments without values.
                        let arg_value_kind = InkArgValueKind::from(*arg.kind());
                        let label = match arg_value_kind {
                            InkArgValueKind::None => "",
                            InkArgValueKind::U32 => "u32",
                            InkArgValueKind::U32OrWildcard => "u32 | _",
                            InkArgValueKind::String(_) => "&str",
                            InkArgValueKind::Bool => "bool",
                            InkArgValueKind::Path(path_kind) => match path_kind {
                                InkArgValuePathKind::Environment => "impl Environment",
                                _ => "Path",
                            },
                        };
                        // Creates inlay hint if a label was defined for the ink! attribute argument.
                        (!label.is_empty()).then_some(InlayHint {
                            label: label.to_string(),
                            position: arg.name().map_or(arg.text_range().end(), |name| {
                                name.syntax().text_range().end()
                            }),
                            range: arg
                                .name()
                                .map_or(arg.text_range(), |name| name.syntax().text_range()),
                            detail: match arg_value_kind {
                                InkArgValueKind::String(InkArgValueStringKind::CommaList) => {
                                    Some("A comma separated/delimited list".to_string())
                                }
                                InkArgValueKind::String(InkArgValueStringKind::Identifier) => {
                                    Some("A valid Rust identifier".to_string())
                                }
                                InkArgValueKind::String(InkArgValueStringKind::SpaceList) => {
                                    Some("A space delimited list".to_string())
                                }
                                _ => None,
                            },
                        })
                    })?
                })
                .collect::<Vec<InlayHint>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::parse_offset_at;

    #[test]
    fn inlay_hints_works() {
        for (code, visible_range_pat, expected_results) in [
            // (code, (Option<(target_pat_start, target_pat_end)>, [(label, detail, pos_pat, (range_pat_start, range_pat_end))])) where:
            // code = source code,
            // range_pat_start = substring used to find the start of the visible range (see `test_utils::parse_offset_at` doc),
            // range_pat_end = substring used to find the end of the range the visible range (see `test_utils::parse_offset_at` doc).
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
            (
                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=my::env::Types, keep_attr="foo,bar")]"#,
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
                "#[ink(message, default, payable, selector=1)]",
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
        ] {
            let visible_range = visible_range_pat.map(|(pat_start, pat_end)| {
                TextRange::new(
                    TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                    TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32),
                )
            });

            let results = inlay_hints(&InkFile::parse(code), visible_range);

            assert_eq!(
                results
                    .into_iter()
                    .map(|item| (item.label, item.position, item.range))
                    .collect::<Vec<(String, TextSize, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(label, pos_pat_start, (range_pat_start, range_pat_end))| (
                        label.to_string(),
                        TextSize::from(parse_offset_at(code, pos_pat_start).unwrap() as u32),
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, range_pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, range_pat_end).unwrap() as u32)
                        )
                    ))
                    .collect::<Vec<(String, TextSize, TextRange)>>(),
                "code: {code}"
            );
        }
    }
}
