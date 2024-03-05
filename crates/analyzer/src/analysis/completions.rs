//! ink! attribute completions.

use ink_analyzer_ir::syntax::{AstNode, SyntaxKind, SyntaxToken, TextRange, TextSize};
use ink_analyzer_ir::{InkArgKind, InkAttributeKind, InkEntity, InkFile, InkMacroKind};

use super::{text_edit::TextEdit, utils};
use crate::Version;

/// An ink! attribute completion item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
    /// Label which identifies the completion.
    pub label: String,
    /// Range of identifier that is being completed.
    pub range: TextRange,
    /// Replacement text for the completion.
    pub edit: TextEdit,
    /// Descriptive information about the completion.
    pub detail: Option<String>,
}

/// Computes ink! attribute completions at the given offset.
pub fn completions(file: &InkFile, offset: TextSize, version: Version) -> Vec<Completion> {
    let mut results = Vec::new();

    // Compute ink! attribute macro completions.
    macro_completions(&mut results, file, offset, version);

    // Compute ink! attribute argument completions.
    argument_completions(&mut results, file, offset, version);

    results
}

/// Computes ink! attribute macro completions at the given offset.
pub fn macro_completions(
    results: &mut Vec<Completion>,
    file: &InkFile,
    offset: TextSize,
    version: Version,
) {
    let item_at_offset = file.item_at_offset(offset);

    // Only computes completions if a focused token can be determined.
    if let Some(focused_token) = item_at_offset.focused_token() {
        // Only computes completions for attributes.
        if let Some((attr, ..)) = item_at_offset.normalized_parent_attr() {
            let focused_token_is_left_bracket = focused_token.kind() == SyntaxKind::L_BRACK;
            let prev_token_is_left_bracket = item_at_offset
                .prev_non_trivia_token()
                .map_or(false, |prev_token| prev_token.kind() == SyntaxKind::L_BRACK);
            let focused_token_is_ink_crate_name = matches!(focused_token.text(), "ink" | "ink_e2e");
            let focused_token_is_ink_crate_name_or_colon_prefix =
                focused_token_is_ink_crate_name || matches!(focused_token.text(), "::" | ":");
            let focused_token_is_in_ink_crate_path_segment =
                (matches!(focused_token.text(), "ink" | "ink_e2e")
                    && item_at_offset
                        .prev_non_trivia_token()
                        .map_or(false, |prev_token| prev_token.kind() == SyntaxKind::L_BRACK))
                    || (matches!(focused_token.text(), "::" | ":")
                        && item_at_offset
                            .prev_non_trivia_token()
                            .as_ref()
                            .map_or(false, |token| matches!(token.text(), "ink" | "ink_e2e")))
                    || (item_at_offset
                        .prev_non_trivia_token()
                        .as_ref()
                        .map_or(false, |token| token.text() == "::")
                        && item_at_offset
                            .prev_non_trivia_token()
                            .as_ref()
                            .and_then(|prev_token| {
                                ink_analyzer_ir::closest_non_trivia_token(
                                    prev_token,
                                    SyntaxToken::prev_token,
                                )
                            })
                            .as_ref()
                            .map_or(false, |token| matches!(token.text(), "ink" | "ink_e2e")));

            // Only computes completions if the focused token is in an attribute macro path context.
            if focused_token_is_left_bracket
                || prev_token_is_left_bracket
                || focused_token_is_in_ink_crate_path_segment
            {
                // Removes the delimiter (i.e `[`) from text range if it's the focused token.
                let edit_range = if focused_token_is_left_bracket {
                    let focused_token_end = focused_token.text_range().end();
                    TextRange::new(focused_token_end, focused_token_end)
                } else {
                    focused_token.text_range()
                };

                // Only suggest ink! attribute macros if the AST item has no other ink! attributes.
                let mut ink_macro_suggestions = Vec::new();
                let ast_item_option = ink_analyzer_ir::parent_ast_item(attr.syntax());
                let has_other_ink_siblings = ast_item_option.as_ref().is_some_and(|item| {
                    ink_analyzer_ir::ink_attrs(item.syntax()).any(|it| it.syntax() != attr.syntax())
                });
                let has_other_ink_macro_siblings = ast_item_option.as_ref().is_some_and(|item| {
                    ink_analyzer_ir::ink_attrs(item.syntax()).any(|it| {
                        it.syntax() != attr.syntax()
                            && matches!(it.kind(), InkAttributeKind::Macro(_))
                    })
                });
                if !has_other_ink_siblings {
                    // Suggests ink! attribute macros based on the context (if any).
                    ink_macro_suggestions = match item_at_offset
                        .normalized_parent_ast_item_keyword()
                    {
                        // Returns suggestions based on the AST item type keyword.
                        Some((ast_item_keyword, ..)) => {
                            utils::valid_ink_macros_by_syntax_kind(ast_item_keyword.kind(), version)
                        }
                        // Handles the case where the AST item type is unknown.
                        None => {
                            // Returns all valid ink! attribute macro suggestions if focused token is part of an ink! path segment.
                            if focused_token_is_in_ink_crate_path_segment {
                                if version == Version::V5 {
                                    vec![
                                        InkMacroKind::ChainExtension,
                                        InkMacroKind::Contract,
                                        InkMacroKind::Event,
                                        InkMacroKind::ScaleDerive,
                                        InkMacroKind::StorageItem,
                                        InkMacroKind::Test,
                                        InkMacroKind::TraitDefinition,
                                        InkMacroKind::E2ETest,
                                    ]
                                } else {
                                    vec![
                                        InkMacroKind::ChainExtension,
                                        InkMacroKind::Contract,
                                        InkMacroKind::StorageItem,
                                        InkMacroKind::Test,
                                        InkMacroKind::TraitDefinition,
                                        InkMacroKind::E2ETest,
                                    ]
                                }
                            } else {
                                // Returns nothing if the ink! context can't be determined.
                                Vec::new()
                            }
                        }
                    };

                    // Filters suggestions by the matching ink! macro crate
                    // if a complete `ink` or `ink_e2e` path segment is already present before the focused token.
                    if focused_token_is_in_ink_crate_path_segment
                        && !focused_token_is_ink_crate_name
                    {
                        if let Some(ink_crate_name) = attr
                            .path()
                            .and_then(|it| it.first_segment())
                            .map(|it| it.to_string())
                        {
                            ink_macro_suggestions
                                .retain(|macro_kind| macro_kind.crate_name() == ink_crate_name);
                        }
                    }

                    // Filters suggestions by the focused prefix if the focused token is
                    // not a delimiter nor in the `ink::` or `ink_e2e::` path segment position.
                    if !focused_token_is_left_bracket
                        && !prev_token_is_left_bracket
                        && !focused_token_is_ink_crate_name_or_colon_prefix
                    {
                        if let Some(prefix) = item_at_offset.focused_token_prefix() {
                            ink_macro_suggestions
                                .retain(|macro_kind| macro_kind.macro_name().starts_with(prefix));
                        }
                    }

                    // Filters out invalid ink! attribute macro suggestions based on parent ink! scope (if any).
                    if let Some(attr_parent) = attr.syntax().parent() {
                        utils::remove_invalid_ink_macro_suggestions_for_parent_ink_scope(
                            &mut ink_macro_suggestions,
                            &attr_parent,
                            version,
                        );
                    }
                }

                // Add context-specific completions to accumulator (if any).
                if !ink_macro_suggestions.is_empty() {
                    for macro_kind in ink_macro_suggestions {
                        let edit = format!(
                            "{}{}{}",
                            // Only includes `ink` if the focused token is either the `[` delimiter,
                            // the next token right after the `[` delimiter, the `ink` path segment.
                            if focused_token_is_left_bracket
                                || prev_token_is_left_bracket
                                || matches!(focused_token.text(), "ink" | "ink_e2e")
                            {
                                macro_kind.crate_name()
                            } else {
                                ""
                            },
                            // Only includes `ink` if the focused token is either the `[` delimiter,
                            // the next token right after the `[` delimiter or
                            // anything in the `ink::` path segment position
                            if focused_token_is_left_bracket
                                || prev_token_is_left_bracket
                                || focused_token_is_ink_crate_name_or_colon_prefix
                            {
                                "::"
                            } else {
                                ""
                            },
                            macro_kind.macro_name()
                        );
                        results.push(Completion {
                            label: edit.clone(),
                            range: edit_range,
                            edit: TextEdit::replace(edit, edit_range),
                            detail: Some(format!("ink! {macro_kind} attribute macro.")),
                        });
                    }
                } else if prev_token_is_left_bracket && !has_other_ink_macro_siblings {
                    // Suggests the `ink` and `ink_e2e` path segments if
                    // the focused token is an `ink` or `ink_e2e` prefix and is also
                    // the next token right after the `[` delimiter.
                    let focused_token_prefix = item_at_offset.focused_token_prefix();
                    let ink_path_suggestions = if has_other_ink_siblings {
                        vec![("ink()", Some("ink($1)"), "ink! attribute")]
                    } else {
                        vec![
                            ("ink", None, "ink! attribute macro"),
                            ("ink_e2e", None, "ink! e2e attribute macro"),
                        ]
                    };
                    for (ink_macro_crate_name, ink_macro_crate_name_snippet, detail) in
                        ink_path_suggestions
                    {
                        if focused_token_prefix
                            .is_some_and(|prefix| ink_macro_crate_name.starts_with(prefix))
                        {
                            results.push(Completion {
                                label: ink_macro_crate_name.to_owned(),
                                range: edit_range,
                                edit: TextEdit::replace_with_snippet(
                                    ink_macro_crate_name.to_owned(),
                                    edit_range,
                                    ink_macro_crate_name_snippet.map(ToString::to_string),
                                ),
                                detail: Some(detail.to_owned()),
                            });
                        }
                    }
                }
            }
        }
    }
}

/// Computes ink! attribute argument completions at the given offset.
pub fn argument_completions(
    results: &mut Vec<Completion>,
    file: &InkFile,
    offset: TextSize,
    version: Version,
) {
    let item_at_offset = file.item_at_offset(offset);

    // Only computes completions if a focused token can be determined.
    if let Some(focused_token) = item_at_offset.focused_token() {
        // Only computes completions for ink! attributes.
        if let Some((ink_attr, ..)) = item_at_offset.normalized_parent_ink_attr() {
            let focused_token_is_left_parenthesis = focused_token.kind() == SyntaxKind::L_PAREN;
            let prev_non_trivia_token_is_left_parenthesis = item_at_offset
                .prev_non_trivia_token()
                .map_or(false, |prev_token| prev_token.kind() == SyntaxKind::L_PAREN);
            let focused_token_is_comma = focused_token.kind() == SyntaxKind::COMMA;
            let prev_non_trivia_token_is_comma = item_at_offset
                .prev_non_trivia_token()
                .map_or(false, |prev_token| prev_token.kind() == SyntaxKind::COMMA);
            let prev_token_is_whitespace = focused_token.prev_token().map_or(true, |prev_token| {
                prev_token.kind() == SyntaxKind::WHITESPACE
            });

            // Only computes completions if the focused token is in an argument context.
            if focused_token_is_left_parenthesis
                || prev_non_trivia_token_is_left_parenthesis
                || focused_token_is_comma
                || prev_non_trivia_token_is_comma
            {
                // Removes the delimiter (i.e `(` and `,`) from text range if it's the focused token.
                let edit_range = if focused_token_is_left_parenthesis || focused_token_is_comma {
                    let focused_token_end = focused_token.text_range().end();
                    TextRange::new(focused_token_end, focused_token_end)
                } else {
                    focused_token.text_range()
                };

                // Suggests ink! attribute arguments based on the context (if any).
                let mut ink_arg_suggestions = match ink_attr.kind() {
                    // For unknown ink! attributes, suggestions are based on the parent item (if any).
                    InkAttributeKind::Macro(InkMacroKind::Unknown)
                    | InkAttributeKind::Arg(InkArgKind::Unknown) => {
                        match item_at_offset.normalized_parent_item_syntax_kind() {
                            // Returns suggestions based on the parent item kind.
                            Some(parent_item_kind) => {
                                utils::valid_ink_args_by_syntax_kind(parent_item_kind, version)
                            }
                            // Handles cases where either the parent item kind is unknown.
                            // Returns all attribute arguments that don't require a macro
                            // if the AST item type is unknown.
                            None => {
                                if version == Version::V5 {
                                    vec![
                                        InkArgKind::Anonymous,
                                        InkArgKind::Constructor,
                                        InkArgKind::Default,
                                        InkArgKind::Event,
                                        InkArgKind::Function,
                                        InkArgKind::HandleStatus,
                                        InkArgKind::Impl,
                                        InkArgKind::Message,
                                        InkArgKind::Namespace,
                                        InkArgKind::Payable,
                                        InkArgKind::Selector,
                                        InkArgKind::SignatureTopic,
                                        InkArgKind::Storage,
                                        InkArgKind::Topic,
                                    ]
                                } else {
                                    vec![
                                        InkArgKind::Anonymous,
                                        InkArgKind::Constructor,
                                        InkArgKind::Default,
                                        InkArgKind::Event,
                                        InkArgKind::Extension,
                                        InkArgKind::HandleStatus,
                                        InkArgKind::Impl,
                                        InkArgKind::Message,
                                        InkArgKind::Namespace,
                                        InkArgKind::Payable,
                                        InkArgKind::Selector,
                                        InkArgKind::Storage,
                                        InkArgKind::Topic,
                                    ]
                                }
                            }
                        }
                    }
                    // For known/valid primary ink! attribute kinds, only suggest valid ink! attribute siblings.
                    kind => utils::valid_sibling_ink_args(*kind, version),
                };

                // Filters out duplicates, conflicting and invalidly scoped ink! arguments.
                utils::remove_duplicate_conflicting_and_invalid_scope_ink_arg_suggestions(
                    &mut ink_arg_suggestions,
                    &ink_attr,
                    version,
                );

                // Filters suggestions by the focused prefix if the focused token is not a delimiter.
                if !focused_token_is_left_parenthesis && !focused_token_is_comma {
                    if let Some(prefix) = item_at_offset.focused_token_prefix() {
                        ink_arg_suggestions
                            .retain(|arg_kind| format!("{arg_kind}").starts_with(prefix));
                    }
                }

                // Add completions to accumulator.
                for arg_kind in ink_arg_suggestions {
                    let prefix = if focused_token_is_comma
                        || (prev_non_trivia_token_is_comma && !prev_token_is_whitespace)
                    {
                        // Inserts some space between the comma and the argument.
                        " "
                    } else {
                        ""
                    };
                    let (edit, snippet) = utils::ink_arg_insert_text(
                        arg_kind,
                        Some(edit_range.end()),
                        Some(&ink_attr),
                    );
                    results.push(Completion {
                        label: edit.clone(),
                        range: edit_range,
                        edit: TextEdit::replace_with_snippet(
                            format!("{prefix}{edit}"),
                            edit_range,
                            snippet.map(|snippet| format!("{prefix}{snippet}")),
                        ),
                        detail: Some(format!("ink! {arg_kind} attribute argument.")),
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::{parse_offset_at, remove_whitespace};

    macro_rules! list_results {
        ($list: expr, $start: literal, $end: literal) => {
            $list
                .iter()
                .map(|name| (*name, Some($start), Some($end)))
                .collect()
        };
        ($list: expr, -$exclude: literal, $start: literal, $end: literal) => {
            $list
                .iter()
                .filter(|name| **name != $exclude)
                .map(|name| (*name, Some($start), Some($end)))
                .collect()
        };
    }

    #[test]
    fn macro_completions_works() {
        for (version, all_macros, adt_macros, adt_macros_sub_paths) in [
            (
                Version::V4,
                vec![
                    "::chain_extension",
                    "::contract",
                    "::storage_item",
                    "::test",
                    "::trait_definition",
                ],
                vec!["ink::storage_item"],
                vec!["::storage_item"],
            ),
            (
                Version::V5,
                vec![
                    "::chain_extension",
                    "::contract",
                    "::event",
                    "::scale_derive",
                    "::storage_item",
                    "::test",
                    "::trait_definition",
                ],
                vec!["ink::event", "ink::storage_item", "ink::scale_derive"],
                vec!["::event", "::storage_item", "::scale_derive"],
            ),
        ] {
            for (code, pat, expected_results) in [
                // (code, [(pat, [(edit, pat_start, pat_end)])]) where:
                // code = source code,
                // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
                // edit = the text that will be inserted (represented without whitespace for simplicity),
                // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
                // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

                // No AST item context.
                ("#[", None, vec![]),
                (
                    "#[i",
                    None,
                    vec![
                        ("ink", Some("<-i"), Some("i")),
                        ("ink_e2e", Some("<-i"), Some("i")),
                    ],
                ),
                ("#[ink_", None, vec![("ink_e2e", Some("<-i"), Some("ink_"))]),
                ("#[ink:", Some(":"), list_results!(all_macros, "<-:", ":")),
                (
                    "#[ink::",
                    Some("::"),
                    list_results!(all_macros, "<-::", "::"),
                ),
                (
                    "#[ink_e2e:",
                    Some(":"),
                    vec![("::test", Some("<-:"), Some(":"))],
                ),
                (
                    "#[ink_e2e::",
                    Some("::"),
                    vec![("::test", Some("<-::"), Some("::"))],
                ),
                // Module context.
                (
                    r#"
                    #[]
                    mod my_contract {}
                "#,
                    Some("["),
                    vec![("ink::contract", Some("["), Some("<-]"))],
                ),
                (
                    r#"
                    #[i]
                    mod my_contract {}
                "#,
                    Some("i"),
                    vec![("ink::contract", Some("<-i"), Some("i"))],
                ),
                (
                    r#"
                    #[ink]
                    mod my_contract {}
                "#,
                    Some("i"),
                    vec![("ink::contract", Some("<-ink"), Some("ink"))],
                ),
                (
                    r#"
                    #[ink::]
                    mod my_contract {}
                "#,
                    Some("::"),
                    vec![("::contract", Some("<-:"), Some("<-]"))],
                ),
                (
                    r#"
                    #[ink::co]
                    mod my_contract {}
                "#,
                    Some(":c"),
                    vec![("contract", Some("::"), Some("<-]"))],
                ),
                // Trait context.
                (
                    r#"
                    #[]
                    trait MyTrait {}
                "#,
                    Some("["),
                    vec![
                        ("ink::chain_extension", Some("["), Some("<-]")),
                        ("ink::trait_definition", Some("["), Some("<-]")),
                    ],
                ),
                (
                    r#"
                    #[i]
                    trait MyTrait {}
                "#,
                    Some("i"),
                    vec![
                        ("ink::chain_extension", Some("<-i"), Some("i")),
                        ("ink::trait_definition", Some("<-i"), Some("i")),
                    ],
                ),
                (
                    r#"
                    #[ink]
                    trait MyTrait {}
                "#,
                    Some("i"),
                    vec![
                        ("ink::chain_extension", Some("<-ink"), Some("ink")),
                        ("ink::trait_definition", Some("<-ink"), Some("ink")),
                    ],
                ),
                (
                    r#"
                    #[ink::]
                    trait MyTrait {}
                "#,
                    Some("::"),
                    vec![
                        ("::chain_extension", Some("<-:"), Some("<-]")),
                        ("::trait_definition", Some("<-:"), Some("<-]")),
                    ],
                ),
                (
                    r#"
                    #[ink::ch]
                    trait MyTrait {}
                "#,
                    Some(":c"),
                    vec![("chain_extension", Some("::"), Some("<-]"))],
                ),
                (
                    r#"
                    #[ink::tr]
                    trait MyTrait {}
                "#,
                    Some(":t"),
                    vec![("trait_definition", Some("::"), Some("<-]"))],
                ),
                // ADT context.
                (
                    r#"
                    #[]
                    enum MyEnum {}
                "#,
                    Some("["),
                    list_results!(adt_macros, -"ink::event", "[", "<-]"),
                ),
                (
                    r#"
                    #[i]
                    struct MyStruct {}
                "#,
                    Some("i"),
                    list_results!(adt_macros, "<-i", "i"),
                ),
                (
                    r#"
                    #[ink]
                    union MyUnion {}
                "#,
                    Some("i"),
                    list_results!(adt_macros, -"ink::event", "<-ink", "ink"),
                ),
                (
                    r#"
                    #[ink::]
                    enum MyEnum {}
                "#,
                    Some("::"),
                    list_results!(adt_macros_sub_paths, -"::event", "<-:", "<-]"),
                ),
                (
                    r#"
                    #[ink::st]
                    struct MyStruct {}
                "#,
                    Some(":st"),
                    vec![("storage_item", Some("::"), Some("<-]"))],
                ),
                // Function context.
                (
                    r#"
                    #[]
                    fn my_fn() {}
                "#,
                    Some("["),
                    vec![
                        ("ink::test", Some("["), Some("<-]")),
                        ("ink_e2e::test", Some("["), Some("<-]")),
                    ],
                ),
                (
                    r#"
                    #[i]
                    fn my_fn() {}
                "#,
                    Some("i"),
                    vec![
                        ("ink::test", Some("<-i"), Some("i")),
                        ("ink_e2e::test", Some("<-i"), Some("i")),
                    ],
                ),
                (
                    r#"
                    #[ink]
                    fn my_fn() {}
                "#,
                    Some("i"),
                    vec![
                        ("ink::test", Some("<-ink"), Some("ink")),
                        ("ink_e2e::test", Some("<-ink"), Some("ink")),
                    ],
                ),
                (
                    r#"
                    #[ink::]
                    fn my_fn() {}
                "#,
                    Some("::"),
                    vec![("::test", Some("<-:"), Some("<-]"))],
                ),
                (
                    r#"
                    #[ink::te]
                    fn my_fn() {}
                "#,
                    Some(":t"),
                    vec![("test", Some("::"), Some("<-]"))],
                ),
                // Contract scope.
                (
                    r#"#
                    [ink::contract]
                    mod my_contract {
                        #[ink::
                    }
                "#,
                    Some("::->"),
                    list_results!(all_macros, -"::contract", "<-::->", "::->"),
                ),
            ] {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

                let mut results = Vec::new();
                macro_completions(&mut results, &InkFile::parse(code), offset, version);

                assert_eq!(
                    results
                        .iter()
                        .map(|completion| (completion.edit.text.trim(), completion.range))
                        .collect::<Vec<(&str, TextRange)>>(),
                    expected_results
                        .into_iter()
                        .map(|(edit, pat_start, pat_end)| (
                            edit,
                            TextRange::new(
                                TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                                TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                            )
                        ))
                        .collect::<Vec<(&str, TextRange)>>(),
                    "code: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn argument_completions_works() {
        for (
            version,
            standalone_args,
            contract_child_args,
            adt_args,
            fn_args,
            event_args,
            extension_args,
            e2e_args,
        ) in [
            (
                Version::V4,
                vec![
                    "anonymous",
                    "constructor",
                    "default",
                    "event",
                    "extension=1",
                    "handle_status=true",
                    "impl",
                    "message",
                    r#"namespace="my_namespace""#,
                    "payable",
                    "selector=1",
                    "storage",
                    "topic",
                ],
                vec![
                    "anonymous",
                    "constructor",
                    "default",
                    "event",
                    "impl",
                    "message",
                    r#"namespace="my_namespace""#,
                    "payable",
                    "selector=1",
                    "storage",
                ],
                vec!["anonymous", "event", "storage"],
                vec![
                    "constructor",
                    "default",
                    "extension=1",
                    "handle_status=true",
                    "message",
                    "payable",
                    "selector=1",
                ],
                vec!["anonymous"],
                vec!["extension=1", "handle_status=true"],
                vec![
                    r#"additional_contracts="""#,
                    "environment=ink::env::DefaultEnvironment",
                    r#"keep_attr="""#,
                ],
            ),
            (
                Version::V5,
                vec![
                    "anonymous",
                    "constructor",
                    "default",
                    "event",
                    "function=1",
                    "handle_status=true",
                    "impl",
                    "message",
                    r#"namespace="my_namespace""#,
                    "payable",
                    "selector=1",
                    r#"signature_topic="""#,
                    "storage",
                    "topic",
                ],
                vec![
                    "anonymous",
                    "constructor",
                    "default",
                    "event",
                    "impl",
                    "message",
                    r#"namespace="my_namespace""#,
                    "payable",
                    "selector=1",
                    r#"signature_topic="""#,
                    "storage",
                ],
                vec!["anonymous", "event", r#"signature_topic="""#, "storage"],
                vec![
                    "constructor",
                    "default",
                    "function=1",
                    "handle_status=true",
                    "message",
                    "payable",
                    "selector=1",
                ],
                vec!["anonymous", r#"signature_topic="""#],
                vec!["function=1", "handle_status=true"],
                vec!["backend(node)", "environment=ink::env::DefaultEnvironment"],
            ),
        ] {
            for (code, pat, expected_results) in [
                // (code, pat, [(edit, pat_start, pat_end)]) where:
                // code = source code,
                // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
                // edit = the text that will be inserted (represented without whitespace for simplicity),
                // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
                // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

                // Non ink! attribute.
                ("#[cfg(", None, vec![]),
                ("#[unknown(", None, vec![]),
                // No AST item context.
                ("#[ink(", None, list_results!(standalone_args, "(", "(")),
                (
                    "#[ink(e",
                    None,
                    if version == Version::V5 {
                        vec![("event", Some("<-e"), Some("e"))]
                    } else {
                        vec![
                            ("event", Some("<-e"), Some("e")),
                            ("extension=1", Some("<-e"), Some("e")),
                        ]
                    },
                ),
                (
                    "#[ink(con",
                    None,
                    vec![("constructor", Some("<-con"), Some("con"))],
                ),
                (
                    "#[ink(message, pa",
                    None,
                    vec![("payable", Some("<-pa"), Some("pa"))],
                ),
                (
                    r#"
                        mod my_module {
                            #[ink(
                        }
                    "#,
                    Some("("),
                    list_results!(standalone_args, "(", "("),
                ),
                (
                    r#"
                        mod my_module {
                            #[ink()
                        }
                    "#,
                    Some("("),
                    list_results!(standalone_args, "(", "("),
                ),
                (
                    r#"
                        mod my_module {
                            #[ink()]
                        }
                    "#,
                    Some("("),
                    list_results!(standalone_args, "(", "("),
                ),
                (
                    r#"
                        mod my_module {
                            #[ink(]
                        }
                    "#,
                    Some("("),
                    list_results!(standalone_args, "(", "("),
                ),
                // ink! attribute argument context with no AST item.
                ("#[ink(event,", None, list_results!(event_args, ",", ",")),
                (
                    "#[ink(constructor,",
                    None,
                    vec![
                        ("default", Some(","), Some(",")),
                        ("payable", Some(","), Some(",")),
                        ("selector=1", Some(","), Some(",")),
                    ],
                ),
                (
                    "#[ink(message,",
                    None,
                    vec![
                        ("default", Some(","), Some(",")),
                        ("payable", Some(","), Some(",")),
                        ("selector=1", Some(","), Some(",")),
                    ],
                ),
                (
                    if version == Version::V5 {
                        "#[ink(function = 1,"
                    } else {
                        "#[ink(extension = 1,"
                    },
                    None,
                    vec![("handle_status=true", Some(","), Some(","))],
                ),
                (
                    "#[ink(impl,",
                    None,
                    vec![(r#"namespace="my_namespace""#, Some(","), Some(","))],
                ),
                (
                    "#[ink(impl,=",
                    Some(","),
                    vec![("namespace", Some(","), Some(","))],
                ),
                (
                    "#[ink(impl, =",
                    Some(","),
                    vec![("namespace", Some(","), Some(","))],
                ),
                // ink! attribute macro context with no AST item.
                (
                    "#[ink::contract(",
                    None,
                    vec![
                        ("env=ink::env::DefaultEnvironment", Some("("), Some("(")),
                        (r#"keep_attr="""#, Some("("), Some("(")),
                    ],
                ),
                (
                    "#[ink::contract(env=my::env::Types,",
                    None,
                    vec![(r#"keep_attr="""#, Some(","), Some(","))],
                ),
                (
                    r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar","#,
                    None,
                    vec![],
                ),
                (
                    "#[ink::storage_item(",
                    None,
                    vec![("derive=true", Some("("), Some("("))],
                ),
                (
                    "#[ink::trait_definition(",
                    None,
                    vec![
                        (r#"keep_attr="""#, Some("("), Some("(")),
                        (r#"namespace="my_namespace""#, Some("("), Some("(")),
                    ],
                ),
                (
                    r#"#[ink::trait_definition(namespace="my_namespace","#,
                    None,
                    vec![(r#"keep_attr="""#, Some(","), Some(","))],
                ),
                ("#[ink_e2e::test(", None, list_results!(e2e_args, "(", "(")),
                // Struct context.
                (
                    r#"
                    #[ink(
                        struct MyStruct {}
                    "#,
                    Some("("),
                    list_results!(adt_args, "(", "("),
                ),
                (
                    r#"
                        #[ink()]
                        struct MyStruct {}
                    "#,
                    Some("("),
                    list_results!(adt_args, "(", "("),
                ),
                (
                    r#"
                        #[ink(]
                        struct MyStruct {}
                    "#,
                    Some("("),
                    list_results!(adt_args, "(", "("),
                ),
                // Struct field context.
                (
                    r#"
                    struct MyStruct {
                            #[ink(
                            value: bool,
                        }
                    "#,
                    Some("("),
                    vec![("topic", Some("("), Some("("))],
                ),
                (
                    r#"
                        struct MyStruct {
                            #[ink()]
                            value: bool,
                        }
                    "#,
                    Some("("),
                    vec![("topic", Some("("), Some("("))],
                ),
                (
                    r#"
                        struct MyStruct {
                            #[ink(]
                            value: bool,
                        }
                    "#,
                    Some("("),
                    vec![("topic", Some("("), Some("("))],
                ),
                // Fn context.
                (
                    r#"
                        #[ink(
                        pub fn my_fn() {}
                    "#,
                    Some("("),
                    list_results!(fn_args, "(", "("),
                ),
                (
                    r#"
                        #[ink(constructor)]
                        #[ink(
                        pub fn my_fn() {}
                    "#,
                    Some("ink(->"),
                    vec![
                        ("default", Some("ink(->"), Some("ink(->")),
                        ("payable", Some("ink(->"), Some("ink(->")),
                        ("selector=1", Some("ink(->"), Some("ink(->")),
                    ],
                ),
                // Impl context.
                (
                    r#"
                        #[ink(
                        impl MyImpl {}
                    "#,
                    Some("("),
                    vec![
                        ("impl", Some("("), Some("(")),
                        (r#"namespace="my_namespace""#, Some("("), Some("(")),
                    ],
                ),
                // Contract scope.
                (
                    r#"
                        #[ink::contract]
                        mod my_contract {
                            #[ink(
                        }
                    "#,
                    Some("("),
                    list_results!(contract_child_args, "(", "("),
                ),
                (
                    r#"
                        #[ink::contract]
                        mod my_contract {
                            #[ink(
                            pub struct MyContract {}
                        }
                    "#,
                    Some("("),
                    list_results!(adt_args, "(", "("),
                ),
                (
                    r#"
                        #[ink::contract]
                        mod my_contract {
                            #[ink(event,
                            pub struct MyContract {}
                        }
                    "#,
                    Some("("),
                    list_results!(event_args, "(", "("),
                ),
                (
                    r#"
                        #[ink::contract]
                        mod my_contract {
                            #[ink(
                            impl MyContract {}
                        }
                    "#,
                    Some("("),
                    vec![
                        ("impl", Some("("), Some("(")),
                        (r#"namespace="my_namespace""#, Some("("), Some("(")),
                    ],
                ),
                (
                    r#"
                        #[ink::contract]
                        mod my_contract {
                            impl MyContract {
                                #[ink(
                                pub fn my_fn() {}
                            }
                        }
                    "#,
                    Some("("),
                    vec![
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector=1", Some("("), Some("(")),
                    ],
                ),
                // Chain extension scope.
                (
                    r#"
                        #[ink::chain_extension]
                        pub trait MyChainExtension {
                            #[ink(
                        }
                    "#,
                    Some("("),
                    list_results!(extension_args, "(", "("),
                ),
                (
                    r#"
                        #[ink::chain_extension]
                        pub trait MyChainExtension {
                            #[ink(
                            fn my_extension();
                        }
                    "#,
                    Some("("),
                    list_results!(extension_args, "(", "("),
                ),
                // Trait definition scope.
                (
                    r#"
                        #[ink::trait_definition]
                        pub trait MyTrait {
                            #[ink(
                        }
                    "#,
                    Some("("),
                    vec![
                        ("default", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector=1", Some("("), Some("(")),
                    ],
                ),
                (
                    r#"
                        #[ink::trait_definition]
                        pub trait MyTrait {
                            #[ink(
                            fn my_message(&self);
                        }
                    "#,
                    Some("("),
                    vec![
                        ("default", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector=1", Some("("), Some("(")),
                    ],
                ),
                // Unique ids.
                (
                    r#"
                        #[ink::contract]
                        mod my_contract {
                            impl MyContract {
                                #[ink(constructor, selector=1)]
                                pub fn constructor_1(&self) {}

                                #[ink(constructor, sel)]
                                pub fn constructor_2(&self) {}
                            }
                        }
                    "#,
                    Some("#[ink(constructor, sel->"),
                    vec![(
                        "selector=2",
                        Some("#[ink(constructor, ->"),
                        Some("#[ink(constructor, sel->"),
                    )],
                ),
                (
                    r#"
                        #[ink::contract]
                        mod my_contract {
                            impl MyContract {
                                #[ink(message, selector=1)]
                                pub fn message_1(&self) {}

                                #[ink(message, sel)]
                                pub fn message_2(&self) {}
                            }
                        }
                    "#,
                    Some("#[ink(message, sel->"),
                    vec![(
                        "selector=2",
                        Some("#[ink(message, ->"),
                        Some("#[ink(message, sel->"),
                    )],
                ),
                (
                    r#"
                        #[ink::trait_definition]
                        pub trait MyTrait {
                            #[ink(message, selector=1)]
                            fn message_1(&self);

                            #[ink(message, sel)]
                            fn message_2(&self);
                        }
                    "#,
                    Some("#[ink(message, sel->"),
                    vec![(
                        "selector=2",
                        Some("#[ink(message, ->"),
                        Some("#[ink(message, sel->"),
                    )],
                ),
                if version == Version::V5 {
                    (
                        r#"
                            #[ink::chain_extension]
                            pub trait MyChainExtension {
                                #[ink(function=1)]
                                fn function_1(&self);

                                #[ink(fun)]
                                fn function_2(&self);
                            }
                        "#,
                        Some("#[ink(fun->"),
                        vec![("function=2", Some("#[ink(->"), Some("#[ink(fun->"))],
                    )
                } else {
                    (
                        r#"
                            #[ink::chain_extension]
                            pub trait MyChainExtension {
                                #[ink(extension=1)]
                                fn extension_1(&self);

                                #[ink(ext)]
                                fn extension_2(&self);
                            }
                        "#,
                        Some("#[ink(ext->"),
                        vec![("extension=2", Some("#[ink(->"), Some("#[ink(ext->"))],
                    )
                },
            ] {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

                let mut results = Vec::new();
                argument_completions(&mut results, &InkFile::parse(code), offset, version);

                assert_eq!(
                    results
                        .into_iter()
                        .map(|completion| (
                            remove_whitespace(completion.edit.text),
                            completion.range
                        ))
                        .collect::<Vec<(String, TextRange)>>(),
                    expected_results
                        .into_iter()
                        .map(|(edit, pat_start, pat_end)| (
                            remove_whitespace(edit.to_owned()),
                            TextRange::new(
                                TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                                TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                            )
                        ))
                        .collect::<Vec<(String, TextRange)>>(),
                    "code: {code}, version: {:?}",
                    version
                );
            }
        }
    }
}
