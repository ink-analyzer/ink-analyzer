//! ink! attribute completions.

use super::utils;
use ink_analyzer_ir::syntax::{AstNode, SyntaxKind, TextRange, TextSize};
use ink_analyzer_ir::{
    ast, FromSyntax, InkArgKind, InkAttributeKind, InkEntity, InkFile, InkMacroKind,
};

/// An ink! attribute completion item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
    /// Label which identifies the completion.
    pub label: String,
    /// Range of identifier that is being completed.
    pub range: TextRange,
    /// Replacement text for the completion.
    pub edit: String,
}

/// Computes ink! attribute completions at the given offset.
pub fn completions(file: &InkFile, offset: TextSize) -> Vec<Completion> {
    let mut results = Vec::new();

    // Compute ink! attribute macro completions.
    macro_completions(&mut results, file, offset);

    // Compute ink! attribute argument completions.
    argument_completions(&mut results, file, offset);

    results
}

/// Computes ink! attribute macro completions at the given offset.
pub fn macro_completions(results: &mut Vec<Completion>, file: &InkFile, offset: TextSize) {
    let item_at_offset = file.item_at_offset(offset);

    // Only computes completions if a focused token can be determined.
    if let Some(focused_token) = item_at_offset.focused_token() {
        // Only computes completions for ink! attributes.
        if let Some((attr, _, _)) = item_at_offset.normalized_parent_attr() {
            let default_label = "ink! attribute macro";

            let focused_token_is_left_bracket = focused_token.kind() == SyntaxKind::L_BRACK;
            let prev_token_is_left_bracket = matches!(
                item_at_offset
                    .prev_non_trivia_token()
                    .map(|prev_token| prev_token.kind()),
                Some(SyntaxKind::L_BRACK)
            );
            let focused_token_is_ink_or_colon_prefix =
                matches!(focused_token.text(), "ink" | "::" | ":");
            let focused_token_is_ink_path_segment = (focused_token.text() == "ink"
                && matches!(
                    item_at_offset
                        .prev_non_trivia_token()
                        .map(|prev_token| prev_token.kind()),
                    Some(SyntaxKind::L_BRACK)
                ))
                || (matches!(focused_token.text(), "::" | ":")
                    && matches!(
                        item_at_offset
                            .prev_non_trivia_token()
                            .as_ref()
                            .map(|prev_token| prev_token.text()),
                        Some("ink")
                    ))
                || (matches!(
                    item_at_offset
                        .prev_non_trivia_token()
                        .as_ref()
                        .map(|prev_token| prev_token.text()),
                    Some("::")
                ) && matches!(
                    item_at_offset
                        .prev_non_trivia_token()
                        .as_ref()
                        .and_then(|prev_token| ink_analyzer_ir::closest_non_trivia_token(
                            prev_token,
                            |token| token.prev_token()
                        ))
                        .as_ref()
                        .map(|prev_prev_token| prev_prev_token.text()),
                    Some("ink")
                ));

            // Only computes completions if the focused token is in an attribute macro path context.
            if focused_token_is_left_bracket
                || prev_token_is_left_bracket
                || focused_token_is_ink_path_segment
            {
                // Removes the delimiter (i.e `[`) from text range if it's the focused token.
                let edit_range = if focused_token_is_left_bracket {
                    let focused_token_end = focused_token.text_range().end();
                    TextRange::new(focused_token_end, focused_token_end)
                } else {
                    focused_token.text_range()
                };

                // Suggests ink! attribute macros based on the context (if any).
                let mut context_specific_ink_attr_macro_suggestions =
                    match item_at_offset.normalized_parent_ast_item_keyword() {
                        // Returns suggestions based on the AST item type keyword.
                        Some((ast_item_keyword, _, _)) => {
                            utils::valid_ink_macros_by_syntax_kind(ast_item_keyword.kind())
                        }
                        // Handles the case where the AST item type is unknown.
                        None => {
                            // Returns all attribute argument suggestions if focused token is part of an ink! path segment.
                            if focused_token_is_ink_path_segment {
                                vec![
                                    InkMacroKind::ChainExtension,
                                    InkMacroKind::Contract,
                                    InkMacroKind::StorageItem,
                                    InkMacroKind::Test,
                                    InkMacroKind::TraitDefinition,
                                ]
                            } else {
                                // Returns nothing if the ink! context can't be determined.
                                Vec::new()
                            }
                        }
                    };

                // Filters out invalid ink! attribute macro suggestions based on parent ink! scope (if any).
                let parent_ink_scope_valid_ink_macros: Vec<InkMacroKind> =
                    ink_analyzer_ir::ink_attrs_closest_ancestors(attr.syntax())
                        .flat_map(|attr| {
                            utils::valid_quasi_direct_descendant_ink_macros(attr.kind())
                        })
                        .collect();
                if !parent_ink_scope_valid_ink_macros.is_empty() {
                    context_specific_ink_attr_macro_suggestions.retain(|macro_kind| {
                        parent_ink_scope_valid_ink_macros.contains(macro_kind)
                    });
                }

                // Filters suggestions by the focused prefix if the focused token is
                // not a delimiter nor in the `ink::` path segment position.
                if !focused_token_is_left_bracket
                    && !prev_token_is_left_bracket
                    && !focused_token_is_ink_or_colon_prefix
                {
                    if let Some(prefix) = item_at_offset.focused_token_prefix() {
                        context_specific_ink_attr_macro_suggestions
                            .retain(|macro_kind| format!("{}", macro_kind).starts_with(prefix));
                    }
                }

                // Add context-specific completions to accumulator (if any).
                if !context_specific_ink_attr_macro_suggestions.is_empty() {
                    context_specific_ink_attr_macro_suggestions
                        .iter()
                        .for_each(|macro_kind| {
                            results.push(Completion {
                                label: format!("ink! {macro_kind} attribute macro."),
                                range: edit_range,
                                edit: format!(
                                    "{}{}{macro_kind}",
                                    // Only includes `ink` if the focused token is either the `[` delimiter,
                                    // the next token right after the `[` delimiter, the `ink` path segment.
                                    if focused_token_is_left_bracket
                                        || prev_token_is_left_bracket
                                        || focused_token.text() == "ink"
                                    {
                                        "ink"
                                    } else {
                                        ""
                                    },
                                    // Only includes `ink` if the focused token is either the `[` delimiter,
                                    // the next token right after the `[` delimiter or
                                    // anything in the `ink::` path segment position
                                    if focused_token_is_left_bracket
                                        || prev_token_is_left_bracket
                                        || focused_token_is_ink_or_colon_prefix
                                    {
                                        "::"
                                    } else {
                                        ""
                                    }
                                ),
                            });
                        });
                } else if prev_token_is_left_bracket {
                    // Otherwise, suggest the `ink` path segment itself if
                    // the focused token is an `ink` prefix and is also
                    // the next token right after the `[` delimiter.
                    if let Some(true) = item_at_offset
                        .focused_token_prefix()
                        .map(|prefix| "ink".starts_with(prefix))
                    {
                        results.push(Completion {
                            label: default_label.to_string(),
                            range: edit_range,
                            edit: "ink".to_string(),
                        });
                    }
                }
            }
        }
    }
}

/// Computes ink! attribute argument completions at the given offset.
pub fn argument_completions(results: &mut Vec<Completion>, file: &InkFile, offset: TextSize) {
    let item_at_offset = file.item_at_offset(offset);

    // Only computes completions if a focused token can be determined.
    if let Some(focused_token) = item_at_offset.focused_token() {
        // Only computes completions for ink! attributes.
        if let Some((ink_attr, _, _)) = item_at_offset.normalized_parent_ink_attr() {
            let focused_token_is_left_parenthesis = focused_token.kind() == SyntaxKind::L_PAREN;
            let prev_token_is_left_parenthesis = matches!(
                item_at_offset
                    .prev_non_trivia_token()
                    .map(|prev_token| prev_token.kind()),
                Some(SyntaxKind::L_PAREN)
            );
            let focused_token_is_comma = focused_token.kind() == SyntaxKind::COMMA;
            let prev_token_is_comma = matches!(
                item_at_offset
                    .prev_non_trivia_token()
                    .map(|prev_token| prev_token.kind()),
                Some(SyntaxKind::COMMA)
            );

            // Only computes completions if the focused token is in an argument context.
            if focused_token_is_left_parenthesis
                || prev_token_is_left_parenthesis
                || focused_token_is_comma
                || prev_token_is_comma
            {
                // Removes the delimiter (i.e `(` and `,`) from text range if it's the focused token.
                let edit_range = if focused_token_is_left_parenthesis || focused_token_is_comma {
                    let focused_token_end = focused_token.text_range().end();
                    TextRange::new(focused_token_end, focused_token_end)
                } else {
                    focused_token.text_range()
                };

                // Suggests ink! attribute arguments based on the context (if any).
                let mut context_specific_ink_arg_suggestions = match ink_attr.kind() {
                    // For unknown ink! attributes, suggestions are based on the AST item (if any).
                    InkAttributeKind::Macro(InkMacroKind::Unknown)
                    | InkAttributeKind::Arg(InkArgKind::Unknown) => {
                        match item_at_offset.normalized_parent_ast_item_keyword() {
                            // Returns suggestions based on the AST item type keyword.
                            Some((ast_item_keyword, _, _)) => {
                                utils::valid_ink_ink_args_by_syntax_kind(ast_item_keyword.kind())
                            }
                            // Handles cases where either the AST item type is unknown or
                            // the ink! attribute is not applied to an AST item (e.g. ink! topic).
                            None => {
                                // Checks whether for the parent is a struct `RecordField`.
                                // `RecordFieldList` is also matched for cases where the ink! attribute is
                                // unclosed and so the field is parsed as if it's part of the attribute.
                                match ink_attr.syntax().parent().and_then(|attr_parent| {
                                    (matches!(
                                        attr_parent.kind(),
                                        SyntaxKind::RECORD_FIELD | SyntaxKind::RECORD_FIELD_LIST
                                    ) && matches!(
                                        ink_analyzer_ir::parent_ast_item(&attr_parent),
                                        Some(ast::Item::Struct(_))
                                    ))
                                    .then_some(attr_parent)
                                }) {
                                    // Returns ink! topic suggest for struct fields.
                                    Some(_) => vec![InkArgKind::Topic],
                                    // Returns all attribute arguments that are capable of being standalone
                                    // if the AST item type is unknown.
                                    None => vec![
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
                                        // See `utils::valid_ink_ink_args_by_syntax_kind` docs for
                                        // rationale for omitting `derive`, `env`, `keep_attr` from this list.
                                    ],
                                }
                            }
                        }
                    }
                    // For known/valid primary ink! attribute kinds, only suggest valid ink! attribute siblings.
                    kind => utils::valid_sibling_ink_args(kind),
                };

                // Filters out duplicate and invalid (based on parent ink! scope) ink! attribute argument suggestions.
                let already_annotated_ink_args: Vec<&InkArgKind> =
                    ink_attr.args().iter().map(|arg| arg.kind()).collect();
                let parent_ink_scope_valid_ink_args: Vec<InkArgKind> = ink_attr
                    .tree()
                    .ink_attrs_closest_ancestors()
                    .flat_map(|attr| utils::valid_quasi_direct_descendant_ink_args(attr.kind()))
                    .collect();
                context_specific_ink_arg_suggestions.retain(|arg_kind| {
                    // Filters out duplicates.
                    !already_annotated_ink_args.contains(&arg_kind)
                        // Filters out invalid arguments for the parent ink! scope (if any).
                        && (parent_ink_scope_valid_ink_args.is_empty()
                        || parent_ink_scope_valid_ink_args.contains(arg_kind))
                });

                // Filters suggestions by the focused prefix if the focused token is not a delimiter.
                if !focused_token_is_left_parenthesis && !focused_token_is_comma {
                    if let Some(prefix) = item_at_offset.focused_token_prefix() {
                        context_specific_ink_arg_suggestions
                            .retain(|arg_kind| format!("{}", arg_kind).starts_with(prefix));
                    }
                }

                // Add completions to accumulator.
                context_specific_ink_arg_suggestions
                    .iter()
                    .for_each(|arg_kind: &InkArgKind| {
                        results.push(Completion {
                            label: format!("ink! {arg_kind} attribute argument."),
                            range: edit_range,
                            edit: arg_kind.to_string(),
                        });
                    });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::parse_offset_at;

    #[test]
    fn macro_completions_works() {
        for (code, scenarios) in [
            // (code, [(pat, [(edit, pat_start, pat_end)])]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the substring that will be replaced,
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // No AST item context.
            ("#[", vec![(None, vec![])]),
            ("#[i", vec![(None, vec![("ink", Some("<-i"), Some("i"))])]),
            (
                "#[ink:",
                vec![(
                    Some(":"),
                    vec![
                        ("::chain_extension", Some("<-:"), Some(":")),
                        ("::contract", Some("<-:"), Some(":")),
                        ("::storage_item", Some("<-:"), Some(":")),
                        ("::test", Some("<-:"), Some(":")),
                        ("::trait_definition", Some("<-:"), Some(":")),
                    ],
                )],
            ),
            (
                "#[ink::",
                vec![(
                    Some("::"),
                    vec![
                        ("::chain_extension", Some("<-::"), Some("::")),
                        ("::contract", Some("<-::"), Some("::")),
                        ("::storage_item", Some("<-::"), Some("::")),
                        ("::test", Some("<-::"), Some("::")),
                        ("::trait_definition", Some("<-::"), Some("::")),
                    ],
                )],
            ),
            // Module context.
            (
                r#"
                    #[]
                    mod my_contract {}
                "#,
                vec![(Some("["), vec![("ink::contract", Some("["), Some("<-]"))])],
            ),
            (
                r#"
                    #[i]
                    mod my_contract {}
                "#,
                vec![(Some("i"), vec![("ink::contract", Some("<-i"), Some("i"))])],
            ),
            (
                r#"
                    #[ink]
                    mod my_contract {}
                "#,
                vec![(
                    Some("i"),
                    vec![("ink::contract", Some("<-ink"), Some("ink"))],
                )],
            ),
            (
                r#"
                    #[ink::]
                    mod my_contract {}
                "#,
                vec![(Some("::"), vec![("::contract", Some("<-:"), Some("<-]"))])],
            ),
            (
                r#"
                    #[ink::co]
                    mod my_contract {}
                "#,
                vec![(Some(":c"), vec![("contract", Some("::"), Some("<-]"))])],
            ),
            // Trait context.
            (
                r#"
                    #[]
                    trait MyTrait {}
                "#,
                vec![(
                    Some("["),
                    vec![
                        ("ink::chain_extension", Some("["), Some("<-]")),
                        ("ink::trait_definition", Some("["), Some("<-]")),
                    ],
                )],
            ),
            (
                r#"
                    #[i]
                    trait MyTrait {}
                "#,
                vec![(
                    Some("i"),
                    vec![
                        ("ink::chain_extension", Some("<-i"), Some("i")),
                        ("ink::trait_definition", Some("<-i"), Some("i")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink]
                    trait MyTrait {}
                "#,
                vec![(
                    Some("i"),
                    vec![
                        ("ink::chain_extension", Some("<-ink"), Some("ink")),
                        ("ink::trait_definition", Some("<-ink"), Some("ink")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink::]
                    trait MyTrait {}
                "#,
                vec![(
                    Some("::"),
                    vec![
                        ("::chain_extension", Some("<-:"), Some("<-]")),
                        ("::trait_definition", Some("<-:"), Some("<-]")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink::ch]
                    trait MyTrait {}
                "#,
                vec![(
                    Some(":c"),
                    vec![("chain_extension", Some("::"), Some("<-]"))],
                )],
            ),
            (
                r#"
                    #[ink::tr]
                    trait MyTrait {}
                "#,
                vec![(
                    Some(":t"),
                    vec![("trait_definition", Some("::"), Some("<-]"))],
                )],
            ),
            // ADT context.
            (
                r#"
                    #[]
                    enum MyEnum {}
                "#,
                vec![(
                    Some("["),
                    vec![("ink::storage_item", Some("["), Some("<-]"))],
                )],
            ),
            (
                r#"
                    #[i]
                    struct MyStruct {}
                "#,
                vec![(
                    Some("i"),
                    vec![("ink::storage_item", Some("<-i"), Some("i"))],
                )],
            ),
            (
                r#"
                    #[ink]
                    union MyUnion {}
                "#,
                vec![(
                    Some("i"),
                    vec![("ink::storage_item", Some("<-ink"), Some("ink"))],
                )],
            ),
            (
                r#"
                    #[ink::]
                    enum MyEnum {}
                "#,
                vec![(
                    Some("::"),
                    vec![("::storage_item", Some("<-:"), Some("<-]"))],
                )],
            ),
            (
                r#"
                    #[ink::st]
                    struct MyStruct {}
                "#,
                vec![(Some(":s"), vec![("storage_item", Some("::"), Some("<-]"))])],
            ),
            // Function context.
            (
                r#"
                    #[]
                    fn my_fn {}
                "#,
                vec![(Some("["), vec![("ink::test", Some("["), Some("<-]"))])],
            ),
            (
                r#"
                    #[i]
                    fn my_fn {}
                "#,
                vec![(Some("i"), vec![("ink::test", Some("<-i"), Some("i"))])],
            ),
            (
                r#"
                    #[ink]
                    fn my_fn {}
                "#,
                vec![(Some("i"), vec![("ink::test", Some("<-ink"), Some("ink"))])],
            ),
            (
                r#"
                    #[ink::]
                    fn my_fn {}
                "#,
                vec![(Some("::"), vec![("::test", Some("<-:"), Some("<-]"))])],
            ),
            (
                r#"
                    #[ink::te]
                    fn my_fn {}
                "#,
                vec![(Some(":t"), vec![("test", Some("::"), Some("<-]"))])],
            ),
            // Contract scope.
            (
                r#"#
                    [ink::contract]
                    mod my_contract {
                        #[ink::
                    }
                "#,
                vec![(
                    Some("::->"),
                    vec![
                        ("::chain_extension", Some("<-::->"), Some("::->")),
                        ("::storage_item", Some("<-::->"), Some("::->")),
                        ("::test", Some("<-::->"), Some("::->")),
                        ("::trait_definition", Some("<-::->"), Some("::->")),
                    ],
                )],
            ),
        ] {
            for (pat, expected_results) in scenarios {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

                let mut results = Vec::new();
                macro_completions(&mut results, &InkFile::parse(code), offset);

                assert_eq!(
                    results
                        .iter()
                        .map(|completion| (completion.edit.as_str(), completion.range))
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
                    "code: {}",
                    code
                );
            }
        }
    }

    #[test]
    fn argument_completions_works() {
        for (code, scenarios) in [
            // (code, [(pat, [(edit, pat_start, pat_end)])]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the substring that will be replaced,
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // Non ink! attribute.
            ("#[cfg(", vec![(None, vec![])]),
            ("#[unknown(", vec![(None, vec![])]),
            // No AST item context.
            (
                "#[ink(",
                vec![(
                    None,
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                        ("impl", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                        ("topic", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                "#[ink(e",
                vec![(
                    None,
                    vec![
                        ("event", Some("<-e"), Some("e")),
                        ("extension", Some("<-e"), Some("e")),
                    ],
                )],
            ),
            (
                "#[ink(con",
                vec![(None, vec![("constructor", Some("<-con"), Some("con"))])],
            ),
            (
                "#[ink(message, pa",
                vec![(None, vec![("payable", Some("<-pa"), Some("pa"))])],
            ),
            (
                r#"
                    mod my_module {
                        #[ink(
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                        ("impl", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                        ("topic", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    mod my_module {
                        #[ink()
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                        ("impl", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                        ("topic", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    mod my_module {
                        #[ink()]
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                        ("impl", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                        ("topic", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    mod my_module {
                        #[ink(]
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                        ("impl", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                        ("topic", Some("("), Some("(")),
                    ],
                )],
            ),
            // ink! attribute argument context with no AST item.
            (
                "#[ink(event,",
                vec![(None, vec![("anonymous", Some(","), Some(","))])],
            ),
            (
                "#[ink(constructor,",
                vec![(
                    None,
                    vec![
                        ("default", Some(","), Some(",")),
                        ("payable", Some(","), Some(",")),
                        ("selector", Some(","), Some(",")),
                    ],
                )],
            ),
            (
                "#[ink(message,",
                vec![(
                    None,
                    vec![
                        ("default", Some(","), Some(",")),
                        ("payable", Some(","), Some(",")),
                        ("selector", Some(","), Some(",")),
                    ],
                )],
            ),
            (
                "#[ink(extension = 1,",
                vec![(None, vec![("handle_status", Some(","), Some(","))])],
            ),
            (
                "#[ink(impl,",
                vec![(None, vec![("namespace", Some(","), Some(","))])],
            ),
            // ink! attribute macro context with no AST item.
            (
                "#[ink::contract(",
                vec![(
                    None,
                    vec![
                        ("env", Some("("), Some("(")),
                        ("keep_attr", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                "#[ink::contract(env=my::env::Types,",
                vec![(None, vec![("keep_attr", Some(","), Some(","))])],
            ),
            (
                r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar","#,
                vec![(None, vec![])],
            ),
            (
                "#[ink::storage_item(",
                vec![(None, vec![("derive", Some("("), Some("("))])],
            ),
            (
                "#[ink::trait_definition(",
                vec![(
                    None,
                    vec![
                        ("keep_attr", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"#[ink::trait_definition(namespace="my_namespace","#,
                vec![(None, vec![("keep_attr", Some(","), Some(","))])],
            ),
            // Struct context.
            (
                r#"
                    #[ink(
                    struct MyStruct {}
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink()]
                    struct MyStruct {}
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink(]
                    struct MyStruct {}
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                    ],
                )],
            ),
            // Struct field context.
            (
                r#"
                    struct MyStruct {
                        #[ink(
                        value: bool,
                    }
                "#,
                vec![(Some("("), vec![("topic", Some("("), Some("("))])],
            ),
            (
                r#"
                    struct MyStruct {
                        #[ink()]
                        value: bool,
                    }
                "#,
                vec![(Some("("), vec![("topic", Some("("), Some("("))])],
            ),
            (
                r#"
                    struct MyStruct {
                        #[ink(]
                        value: bool,
                    }
                "#,
                vec![(Some("("), vec![("topic", Some("("), Some("("))])],
            ),
            // Fn context.
            (
                r#"
                    #[ink(
                    pub fn my_fn {}
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                    ],
                )],
            ),
            // Impl context.
            (
                r#"
                    #[ink(
                    impl MyImpl {}
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("impl", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                    ],
                )],
            ),
            // Contract scope.
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                        #[ink(
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("impl", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                        #[ink(
                        pub struct MyContract {}
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("anonymous", Some("("), Some("(")),
                        ("event", Some("("), Some("(")),
                        ("storage", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                        #[ink(event,
                        pub struct MyContract {}
                    }
                "#,
                vec![(Some("("), vec![("anonymous", Some("("), Some("("))])],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                        #[ink(
                        impl MyContract {}
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("impl", Some("("), Some("(")),
                        ("namespace", Some("("), Some("(")),
                    ],
                )],
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
                vec![(
                    Some("("),
                    vec![
                        ("constructor", Some("("), Some("(")),
                        ("default", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                    ],
                )],
            ),
            // Chain extension scope.
            (
                r#"
                    #[ink::chain_extension]
                    pub trait MyChainExtension {
                        #[ink(
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink::chain_extension]
                    pub trait MyChainExtension {
                        #[ink(
                        fn my_extension();
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("extension", Some("("), Some("(")),
                        ("handle_status", Some("("), Some("(")),
                    ],
                )],
            ),
            // Trait definition scope.
            (
                r#"
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("default", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                    ],
                )],
            ),
            (
                r#"
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(
                        fn my_message(&self);
                    }
                "#,
                vec![(
                    Some("("),
                    vec![
                        ("default", Some("("), Some("(")),
                        ("message", Some("("), Some("(")),
                        ("payable", Some("("), Some("(")),
                        ("selector", Some("("), Some("(")),
                    ],
                )],
            ),
        ] {
            for (pat, expected_results) in scenarios {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

                let mut results = Vec::new();
                argument_completions(&mut results, &InkFile::parse(code), offset);

                assert_eq!(
                    results
                        .iter()
                        .map(|completion| (completion.edit.as_str(), completion.range))
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
                    "code: {}",
                    code
                );
            }
        }
    }
}
