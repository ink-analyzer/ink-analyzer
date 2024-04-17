//! ink! attribute completions.

use ink_analyzer_ir::ast::HasAttrs;
use ink_analyzer_ir::syntax::{AstNode, AstToken, SyntaxKind, SyntaxToken, TextRange, TextSize};
use ink_analyzer_ir::{ast, ChainExtension, Contract, Event, EventV2, InkImpl, TraitDefinition};
use ink_analyzer_ir::{
    InkArg, InkArgKind, InkArgValueKind, InkAttributeKind, InkEntity, InkFile, InkMacroKind,
    Version,
};

use super::{
    text_edit::{self, TextEdit},
    utils,
};

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
    /// Type of item being completed.
    pub kind: CompletionKind,
}

/// An ink! attribute completion item kind.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompletionKind {
    Attr,
    Enum,
    Field,
    Fn,
    Mod,
    Struct,
    Trait,
}

/// Computes ink! attribute completions at the given offset.
pub fn completions(file: &InkFile, offset: TextSize, version: Version) -> Vec<Completion> {
    let mut results = Vec::new();

    // Compute ink! attribute macro completions.
    macro_completions(&mut results, file, offset, version);

    // Compute ink! attribute argument completions.
    argument_completions(&mut results, file, offset, version);

    // Compute ink! entity completions.
    entity_completions(&mut results, file, offset, version);

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
                            kind: CompletionKind::Attr,
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
                                kind: CompletionKind::Attr,
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
                // Suggestions ink! attribute arguments based on the context (if any).
                let mut ink_arg_suggestions = Vec::new();
                let mut edit_range = None;
                let mut is_nested = false;
                let mut parent_arg_name = None;

                // Suggests "nested" ink! attribute arguments (if appropriate).
                let is_valid_focused_arg = |arg: &InkArg| {
                    *arg.kind() != InkArgKind::Unknown
                        && arg.text_range().contains_inclusive(offset)
                        && !arg.meta().is_empty()
                };
                if let Some(top_arg) = ink_attr.args().iter().find(|arg| is_valid_focused_arg(arg))
                {
                    let mut nested_arg = None;
                    while let Some(arg) = nested_arg
                        .as_ref()
                        .unwrap_or(top_arg)
                        .nested()
                        .filter(is_valid_focused_arg)
                    {
                        nested_arg = Some(arg);
                    }

                    let focused_arg = nested_arg.as_ref().unwrap_or(top_arg);
                    let nested_arg_suggestions = match InkArgValueKind::from(*focused_arg.kind()) {
                        InkArgValueKind::Arg(kind, _) => vec![kind],
                        InkArgValueKind::Choice(kind_1, kind_2, _) => vec![kind_1, kind_2],
                        _ => Vec::new(),
                    };
                    if !nested_arg_suggestions.is_empty() {
                        ink_arg_suggestions = nested_arg_suggestions;
                        // Unknown args are filtered out by `is_valid_focused_arg`, so the arg must have a name at this point.
                        let meta_name = focused_arg
                            .name()
                            .expect("Valid ink! args must have a name");
                        let name_end = meta_name.syntax().text_range().end();
                        edit_range = Some(TextRange::new(name_end, focused_arg.text_range().end()));
                        is_nested = true;
                        parent_arg_name = Some(meta_name.to_string());
                    }
                }

                // Suggests "normal" ink! attribute arguments (if any).
                if ink_arg_suggestions.is_empty() {
                    // Suggests ink! attribute arguments based on the context (if any).
                    ink_arg_suggestions = match ink_attr.kind() {
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
                }

                // Filters suggestions by the focused prefix if the focused token is not a delimiter.
                if !focused_token_is_left_parenthesis && !focused_token_is_comma {
                    if let Some(prefix) = item_at_offset.focused_token_prefix() {
                        ink_arg_suggestions.retain(|arg_kind| {
                            format!("{arg_kind}").starts_with(prefix)
                                || parent_arg_name.as_ref().is_some_and(|name| name == prefix)
                        });
                    }
                }

                // Add completions to accumulator.
                for arg_kind in ink_arg_suggestions {
                    let (prefix, suffix) = if is_nested {
                        ("(", ")")
                    } else if focused_token_is_comma
                        || (prev_non_trivia_token_is_comma && !prev_token_is_whitespace)
                    {
                        // Inserts some space between the comma and the argument.
                        (" ", "")
                    } else {
                        ("", "")
                    };

                    let range = edit_range.unwrap_or(
                        if focused_token_is_left_parenthesis || focused_token_is_comma {
                            // Removes the delimiter (i.e `(` and `,`) from text range if it's the focused token.
                            let focused_token_end = focused_token.text_range().end();
                            TextRange::new(focused_token_end, focused_token_end)
                        } else {
                            focused_token.text_range()
                        },
                    );
                    let (edit, snippet) =
                        utils::ink_arg_insert_text(arg_kind, Some(range.end()), Some(&ink_attr));
                    results.push(Completion {
                        label: edit.clone(),
                        range,
                        edit: TextEdit::replace_with_snippet(
                            format!("{prefix}{edit}{suffix}"),
                            range,
                            snippet.map(|snippet| format!("{prefix}{snippet}{suffix}")),
                        ),
                        detail: Some(format!("ink! {arg_kind} attribute argument.")),
                        kind: CompletionKind::Attr,
                    });
                }
            }
        }
    }
}

/// Computes ink! entity completions at the given offset.
pub fn entity_completions(
    results: &mut Vec<Completion>,
    file: &InkFile,
    offset: TextSize,
    version: Version,
) {
    let item_at_offset = file.item_at_offset(offset);

    // Only computes completions if a focused token can be determined, and it's not a comment.
    let Some(focused_token) = item_at_offset
        .focused_token()
        .filter(|token| token.kind() != SyntaxKind::COMMENT)
    else {
        return;
    };

    // Bail if focused token is part of an attribute.
    if item_at_offset.parent_attr().is_some() {
        return;
    }

    let normalized_focused_text = if focused_token.kind().is_trivia() {
        ""
    } else {
        focused_token.text()
    };

    // Computes the edit range as the entire line for the offset,
    // except if the focused token is whitespace with a new line, in which case the passed offset is used.
    let mut prev_non_trivia_line_siblings = Vec::new();
    let mut next_non_trivia_line_siblings = Vec::new();
    let mut last_line_sibling = None;
    let is_inline_token =
        |token: &SyntaxToken| !token.kind().is_trivia() || !token.text().contains('\n');
    let mut line_start = || {
        std::iter::successors(
            focused_token.prev_token().filter(is_inline_token),
            |token| token.prev_token().filter(is_inline_token),
        )
        .inspect(|token| {
            if !token.kind().is_trivia() {
                prev_non_trivia_line_siblings.push(token.text().to_owned());
            }
        })
        .last()
        .as_ref()
        .map(SyntaxToken::text_range)
        .unwrap_or_else(|| focused_token.text_range())
        .start()
    };
    let mut line_end = || {
        std::iter::successors(
            focused_token.next_token().filter(is_inline_token),
            |token| token.next_token().filter(is_inline_token),
        )
        .inspect(|token| {
            if !token.kind().is_trivia() {
                next_non_trivia_line_siblings.push(token.text().to_owned());
            }
            last_line_sibling = Some(token.clone());
        })
        .last()
        .as_ref()
        .map(SyntaxToken::text_range)
        .unwrap_or_else(|| focused_token.text_range())
        .end()
    };
    let (mut start, end) =
        if focused_token.kind() == SyntaxKind::WHITESPACE && focused_token.text().contains('\n') {
            let ws_offset = offset - focused_token.text_range().start();
            let (ws_before, ws_after) = focused_token.text().split_at(ws_offset.into());
            let is_cursor_after_newline = ws_before.contains('\n');
            let is_cursor_before_newline = ws_after.contains('\n');
            match (is_cursor_after_newline, is_cursor_before_newline) {
                // Between newlines.
                (true, true) => (offset, offset),
                // After newline.
                (true, false) => (offset, line_end()),
                // Before newline.
                (false, true) => (line_start(), offset),
                // No newlines, should be unreachable.
                (false, false) => (line_start(), line_end()),
            }
        } else {
            // Not focused on/in whitespace.
            (line_start(), line_end())
        };
    // Normalize start offset to remove/ignore unnecessary indenting.
    if let Some(start_token) = file
        .item_at_offset(start)
        .token_at_offset()
        .clone()
        .left_biased()
    {
        let ws_token = if start_token.kind() == SyntaxKind::WHITESPACE {
            Some(start_token)
        } else {
            start_token.prev_token()
        };
        if let Some(ws_token) = ws_token.filter(|token| token.kind() == SyntaxKind::WHITESPACE) {
            if ws_token.text_range().end() <= start {
                let indent = utils::end_indenting(ws_token.text());
                if !indent.is_empty() {
                    start = ws_token.text_range().end() - TextSize::from(indent.len() as u32);
                }
            } else {
                let ws_offset = start - ws_token.text_range().start();
                let (ws_before, _) = ws_token.text().split_at(ws_offset.into());
                let indent = utils::end_indenting(ws_before);
                if !indent.is_empty() {
                    start -= TextSize::from(indent.len() as u32);
                }
            }
        }
    }
    let range = TextRange::new(start, end);

    // Checks if the focused text or another word on the line matches one of the options.
    let is_line_affix_of = |options: &[&str]| {
        // Empty line.
        (normalized_focused_text.is_empty()
            && prev_non_trivia_line_siblings.is_empty()
            && next_non_trivia_line_siblings.is_empty())
            // Focused text is a substring of one of the keywords.
            || (!normalized_focused_text.is_empty()
                && options
                    .iter()
                    .any(|option| option.contains(normalized_focused_text)))
            // At least one word on the line is a substring of one of the keywords.
            || prev_non_trivia_line_siblings
                .iter()
                .chain(next_non_trivia_line_siblings.iter())
                .any(|focused_sibling_text| {
                    !focused_sibling_text.is_empty()
                        && options
                            .iter()
                            .any(|option| option.contains(focused_sibling_text))
                })
    };

    // Checks if the next non-trivia token is wrapped in an error node.
    // Useful for determining whether to offer suggestions more conservatively
    // (e.g. when adding items that shouldn't be duplicated).
    let is_next_node_error = || {
        ink_analyzer_ir::closest_non_trivia_token(focused_token, SyntaxToken::next_token)
            .as_ref()
            .and_then(SyntaxToken::parent)
            .is_some_and(|token| token.kind() == SyntaxKind::ERROR)
    };

    macro_rules! add_event_v2 {
        ($indent: expr, $module: expr) => {
            if version == Version::V5 && is_line_affix_of(&["struct", "event"]) {
                results.push(Completion {
                    label: "#[ink::event]..pub struct Event {...}".to_owned(),
                    range,
                    edit: text_edit::add_event_v2(range, $indent, $module),
                    detail: Some("ink! event 2.0".to_owned()),
                    kind: CompletionKind::Struct,
                });
            }
        };
        () => {
            add_event_v2!(None, None)
        };
    }

    // Determines the "focused item" for the completion context (if any).
    let is_record_field =
        ink_analyzer_ir::closest_ancestor_ast_type::<_, ast::RecordField>(focused_token).is_some()
            || ink_analyzer_ir::closest_ancestor_ast_type::<_, ast::RecordFieldList>(focused_token)
                .is_some();
    let is_record_field_or_ws_only_line = is_record_field
        || (focused_token.kind() == SyntaxKind::WHITESPACE
            && prev_non_trivia_line_siblings.is_empty()
            && next_non_trivia_line_siblings.is_empty());
    let focused_item = if is_record_field_or_ws_only_line {
        // Record fields (e.g. `struct` fields) and whitespace-only lines have no wrapping "focused" item.
        None
    } else {
        item_at_offset.parent_ast_item()
    };
    // Bail if focused token is part of an item that spans multiple lines,
    // except if the next token is an error as that likely just indicates an intermittent join
    // of the current incomplete item to the next one by the parser.
    let is_multi_line_focused_item = focused_item.as_ref().is_some_and(|focused_item| {
        focused_item.syntax().text_range().end()
            > last_line_sibling
                .as_ref()
                .map(SyntaxToken::text_range)
                .unwrap_or_else(|| focused_token.text_range())
                .end()
    });
    if is_multi_line_focused_item && !is_next_node_error() {
        return;
    }

    // Determines the "parent item" for the completion context (if any).
    let parent_item = if is_record_field_or_ws_only_line {
        // Completion context "parent item" for record fields (e.g. `struct` fields) and
        // whitespace-only lines is the direct parent item.
        item_at_offset.parent_ast_item()
    } else {
        // Otherwise, the completion context "parent item" is the parent of the "focused" item.
        focused_item
            .and_then(|focused_item| ink_analyzer_ir::parent_ast_item(focused_item.syntax()))
    };
    if let Some(parent_item) = parent_item {
        // Computes completions based on parent item.
        match parent_item {
            ast::Item::Module(module_item) => {
                if let Some(contract) = Contract::cast(module_item.syntax().clone()) {
                    if contract.storage().is_none()
                        && is_line_affix_of(&["struct", "storage"])
                        && !is_next_node_error()
                    {
                        // Adds ink! storage.
                        results.push(Completion {
                            label: "#[ink(storage)]..pub struct Storage {...}".to_owned(),
                            range,
                            edit: text_edit::add_storage(&contract, range),
                            detail: Some("ink! storage".to_owned()),
                            kind: CompletionKind::Struct,
                        });
                    }

                    // Adds ink! event 2.0.
                    let indent = utils::item_children_indenting(module_item.syntax());
                    add_event_v2!((!indent.is_empty()).then_some(&indent), Some(&module_item));

                    // Adds ink! event.
                    if is_line_affix_of(&["struct", "event"]) {
                        results.push(Completion {
                            label: "#[ink(event)]..pub struct Event {...}".to_owned(),
                            range,
                            edit: text_edit::add_event_v1(&module_item, range),
                            detail: Some("ink! event".to_owned()),
                            kind: CompletionKind::Struct,
                        });
                    }
                } else {
                    let is_cfg_test = module_item
                        .attrs()
                        .any(|attr| utils::is_cfg_test_attr(&attr));
                    if is_cfg_test && is_line_affix_of(&["fn", "test"]) {
                        // Adds ink! test.
                        results.push(Completion {
                            label: "#[ink::test]..fn test(..) {...}".to_owned(),
                            range,
                            edit: text_edit::add_test(&module_item, range),
                            detail: Some("ink! test".to_owned()),
                            kind: CompletionKind::Fn,
                        });
                    }

                    let is_cfg_e2e_tests = module_item
                        .attrs()
                        .any(|attr| utils::is_cfg_e2e_tests_attr(&attr));
                    if is_cfg_e2e_tests && is_line_affix_of(&["fn", "test", "e2e"]) {
                        // Adds ink! e2e test.
                        results.push(Completion {
                            label: "#[ink_e2e::test]..fn test(..) {...}".to_owned(),
                            range,
                            edit: text_edit::add_e2e_test(&module_item, range, version),
                            detail: Some("ink! e2e test".to_owned()),
                            kind: CompletionKind::Fn,
                        });
                    }
                }
            }
            ast::Item::Impl(impl_item) => {
                // Ignore trait implementations.
                if impl_item.trait_().is_some() {
                    return;
                }
                // Bail if not an ink! impl and parent isn't an ink! contract.
                if !InkImpl::can_cast(impl_item.syntax())
                    && ink_analyzer_ir::ink_parent::<Contract>(impl_item.syntax()).is_none()
                {
                    return;
                };

                // Adds ink! constructor.
                if is_line_affix_of(&["fn", "constructor", "new"]) {
                    results.push(Completion {
                        label: "#[ink(constructor)]..pub fn new(..) {...}".to_owned(),
                        range,
                        edit: text_edit::add_constructor_to_impl(&impl_item, range),
                        detail: Some("ink! constructor".to_owned()),
                        kind: CompletionKind::Fn,
                    });
                }

                // Adds ink! message.
                if is_line_affix_of(&["fn", "message"]) {
                    results.push(Completion {
                        label: "#[ink(message)]..pub fn message(..) {...}".to_owned(),
                        range,
                        edit: text_edit::add_message_to_impl(&impl_item, range),
                        detail: Some("ink! message".to_owned()),
                        kind: CompletionKind::Fn,
                    });
                }
            }
            ast::Item::Trait(trait_item) => {
                if let Some(trait_def) = TraitDefinition::cast(trait_item.syntax().clone()) {
                    // Adds ink! message declaration.
                    if is_line_affix_of(&["fn", "message"]) {
                        results.push(Completion {
                            label: "#[ink(message)]..fn message(..);".to_owned(),
                            range,
                            edit: text_edit::add_message_to_trait_def(&trait_def, range),
                            detail: Some("ink! message".to_owned()),
                            kind: CompletionKind::Fn,
                        });
                    }
                } else if let Some(chain_extension) =
                    ChainExtension::cast(trait_item.syntax().clone())
                {
                    // Add `ErrorCode` type.
                    if chain_extension.error_code().is_none()
                        && is_line_affix_of(&["type", "error", "code", "ErrorCode"])
                    {
                        results.push(Completion {
                            label: "type ErrorCode = ..;".to_owned(),
                            range,
                            edit: text_edit::add_error_code_type(&chain_extension, range),
                            detail: Some("`ErrorCode` type for ink! chain extension".to_owned()),
                            kind: CompletionKind::Fn,
                        });
                    }

                    // Adds ink! extension.
                    if is_line_affix_of(&["fn", "extension", "function"]) {
                        results.push(Completion {
                            label: format!(
                                "#[ink({}=..)]..pub fn extension(..);",
                                if version == Version::V5 {
                                    "function"
                                } else {
                                    "extension"
                                }
                            ),
                            range,
                            edit: text_edit::add_extension(&chain_extension, range, version),
                            detail: Some("ink! extension `fn`".to_owned()),
                            kind: CompletionKind::Fn,
                        });
                    }
                }
            }
            ast::Item::Struct(struct_item) => {
                // Bail if not an ink! event `struct`.
                if !Event::can_cast(struct_item.syntax())
                    && !EventV2::can_cast(struct_item.syntax())
                {
                    return;
                }

                // Adds ink! topic.
                results.push(Completion {
                    label: "#[ink(topic)]..field: ..".to_owned(),
                    range,
                    edit: text_edit::add_topic(&struct_item, range, None, None),
                    detail: Some("ink! topic".to_owned()),
                    kind: CompletionKind::Field,
                });
            }
            _ => (),
        }
    } else {
        // Computes root-level ink! entity completions.
        if file.contracts().is_empty()
            && is_line_affix_of(&["mod", "contract"])
            && !is_next_node_error()
        {
            // Adds ink! contract.
            results.push(Completion {
                label: "#[ink::contract]..mod contract {...}".to_owned(),
                range,
                edit: text_edit::add_contract(range, None, version),
                detail: Some("ink! contract".to_owned()),
                kind: CompletionKind::Mod,
            });
        }

        // Adds ink! event 2.0.
        add_event_v2!();

        // Adds ink! trait definition.
        if is_line_affix_of(&["trait", "definition", "trait_definition"]) {
            results.push(Completion {
                label: "#[ink::trait_definition]..pub trait TraitDefinition {...}".to_owned(),
                range,
                edit: text_edit::add_trait_def(range, None),
                detail: Some("ink! trait definition".to_owned()),
                kind: CompletionKind::Trait,
            });
        }

        // Adds ink! chain extension.
        if is_line_affix_of(&["trait", "chain", "extension", "chain_extension"]) {
            results.push(Completion {
                label: format!(
                    "#[ink::chain_extension{}]..pub trait ChainExtension {{...}}",
                    if version == Version::V5 { "(...)" } else { "" }
                ),
                range,
                edit: text_edit::add_chain_extension(range, None, version),
                detail: Some("ink! chain extension".to_owned()),
                kind: CompletionKind::Trait,
            });
        }

        // Adds ink! storage item.
        if is_line_affix_of(&["struct", "enum", "storage", "item", "storage_item"]) {
            results.push(Completion {
                label: "#[ink::storage_item]..pub struct StorageItem {...}".to_owned(),
                range,
                edit: text_edit::add_storage_item(range, None),
                detail: Some("ink! storage item".to_owned()),
                kind: CompletionKind::Struct,
            });
        }

        // Adds ink! environment.
        if is_line_affix_of(&[
            "struct",
            "enum",
            "environment",
            "derive",
            "scale",
            "scale_derive",
            "scale_info",
            "encode",
            "decode",
            "type",
            "info",
            "type_info",
        ]) {
            results.push(Completion {
                label: format!(
                    "{}..pub enum MyEnvironment {{...}}",
                    if version == Version::V5 {
                        "#[ink::scale_derive(TypeInfo)]"
                    } else {
                        "#[derive(scale_info::TypeInfo)]"
                    }
                ),
                range,
                edit: text_edit::add_environment(range, None, version),
                detail: Some("ink! environment".to_owned()),
                kind: CompletionKind::Enum,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::{parse_offset_at, remove_whitespace, PartialMatchStr};

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
                vec!["ink::event", "ink::scale_derive", "ink::storage_item"],
                vec!["::event", "::scale_derive", "::storage_item"],
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
            fixtures,
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
                vec![],
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
                vec![
                    (
                        "#[ink::event(",
                        Some("("),
                        vec![
                            ("anonymous", Some("("), Some("(")),
                            (r#"signature_topic="""#, Some("("), Some("(")),
                        ],
                    ),
                    (
                        "#[ink::scale_derive(",
                        Some("("),
                        vec![
                            ("Encode", Some("("), Some("(")),
                            ("Decode", Some("("), Some("(")),
                            ("TypeInfo", Some("("), Some("(")),
                        ],
                    ),
                    (
                        "#[ink_e2e::test(backend",
                        Some("backend"),
                        vec![
                            ("(node)", Some("backend"), Some("backend")),
                            ("(runtime_only)", Some("backend"), Some("backend")),
                        ],
                    ),
                    (
                        "#[ink_e2e::test(backend(",
                        Some("(->"),
                        vec![
                            ("(node)", Some("<-(->"), Some("(->")),
                            ("(runtime_only)", Some("<-(->"), Some("(->")),
                        ],
                    ),
                    (
                        "#[ink_e2e::test(backend()",
                        Some("(->"),
                        vec![
                            ("(node)", Some("<-(->"), Some(")->")),
                            ("(runtime_only)", Some("<-(->"), Some(")->")),
                        ],
                    ),
                    (
                        "#[ink_e2e::test(backend(run",
                        Some("run"),
                        vec![("(runtime_only)", Some("<-(run"), Some("run"))],
                    ),
                    (
                        "#[ink_e2e::test(backend(node",
                        Some("node"),
                        vec![(r#"(url="ws://127.0.0.1:9000")"#, Some("node"), Some("node"))],
                    ),
                    (
                        "#[ink_e2e::test(backend(node(",
                        Some("(->"),
                        vec![(r#"(url="ws://127.0.0.1:9000")"#, Some("<-(->"), Some("(->"))],
                    ),
                    (
                        "#[ink_e2e::test(backend(runtime_only",
                        Some("runtime_only"),
                        vec![(
                            "(sandbox=ink_e2e::MinimalSandbox)",
                            Some("runtime_only"),
                            Some("runtime_only"),
                        )],
                    ),
                    (
                        "#[ink_e2e::test(backend(runtime_only(",
                        Some("(->"),
                        vec![(
                            "(sandbox=ink_e2e::MinimalSandbox)",
                            Some("<-(->"),
                            Some("(->"),
                        )],
                    ),
                ],
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
            ]
            .into_iter()
            .chain(fixtures)
            {
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

    #[test]
    fn entity_completions_works() {
        for version in [Version::V4, Version::V5] {
            for (code, pat, expected_results) in [
                // (code, [(pat, [(edit, pat_start, pat_end)])]) where:
                // code = source code,
                // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
                // edit = the text that will be inserted (represented without whitespace for simplicity),
                // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
                // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

                // Root entities.
                ("", None, vec![]),
                // `mod`.
                (
                    "mod",
                    Some("mo"),
                    vec![("#[ink::contract]", Some("<-mod"), Some("mod"))],
                ),
                (
                    "mo",
                    Some("mo"),
                    vec![("#[ink::contract]", Some("<-mo"), Some("mo"))],
                ),
                (
                    "contract",
                    Some("con"),
                    vec![("#[ink::contract]", Some("<-contract"), Some("contract"))],
                ),
                (
                    "pub mod",
                    Some("mod"),
                    vec![("#[ink::contract]", Some("<-pub"), Some("mod"))],
                ),
                (
                    "pub mod",
                    Some("pub"),
                    vec![("#[ink::contract]", Some("<-pub"), Some("mod"))],
                ),
                (
                    "pub mod",
                    Some("pub "),
                    vec![("#[ink::contract]", Some("<-pub"), Some("mod"))],
                ),
                (
                    "pub mod contract",
                    Some("contract"),
                    vec![("#[ink::contract]", Some("<-pub"), Some("contract"))],
                ),
                (
                    "pub mod contract {",
                    Some("{"),
                    vec![("#[ink::contract]", Some("<-pub"), Some("{"))],
                ),
                (
                    "pub mod contract {}",
                    Some("{"),
                    vec![("#[ink::contract]", Some("<-pub"), Some("}"))],
                ),
                (
                    r"
//! ink! example.

pub mod contract",
                    Some("mod"),
                    vec![("#[ink::contract]", Some("<-pub"), Some("contract"))],
                ),
                (
                    r"
#[ink::contract]
pub mod contract {}

pub mod",
                    Some("mod->"),
                    vec![],
                ),
                (
                    r"
#[ink::contract]
pub mod contract1 {}

pub mod contract2",
                    Some("contract2"),
                    vec![],
                ),
                (
                    r"
pub mod

#[ink::contract]
pub mod contract {}",
                    Some("mod"),
                    vec![],
                ),
                (
                    r"
pub mod contract1

#[ink::contract]
pub mod contract2 {}",
                    Some("contract1"),
                    vec![],
                ),
                (
                    r"
#[ink::contract]
pub mod contract1 {
    pub mod contract2
}",
                    Some("contract2"),
                    vec![],
                ),
                // `trait`.
                (
                    "trait",
                    Some("trait"),
                    vec![
                        ("#[ink::trait_definition]", Some("<-trait"), Some("trait")),
                        ("#[ink::chain_extension", Some("<-trait"), Some("trait")),
                    ],
                ),
                (
                    "trait_def",
                    Some("trait_def"),
                    vec![(
                        "#[ink::trait_definition]",
                        Some("<-trait_def"),
                        Some("trait_def"),
                    )],
                ),
                (
                    "chain",
                    Some("chain"),
                    vec![("#[ink::chain_extension", Some("<-chain"), Some("chain"))],
                ),
                (
                    "extension",
                    Some("extension"),
                    vec![(
                        "#[ink::chain_extension",
                        Some("<-extension"),
                        Some("extension"),
                    )],
                ),
                (
                    r"
#[ink::trait_definition]
pub trait MyTrait {

}

trait",
                    Some("trait->"),
                    vec![
                        (
                            "#[ink::trait_definition]",
                            Some("<-trait->"),
                            Some("trait->"),
                        ),
                        ("#[ink::chain_extension", Some("<-trait->"), Some("trait->")),
                    ],
                ),
                (
                    r"
#[ink::trait_definition]
pub trait MyTrait1 {

}

trait MyTrait2",
                    Some("MyTrait2"),
                    vec![
                        (
                            "#[ink::trait_definition]",
                            Some("<-trait->"),
                            Some("MyTrait2"),
                        ),
                        (
                            "#[ink::chain_extension",
                            Some("<-trait->"),
                            Some("MyTrait2"),
                        ),
                    ],
                ),
                (
                    r"
trait

#[ink::trait_definition]
pub trait MyTrait {

}",
                    Some("trait"),
                    vec![
                        ("#[ink::trait_definition]", Some("<-trait"), Some("trait")),
                        ("#[ink::chain_extension", Some("<-trait"), Some("trait")),
                    ],
                ),
                (
                    r"
trait MyTrait1

#[ink::trait_definition]
pub trait MyTrait2 {

}",
                    Some("MyTrait1"),
                    vec![
                        (
                            "#[ink::trait_definition]",
                            Some("<-trait"),
                            Some("MyTrait1"),
                        ),
                        ("#[ink::chain_extension", Some("<-trait"), Some("MyTrait1")),
                    ],
                ),
                (
                    r"
#[ink::chain_extension]
pub trait MyChainExtension {

}

trait",
                    Some("trait->"),
                    vec![
                        (
                            "#[ink::trait_definition]",
                            Some("<-trait->"),
                            Some("trait->"),
                        ),
                        ("#[ink::chain_extension", Some("<-trait->"), Some("trait->")),
                    ],
                ),
                // `struct`.
                (
                    "struct",
                    Some("struct"),
                    if version == Version::V5 {
                        vec![
                            ("#[ink::event]", Some("<-struct"), Some("struct")),
                            ("#[ink::storage_item]", Some("<-struct"), Some("struct")),
                            ("pub enum MyEnvironment {", Some("<-struct"), Some("struct")),
                        ]
                    } else {
                        vec![
                            ("#[ink::storage_item]", Some("<-struct"), Some("struct")),
                            ("pub enum MyEnvironment {", Some("<-struct"), Some("struct")),
                        ]
                    },
                ),
                (
                    "event",
                    Some("event"),
                    if version == Version::V5 {
                        vec![("#[ink::event]", Some("<-event"), Some("event"))]
                    } else {
                        vec![]
                    },
                ),
                (
                    "storage",
                    Some("storage"),
                    vec![("#[ink::storage_item]", Some("<-storage"), Some("storage"))],
                ),
                (
                    "environ",
                    Some("environ"),
                    vec![("pub enum MyEnvironment", Some("<-environ"), Some("environ"))],
                ),
                // `enum`.
                (
                    "enum",
                    Some("enum"),
                    vec![
                        ("#[ink::storage_item]", Some("<-enum"), Some("enum")),
                        ("pub enum MyEnvironment {", Some("<-enum"), Some("enum")),
                    ],
                ),
                // `mod` entities.
                (
                    r"
mod contract {

}",
                    Some("contract {\n"),
                    vec![],
                ),
                (
                    r"
#[ink::contract]
mod contract {

}",
                    Some("contract {\n"),
                    if version == Version::V5 {
                        vec![
                            (
                                "#[ink(storage)]",
                                Some("contract {\n"),
                                Some("contract {\n"),
                            ),
                            ("#[ink::event]", Some("contract {\n"), Some("contract {\n")),
                            ("#[ink(event)]", Some("contract {\n"), Some("contract {\n")),
                        ]
                    } else {
                        vec![
                            (
                                "#[ink(storage)]",
                                Some("contract {\n"),
                                Some("contract {\n"),
                            ),
                            ("#[ink(event)]", Some("contract {\n"), Some("contract {\n")),
                        ]
                    },
                ),
                (
                    r"
#[ink::contract]
mod contract {
    struct
}",
                    Some("struct"),
                    if version == Version::V5 {
                        vec![
                            ("#[ink(storage)]", Some("<-    struct"), Some("struct")),
                            ("#[ink::event]", Some("<-    struct"), Some("struct")),
                            ("#[ink(event)]", Some("<-    struct"), Some("struct")),
                        ]
                    } else {
                        vec![
                            ("#[ink(storage)]", Some("<-    struct"), Some("struct")),
                            ("#[ink(event)]", Some("<-    struct"), Some("struct")),
                        ]
                    },
                ),
                (
                    r"
#[ink::contract]
mod contract {
    storage
}",
                    Some("storage"),
                    vec![("#[ink(storage)]", Some("<-    storage"), Some("storage"))],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    event
}",
                    Some("event"),
                    if version == Version::V5 {
                        vec![
                            ("#[ink::event]", Some("<-    event"), Some("event")),
                            ("#[ink(event)]", Some("<-    event"), Some("event")),
                        ]
                    } else {
                        vec![("#[ink(event)]", Some("<-    event"), Some("event"))]
                    },
                ),
                (
                    r"
#[ink::contract]
mod contract {
    #[ink(storage)]
    pub struct Contract;

    #[ink(event)]
    pub struct Event {}

    struct
}",
                    Some("struct->"),
                    if version == Version::V5 {
                        vec![
                            ("#[ink::event]", Some("<-    struct->"), Some("struct->")),
                            ("#[ink(event)]", Some("<-    struct->"), Some("struct->")),
                        ]
                    } else {
                        vec![("#[ink(event)]", Some("<-    struct->"), Some("struct->"))]
                    },
                ),
                (
                    r"
#[ink::contract]
mod contract {
    struct

    #[ink(event)]
    pub struct Event {}
}",
                    Some("struct"),
                    // No `storage` suggestion because `event` item is parsed as an error node.
                    if version == Version::V5 {
                        vec![
                            ("#[ink::event]", Some("<-    struct"), Some("struct")),
                            ("#[ink(event)]", Some("<-    struct"), Some("struct")),
                        ]
                    } else {
                        vec![("#[ink(event)]", Some("<-    struct"), Some("struct"))]
                    },
                ),
                (
                    r"
#[ink::contract]
mod contract {
    struct Contract

    #[ink(event)]
    pub struct Event {}
}",
                    Some("struct"),
                    if version == Version::V5 {
                        vec![
                            ("#[ink(storage)]", Some("<-    struct"), Some("Contract")),
                            ("#[ink::event]", Some("<-    struct"), Some("Contract")),
                            ("#[ink(event)]", Some("<-    struct"), Some("Contract")),
                        ]
                    } else {
                        vec![
                            ("#[ink(storage)]", Some("<-    struct"), Some("Contract")),
                            ("#[ink(event)]", Some("<-    struct"), Some("Contract")),
                        ]
                    },
                ),
                (
                    r"
#[ink::contract]
mod contract {
    #[ink(storage)]
    pub struct Contract;

    storage
}",
                    Some("storage"),
                    vec![],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    #[ink(event)]
    pub struct Event {}

    event
}",
                    Some("event->"),
                    if version == Version::V5 {
                        vec![
                            ("#[ink::event]", Some("<-    event->"), Some("event->")),
                            ("#[ink(event)]", Some("<-    event->"), Some("event->")),
                        ]
                    } else {
                        vec![("#[ink(event)]", Some("<-    event->"), Some("event->"))]
                    },
                ),
                (
                    r"
#[cfg(test)]
mod tests {

}",
                    Some("tests {\n"),
                    vec![("#[ink::test]", Some("tests {\n"), Some("tests {\n"))],
                ),
                (
                    r"
#[cfg(test)]
mod tests {
    fn
}",
                    Some("fn"),
                    vec![("#[ink::test]", Some("<-    fn"), Some("fn"))],
                ),
                (
                    r#"
#[cfg(all(test, feature = "e2e-tests"))]
mod e2e_tests {

}"#,
                    Some("e2e_tests {\n"),
                    vec![
                        ("#[ink::test]", Some("e2e_tests {\n"), Some("e2e_tests {\n")),
                        (
                            "#[ink_e2e::test]",
                            Some("e2e_tests {\n"),
                            Some("e2e_tests {\n"),
                        ),
                    ],
                ),
                (
                    r#"
#[cfg(all(test, feature = "e2e-tests"))]
mod e2e_tests {
    e2e
}"#,
                    Some("e2e->"),
                    vec![("#[ink_e2e::test]", Some("<-    e2e->"), Some("e2e->"))],
                ),
                // `impl` entities.
                (
                    r"
impl Contract {

}",
                    Some("Contract {\n"),
                    vec![],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    impl Contract {

    }
}",
                    Some("Contract {\n"),
                    vec![
                        (
                            "#[ink(constructor)]",
                            Some("Contract {\n"),
                            Some("Contract {\n"),
                        ),
                        (
                            "#[ink(message)]",
                            Some("Contract {\n"),
                            Some("Contract {\n"),
                        ),
                    ],
                ),
                (
                    r"
#[ink(impl)]
impl Contract {

}",
                    Some("Contract {\n"),
                    vec![
                        (
                            "#[ink(constructor)]",
                            Some("Contract {\n"),
                            Some("Contract {\n"),
                        ),
                        (
                            "#[ink(message)]",
                            Some("Contract {\n"),
                            Some("Contract {\n"),
                        ),
                    ],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    impl Contract {
        fn
    }
}",
                    Some("fn"),
                    vec![
                        ("#[ink(constructor)]", Some("<-        fn"), Some("fn")),
                        ("#[ink(message)]", Some("<-        fn"), Some("fn")),
                    ],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    impl Contract {
        new
    }
}",
                    Some("new"),
                    vec![("#[ink(constructor)]", Some("<-        new"), Some("new"))],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    impl Contract {
        message
    }
}",
                    Some("message"),
                    vec![(
                        "#[ink(message)]",
                        Some("<-        message"),
                        Some("message"),
                    )],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    impl Contract {
        #[ink(constructor)]
        pub fn new() -> Self {}

        fn
    }
}",
                    Some("fn->"),
                    vec![
                        ("#[ink(constructor)]", Some("<-        fn->"), Some("fn->")),
                        ("#[ink(message)]", Some("<-        fn->"), Some("fn->")),
                    ],
                ),
                (
                    r"
#[ink::contract]
mod contract {
    impl Contract {
        fn

        #[ink(constructor)]
        pub fn new() -> Self {}
    }
}",
                    Some("fn"),
                    vec![
                        ("#[ink(constructor)]", Some("<-        fn"), Some("fn")),
                        ("#[ink(message)]", Some("<-        fn"), Some("fn")),
                    ],
                ),
                // `trait` entities.
                (
                    r"
pub trait MyTrait {

}",
                    Some("MyTrait {\n"),
                    vec![],
                ),
                (
                    r"
#[ink::trait_definition]
pub trait MyTrait {

}",
                    Some("MyTrait {\n"),
                    vec![("#[ink(message)]", Some("MyTrait {\n"), Some("MyTrait {\n"))],
                ),
                (
                    r"
#[ink::trait_definition]
pub trait MyTrait {
    fn
}",
                    Some("fn"),
                    vec![("#[ink(message)]", Some("<-    fn"), Some("fn"))],
                ),
                (
                    r"
#[ink::chain_extension]
pub trait MyChainExtension {

}",
                    Some("MyChainExtension {\n"),
                    vec![
                        (
                            "type ErrorCode = ()",
                            Some("MyChainExtension {\n"),
                            Some("MyChainExtension {\n"),
                        ),
                        (
                            if version == Version::V5 {
                                "#[ink(function = 1)]"
                            } else {
                                "#[ink(extension = 1)]"
                            },
                            Some("MyChainExtension {\n"),
                            Some("MyChainExtension {\n"),
                        ),
                    ],
                ),
                (
                    r"
#[ink::chain_extension]
pub trait MyChainExtension {
    type
}",
                    Some("type"),
                    vec![("type ErrorCode = ()", Some("<-    type"), Some("type"))],
                ),
                (
                    r"
#[ink::chain_extension]
pub trait MyChainExtension {
    fn
}",
                    Some("fn"),
                    vec![(
                        if version == Version::V5 {
                            "#[ink(function = 1)]"
                        } else {
                            "#[ink(extension = 1)]"
                        },
                        Some("<-    fn"),
                        Some("fn"),
                    )],
                ),
                // `struct` entities.
                (
                    r"
struct MyStruct {

}",
                    Some("MyStruct {\n"),
                    vec![],
                ),
                (
                    r"
struct MyStruct {
    field
}",
                    Some("field"),
                    vec![],
                ),
                (
                    r"
#[ink(storage)]
struct MyStruct {

}",
                    Some("MyStruct {\n"),
                    vec![],
                ),
                (
                    if version == Version::V5 {
                        r"
#[ink::event]
struct MyStruct {

}"
                    } else {
                        r"
#[ink(event)]
struct MyStruct {

}"
                    },
                    Some("MyStruct {\n"),
                    vec![("#[ink(topic)]", Some("MyStruct {\n"), Some("MyStruct {\n"))],
                ),
                (
                    if version == Version::V5 {
                        r"
#[ink::event]
struct MyStruct {
    field
}"
                    } else {
                        r"
#[ink(event)]
struct MyStruct {
    field
}"
                    },
                    Some("field"),
                    vec![("#[ink(topic)]", Some("<-    field"), Some("field"))],
                ),
                // multi-line items.
                (
                    r"
mod contract {
}",
                    Some("mod contract {"),
                    vec![],
                ),
                (
                    r"
mod contract {
}",
                    Some("}"),
                    vec![],
                ),
                (
                    r"
#[ink::contract]
mod contract {
}",
                    Some("#[ink::contract]\n"),
                    vec![],
                ),
                (
                    r"
#[ink::trait_definition]
pub trait MyTrait {
}",
                    Some("#[ink::trait_definition]\n"),
                    vec![],
                ),
            ] {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

                let mut results = Vec::new();
                entity_completions(&mut results, &InkFile::parse(code), offset, version);

                assert_eq!(
                    results
                        .iter()
                        .map(|completion| (
                            PartialMatchStr::from(completion.edit.text.as_str()),
                            completion.range
                        ))
                        .collect::<Vec<(PartialMatchStr, TextRange)>>(),
                    expected_results
                        .into_iter()
                        .map(|(edit, pat_start, pat_end)| (
                            PartialMatchStr::from(edit),
                            TextRange::new(
                                TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                                TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                            )
                        ))
                        .collect::<Vec<(PartialMatchStr, TextRange)>>(),
                    "code: {code}, version: {:?}",
                    version
                );
            }
        }
    }
}
