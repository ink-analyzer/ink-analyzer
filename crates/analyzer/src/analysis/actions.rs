//! ink! attribute code/intent actions.

use ink_analyzer_ir::ast::HasAttrs;
use ink_analyzer_ir::syntax::{
    AstNode, SyntaxElement, SyntaxKind, SyntaxNode, TextRange, TextSize,
};
use ink_analyzer_ir::{ast, FromAST, FromSyntax, InkAttribute, InkAttributeKind, InkFile};

use super::utils;
use crate::analysis::text_edit::TextEdit;

/// An ink! attribute code/intent action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Action {
    /// Label which identifies the action.
    pub label: String,
    /// The kind of the action (e.g quickfix or refactor).
    pub kind: ActionKind,
    /// Range where the action will be applied.
    pub range: TextRange,
    /// Text edits that will performed by the action.
    pub edits: Vec<TextEdit>,
}

/// Computes ink! attribute actions for the given offset.
pub fn actions(file: &InkFile, range: TextRange) -> Vec<Action> {
    let mut results = Vec::new();

    // Compute AST item-based ink! attribute actions.
    ast_item_actions(&mut results, file, range);

    // Compute ink! attribute actions based on focused ink! attribute.
    ink_attribute_actions(&mut results, file, range);

    results
}

/// The kind of the action (e.g quickfix or refactor).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum ActionKind {
    QuickFix,
    Refactor,
}

impl Action {
    /// Removes an ink! item.
    pub(crate) fn remove_item(item: &SyntaxNode) -> Self {
        Self {
            label: "Remove item.".to_string(),
            kind: ActionKind::QuickFix,
            range: item.text_range(),
            edits: vec![TextEdit::delete(item.text_range())],
        }
    }

    /// Removes an ink! attribute.
    pub(crate) fn remove_attribute(attr: &InkAttribute) -> Self {
        Self {
            label: format!("Remove `{}` attribute.", attr.syntax()),
            kind: ActionKind::QuickFix,
            range: attr.syntax().text_range(),
            edits: vec![TextEdit::delete(attr.syntax().text_range())],
        }
    }

    /// Moves an item (i.e a syntax node) to a new location.
    pub(crate) fn move_item(
        item: &SyntaxNode,
        offset: TextSize,
        label: String,
        indent_option: Option<&str>,
    ) -> Self {
        Self::move_item_with_affixes(item, offset, label, indent_option, None, None)
    }

    /// Moves an item (i.e a syntax node) to a new location with affixes (i.e. prefixes and suffixes).
    pub(crate) fn move_item_with_affixes(
        item: &SyntaxNode,
        offset: TextSize,
        label: String,
        indent_option: Option<&str>,
        prefix_option: Option<&str>,
        suffix_option: Option<&str>,
    ) -> Self {
        // Gets the unindented insert text.
        // NOTE: removes item's top-level indenting (if any).
        let mut insert_text = utils::item_indenting(item).map_or(item.to_string(), |item_indent| {
            utils::reduce_indenting(item.to_string().as_str(), item_indent.as_str())
        });

        // Applies indenting based on insert location (if specified).
        if let Some(indent) = indent_option {
            insert_text = utils::apply_indenting(insert_text.as_str(), indent);
        }

        // Adds prefix (if any).
        if let Some(prefix) = prefix_option {
            insert_text = format!("{prefix}{insert_text}");
        }

        // Adds suffix (if any).
        if let Some(suffix) = suffix_option {
            insert_text = format!("{insert_text}{suffix}");
        }

        Self {
            label,
            kind: ActionKind::QuickFix,
            range: item.text_range(),
            edits: vec![
                // Insert a copy of the item at the specified offset.
                TextEdit::insert(insert_text, offset),
                // Delete the item from current location.
                TextEdit::delete(item.text_range()),
            ],
        }
    }
}

/// Computes AST item-based ink! attribute actions at the given offset.
pub fn ast_item_actions(results: &mut Vec<Action>, file: &InkFile, range: TextRange) {
    // Only computes actions if a focused element can be determined.
    if let Some(focused_elem) = utils::focused_element(file, range) {
        // Only computes actions if the focused element isn't part of an attribute.
        if utils::covering_attribute(file, range).is_none() {
            // Only computes actions if the parent AST item can be determined.
            if let Some(ast_item) = utils::parent_ast_item(file, range) {
                // Gets the covering struct record field if the AST item is a struct.
                let record_field: Option<ast::RecordField> = match &ast_item {
                    ast::Item::Struct(_) => match focused_elem {
                        SyntaxElement::Node(it) => ink_analyzer_ir::closest_ancestor_ast_type(&it),
                        SyntaxElement::Token(it) => ink_analyzer_ir::closest_ancestor_ast_type(&it),
                    },
                    _ => None,
                };

                // Only computes actions if the focus is on either a struct record field or
                // an AST item's declaration (i.e not inside the AST item's item list or body) for
                // an item that can be annotated with ink! attributes.
                if record_field.is_some() || is_focused_on_ast_item_declaration(&ast_item, range) {
                    // Retrieves the target syntax node as either the covering struct field (if present) or
                    // the parent AST item (for all other cases).
                    let target = record_field
                        .as_ref()
                        .map_or(ast_item.syntax(), AstNode::syntax);

                    // Only suggest ink! attribute macros if the AST item has no other ink! attributes.
                    if ink_analyzer_ir::ink_attrs(target).next().is_none() {
                        // Suggests ink! attribute macros based on the context.
                        let mut ink_macro_suggestions =
                            utils::valid_ink_macros_by_syntax_kind(target.kind());

                        // Filters out duplicate and invalid ink! attribute macro actions based on parent ink! scope (if any).
                        utils::remove_duplicate_ink_macro_suggestions(
                            &mut ink_macro_suggestions,
                            target,
                        );
                        utils::remove_invalid_ink_macro_suggestions_for_parent_ink_scope(
                            &mut ink_macro_suggestions,
                            target,
                        );

                        if !ink_macro_suggestions.is_empty() {
                            // Determines the insertion offset and affixes for the action.
                            let (insert_offset, insert_prefix, insert_suffix) =
                                utils::ink_attribute_insertion_offset_and_affixes(target);

                            // Add ink! attribute macro actions to accumulator.
                            for macro_kind in ink_macro_suggestions {
                                results.push(Action {
                                    label: format!("Add ink! {macro_kind} attribute macro."),
                                    kind: ActionKind::Refactor,
                                    range: TextRange::new(insert_offset, insert_offset),
                                    edits: vec![TextEdit::insert(
                                        format!(
                                            "{}#[{}]{}",
                                            insert_prefix.as_deref().unwrap_or_default(),
                                            macro_kind.path_as_str(),
                                            insert_suffix.as_deref().unwrap_or_default()
                                        ),
                                        insert_offset,
                                    )],
                                });
                            }
                        }
                    }

                    // Gets the primary ink! attribute candidate (if any).
                    let primary_ink_attr_candidate =
                        utils::primary_ink_attribute_candidate(ink_analyzer_ir::ink_attrs(target))
                            .map(|(attr, ..)| attr);

                    // Suggests ink! attribute arguments based on the context.
                    let mut ink_arg_suggestions =
                        if let Some(ink_attr) = primary_ink_attr_candidate.as_ref() {
                            // Make suggestions based on the first valid ink! attribute (if any).
                            utils::valid_sibling_ink_args(*ink_attr.kind())
                        } else {
                            // Otherwise make suggestions based on the AST item's syntax kind.
                            utils::valid_ink_args_by_syntax_kind(target.kind())
                        };

                    // Filters out duplicate ink! attribute argument actions.
                    utils::remove_duplicate_ink_arg_suggestions(&mut ink_arg_suggestions, target);
                    // Filters out conflicting ink! attribute argument actions.
                    utils::remove_conflicting_ink_arg_suggestions(&mut ink_arg_suggestions, target);
                    // Filters out invalid ink! attribute argument actions based on parent ink! scope
                    // if there's either no valid ink! attribute macro (not argument) applied to the item
                    // (i.e either no valid ink! attribute macro or only ink! attribute arguments).
                    if primary_ink_attr_candidate.is_none()
                        || !matches!(
                            primary_ink_attr_candidate.as_ref().map(|attr| attr.kind()),
                            Some(InkAttributeKind::Macro(_))
                        )
                    {
                        utils::remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
                            &mut ink_arg_suggestions,
                            target,
                        );
                    }

                    if !ink_arg_suggestions.is_empty() {
                        // Add ink! attribute argument actions to accumulator.
                        for arg_kind in ink_arg_suggestions {
                            // Determines the insertion offset and affixes for the action and whether or not an existing attribute can be extended.
                            let ((insert_offset, insert_prefix, insert_suffix), is_extending) =
                                primary_ink_attr_candidate
                                    .as_ref()
                                    .and_then(|ink_attr| {
                                        // Try to extend an existing attribute (if possible).
                                        utils::ink_arg_insertion_offset_and_affixes(
                                            arg_kind, ink_attr,
                                        )
                                        .map(
                                            |(insert_offset, insert_prefix, insert_suffix)| {
                                                (
                                                    (
                                                        insert_offset,
                                                        Some(insert_prefix.to_string()),
                                                        Some(insert_suffix.to_string()),
                                                    ),
                                                    true,
                                                )
                                            },
                                        )
                                    })
                                    .unwrap_or((
                                        // Fallback to inserting a new attribute.
                                        utils::ink_attribute_insertion_offset_and_affixes(target),
                                        false,
                                    ));

                            // Sets the text range for the edit.
                            let edit_range = TextRange::new(insert_offset, insert_offset);

                            // Adds ink! attribute argument action to accumulator.
                            let (edit, snippet) = utils::ink_arg_insertion_text(
                                arg_kind,
                                Some(edit_range.end()),
                                primary_ink_attr_candidate
                                    .as_ref()
                                    .map(InkAttribute::syntax),
                            );
                            results.push(Action {
                                label: format!("Add ink! {arg_kind} attribute argument."),
                                kind: ActionKind::Refactor,
                                range: edit_range,
                                edits: vec![TextEdit::insert_with_snippet(
                                    format!(
                                        "{}{}{}",
                                        insert_prefix.as_deref().unwrap_or_default(),
                                        if is_extending {
                                            edit
                                        } else {
                                            format!("#[ink({edit})]")
                                        },
                                        insert_suffix.as_deref().unwrap_or_default(),
                                    ),
                                    insert_offset,
                                    snippet.map(|snippet| {
                                        format!(
                                            "{}{}{}",
                                            insert_prefix.as_deref().unwrap_or_default(),
                                            if is_extending {
                                                snippet
                                            } else {
                                                format!("#[ink({snippet})]")
                                            },
                                            insert_suffix.as_deref().unwrap_or_default(),
                                        )
                                    }),
                                )],
                            });
                        }
                    }
                }
            }
        }
    }
}

/// Computes AST item-based ink! attribute actions at the given offset.
pub fn ink_attribute_actions(results: &mut Vec<Action>, file: &InkFile, range: TextRange) {
    // Only computes actions if the focused range is part of/covered by an ink! attribute.
    if let Some(ink_attr) = utils::covering_ink_attribute(file, range) {
        // Only computes actions for closed attributes because
        // unclosed attributes are too tricky for useful contextual edits.
        if ink_attr.ast().r_brack_token().is_some() {
            // Suggests ink! attribute arguments based on the context.
            let mut ink_arg_suggestions = utils::valid_sibling_ink_args(*ink_attr.kind());

            if let Some(attr_parent) = ink_attr.syntax().parent() {
                // Filters out duplicate ink! attribute argument actions.
                utils::remove_duplicate_ink_arg_suggestions(&mut ink_arg_suggestions, &attr_parent);

                // Filters out conflicting ink! attribute argument actions.
                utils::remove_conflicting_ink_arg_suggestions(
                    &mut ink_arg_suggestions,
                    &attr_parent,
                );

                // Filters out invalid (based on parent ink! scope) ink! attribute argument actions,
                // Doesn't apply to ink! attribute macros as their arguments are not influenced by the parent scope.
                if let InkAttributeKind::Arg(_) = ink_attr.kind() {
                    utils::remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
                        &mut ink_arg_suggestions,
                        &attr_parent,
                    );
                }
            }

            // Adds ink! attribute argument actions to accumulator.
            for arg_kind in ink_arg_suggestions {
                // Determines the insertion offset and affixes for the action.
                if let Some((insert_offset, insert_prefix, insert_suffix)) =
                    utils::ink_arg_insertion_offset_and_affixes(arg_kind, &ink_attr)
                {
                    // Computes the text range for the edit.
                    let edit_range = TextRange::new(insert_offset, insert_offset);

                    // Adds ink! attribute argument action to accumulator.
                    let (edit, snippet) = utils::ink_arg_insertion_text(
                        arg_kind,
                        Some(edit_range.end()),
                        Some(ink_attr.syntax()),
                    );
                    results.push(Action {
                        label: format!("Add ink! {arg_kind} attribute argument."),
                        kind: ActionKind::Refactor,
                        range: edit_range,
                        edits: vec![TextEdit::insert_with_snippet(
                            format!("{insert_prefix}{edit}{insert_suffix}"),
                            insert_offset,
                            snippet
                                .map(|snippet| format!("{insert_prefix}{snippet}{insert_suffix}")),
                        )],
                    });
                }
            }
        }
    }
}

/// Determines if the offset is focused on an AST item's declaration
/// (i.e not inside the AST item's item list or body) for an item that can be annotated with ink! attributes.
fn is_focused_on_ast_item_declaration(item: &ast::Item, range: TextRange) -> bool {
    // Shared logic that ensures the range is not inside the AST item's item list or body.
    let is_focused_on_declaration_impl = |item_list: Option<&SyntaxNode>| {
        item_list.map_or(true, |node| {
            let opening_curly_end = ink_analyzer_ir::first_child_token(node).and_then(|token| {
                (token.kind() == SyntaxKind::L_CURLY).then_some(token.text_range().end())
            });
            let closing_curly_start = ink_analyzer_ir::last_child_token(node).and_then(|token| {
                (token.kind() == SyntaxKind::R_CURLY).then_some(token.text_range().start())
            });

            let opening_curly_end_or_node_end =
                opening_curly_end.unwrap_or(node.text_range().end());
            let closing_curly_start_or_node_end =
                closing_curly_start.unwrap_or(node.text_range().end());

            // We already know the range is focused on the AST item,
            // this makes sure either it's before the body (if any) or after the body (i.e covering the closing curly bracket).
            range.end() <= opening_curly_end_or_node_end
                || closing_curly_start_or_node_end <= range.start()
        })
    };

    // Gets the last attribute for the AST item (if any).
    item.attrs().last()
        // Ensures start offset is either after the last attribute (if any) or after the beginning of the AST item.
        .map_or(item.syntax().text_range().start(), |attr| attr.syntax().text_range().end()) <= range.start()
        // Ensures end offset is before the end of the AST item.
        && range.end() <= item.syntax().text_range().end()
        // Ensures the offset in not inside the AST item's item list or body.
        && match item {
            // We only care about AST items that can be annotated with ink! attributes.
            ast::Item::Module(item) => {
                is_focused_on_declaration_impl(item.item_list().as_ref().map(AstNode::syntax))
            }
            ast::Item::Trait(item) => is_focused_on_declaration_impl(
                item.assoc_item_list().as_ref().map(AstNode::syntax),
            ),
            ast::Item::Enum(item) => is_focused_on_declaration_impl(
                item.variant_list().as_ref().map(AstNode::syntax),
            ),
            ast::Item::Struct(item) => {
                is_focused_on_declaration_impl(item.field_list().as_ref().map(AstNode::syntax))
            }
            ast::Item::Union(item) => is_focused_on_declaration_impl(
                item.record_field_list().as_ref().map(AstNode::syntax),
            ),
            ast::Item::Fn(item) => {
                is_focused_on_declaration_impl(item.body().as_ref().map(AstNode::syntax))
            }
            ast::Item::Impl(item) => is_focused_on_declaration_impl(
                item.assoc_item_list().as_ref().map(AstNode::syntax),
            ),
            // Everything else is ignored.
            _ => false,
        }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::syntax::TextSize;
    use ink_analyzer_ir::FromSyntax;
    use test_utils::{parse_offset_at, remove_whitespace};

    #[test]
    fn ast_item_actions_works() {
        for (code, pat, expected_results) in [
            // (code, pat, [(edit, pat_start, pat_end)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the text that will inserted (represented without whitespace for simplicity),
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // No AST item declaration in focus.
            ("// A comment in focus.", None, vec![]),
            (
                r#"
                    mod my_module {
                        // The module declaration is out of focus when this comment is in focus.
                    }
                "#,
                Some("<-//"),
                vec![],
            ),
            // Module focus.
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("<-mod"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("my_con"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("<-{"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("{"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("}"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("<-}"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    #[foo]
                    mod my_contract {
                    }
                "#,
                Some("<-mod"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("<-mod"),
                vec![
                    ("(env=)", Some("#[ink::contract"), Some("#[ink::contract")),
                    (
                        "(keep_attr=)",
                        Some("#[ink::contract"),
                        Some("#[ink::contract"),
                    ),
                ],
            ),
            // Trait focus.
            (
                r#"
                    pub trait MyTrait {
                    }
                "#,
                Some("<-pub"),
                vec![
                    ("#[ink::chain_extension]", Some("<-pub"), Some("<-pub")),
                    ("#[ink::trait_definition]", Some("<-pub"), Some("<-pub")),
                ],
            ),
            (
                r#"
                    #[ink::chain_extension]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-pub"),
                vec![],
            ),
            (
                r#"
                    #[ink::trait_definition]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-pub"),
                vec![
                    (
                        "(keep_attr=)",
                        Some("#[ink::trait_definition"),
                        Some("#[ink::trait_definition"),
                    ),
                    (
                        "(namespace=)",
                        Some("#[ink::trait_definition"),
                        Some("#[ink::trait_definition"),
                    ),
                ],
            ),
            // ADT focus.
            (
                r#"
                    enum MyEnum {
                    }
                "#,
                Some("<-enum"),
                vec![("#[ink::storage_item]", Some("<-enum"), Some("<-enum"))],
            ),
            (
                r#"
                    struct MyStruct {
                    }
                "#,
                Some("<-struct"),
                vec![
                    ("#[ink::storage_item]", Some("<-struct"), Some("<-struct")),
                    ("#[ink(anonymous)]", Some("<-struct"), Some("<-struct")),
                    ("#[ink(event)]", Some("<-struct"), Some("<-struct")),
                    ("#[ink(storage)]", Some("<-struct"), Some("<-struct")),
                ],
            ),
            (
                r#"
                    union MyUnion {
                    }
                "#,
                Some("<-union"),
                vec![("#[ink::storage_item]", Some("<-union"), Some("<-union"))],
            ),
            // Struct field focus.
            (
                r#"
                    struct MyStruct {
                        value: bool,
                    }
                "#,
                Some("<-value"),
                vec![("#[ink(topic)]", Some("<-value"), Some("<-value"))],
            ),
            // Fn focus.
            (
                r#"
                    fn my_fn() {
                    }
                "#,
                Some("<-fn"),
                vec![
                    ("#[ink::test]", Some("<-fn"), Some("<-fn")),
                    ("#[ink_e2e::test]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(constructor)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(default)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(extension=)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(handle_status=)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(message)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(payable)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(selector=)]", Some("<-fn"), Some("<-fn")),
                ],
            ),
            (
                r#"
                    #[ink::test]
                    fn my_fn() {
                    }
                "#,
                Some("<-fn"),
                vec![],
            ),
            (
                r#"
                    #[ink_e2e::test]
                    fn my_fn() {
                    }
                "#,
                Some("<-fn"),
                vec![
                    (
                        "(additional_contracts=)",
                        Some("#[ink_e2e::test"),
                        Some("#[ink_e2e::test"),
                    ),
                    (
                        "(environment=)",
                        Some("#[ink_e2e::test"),
                        Some("#[ink_e2e::test"),
                    ),
                    (
                        "(keep_attr=)",
                        Some("#[ink_e2e::test"),
                        Some("#[ink_e2e::test"),
                    ),
                ],
            ),
            (
                r#"
                    #[ink(constructor)]
                    fn my_fn() {
                    }
                "#,
                Some("<-fn"),
                vec![
                    (
                        ", default",
                        Some("#[ink(constructor"),
                        Some("#[ink(constructor"),
                    ),
                    (
                        ", payable",
                        Some("#[ink(constructor"),
                        Some("#[ink(constructor"),
                    ),
                    (
                        ", selector=",
                        Some("#[ink(constructor"),
                        Some("#[ink(constructor"),
                    ),
                ],
            ),
            (
                r#"
                    #[ink(event)]
                    struct MyEvent {
                    }
                "#,
                Some("<-struct"),
                vec![(", anonymous", Some("#[ink(event"), Some("#[ink(event"))],
            ),
            (
                r#"
                    #[ink(anonymous)]
                    struct MyEvent {
                    }
                "#,
                Some("<-struct"),
                vec![("event, ", Some("#[ink("), Some("#[ink("))],
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
            let range = TextRange::new(offset, offset);

            let mut results = Vec::new();
            ast_item_actions(&mut results, &InkFile::parse(code), range);

            assert_eq!(
                results
                    .into_iter()
                    .map(|action| (
                        remove_whitespace(action.edits[0].text.clone()),
                        action.edits[0].range
                    ))
                    .collect::<Vec<(String, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(edit, pat_start, pat_end)| (
                        remove_whitespace(edit.to_string()),
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    ))
                    .collect::<Vec<(String, TextRange)>>(),
                "code: {code}"
            );
        }
    }

    #[test]
    fn ink_attribute_actions_works() {
        for (code, pat, expected_results) in [
            // (code, pat, [(edit, pat_start, pat_end)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the text that will inserted (represented without whitespace for simplicity),
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // No ink! attribute in focus.
            ("// A comment in focus.", None, vec![]),
            (
                r#"
                    #[foo]
                    mod my_module {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            (
                r#"
                    #[foo]
                    mod my_module {
                    }
                "#,
                Some("[fo"),
                vec![],
            ),
            (
                r#"
                    #[foo]
                    mod my_module {
                    }
                "#,
                Some("foo]"),
                vec![],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_module {
                    }
                "#,
                Some("<-mod"),
                vec![],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_module {
                    }
                "#,
                Some("my_"),
                vec![],
            ),
            // ink! attribute macros.
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![
                    ("(env=)", Some("<-]"), Some("<-]")),
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("ink::"),
                vec![
                    ("(env=)", Some("<-]"), Some("<-]")),
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("contract]"),
                vec![
                    ("(env=)", Some("<-]"), Some("<-]")),
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![(", keep_attr=", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types,)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![("keep_attr=", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::chain_extension]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            (
                r#"
                    #[ink::trait_definition]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                    ("(namespace=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::trait_definition(namespace="my_namespace")]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![(", keep_attr=", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    enum MyEnum {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    union MyUnion {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::test]
                    fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            (
                r#"
                    #[ink_e2e::test]
                    fn it_works() {
                    }
                "#,
                Some("<-#["),
                vec![
                    ("(additional_contracts=)", Some("<-]"), Some("<-]")),
                    ("(environment=)", Some("<-]"), Some("<-]")),
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                ],
            ),
            // ink! attribute arguments.
            (
                r#"
                    #[ink(storage)]
                    pub struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![(", anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                    }
                "#,
                Some("ink("),
                vec![(", anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                    }
                "#,
                Some("event)]"),
                vec![(", anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event,)]
                    pub struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![("anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                        #[ink(topic)]
                        value: bool,
                    }
                "#,
                Some("#[ink(top"),
                vec![],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                        #[ink(topic)]
                        value: bool,
                    }
                "#,
                Some("<-#[->"),
                vec![],
            ),
            (
                r#"
                    #[ink(constructor)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", payable", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(constructor, payable)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(constructor)]
                    #[ink(payable)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(constructor)]
                    #[ink(payable)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#[->"),
                vec![
                    (", default", Some("<-)]->"), Some("<-)]->")),
                    (", selector=", Some("<-)]->"), Some("<-)]->")),
                ],
            ),
            (
                r#"
                    #[ink(message)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", payable", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(extension=1)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![(", handle_status=", Some("<-)]"), Some("<-)]"))],
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
            let range = TextRange::new(offset, offset);

            let mut results = Vec::new();
            ink_attribute_actions(&mut results, &InkFile::parse(code), range);

            assert_eq!(
                results
                    .into_iter()
                    .map(|action| (
                        remove_whitespace(action.edits[0].text.clone()),
                        action.edits[0].range
                    ))
                    .collect::<Vec<(String, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(edit, pat_start, pat_end)| (
                        remove_whitespace(edit.to_string()),
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    ))
                    .collect::<Vec<(String, TextRange)>>(),
                "code: {code}"
            );
        }
    }

    #[test]
    fn is_focused_on_ast_item_declaration_works() {
        for (code, test_cases) in [
            // (code, [(pat, result)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // result = expected result from calling `is_focused_on_ast_item_declaration` (i.e whether or not an AST item's declaration is in focus),

            // Module.
            (
                r#"
                    #[abc]
                    #[ink::contract]
                    mod my_module {
                        // The module declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::con"), false),
                    (Some("contract]"), true),
                    (Some("<-mod"), true),
                    (Some("mo"), true),
                    (Some("mod"), true),
                    (Some("<-my_module"), true),
                    (Some("my_"), true),
                    (Some("<-my_module"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Trait.
            (
                r#"
                    #[abc]
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        // The trait declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::trait"), false),
                    (Some("definition]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-trait MyTrait"), true),
                    (Some("pub tr"), true),
                    (Some("pub trait"), true),
                    (Some("<-MyTrait"), true),
                    (Some("My"), true),
                    (Some("<-MyTrait"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Enum.
            (
                r#"
                    #[abc]
                    #[ink::storage_item]
                    pub enum MyEnum {
                        // The enum declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::storage"), false),
                    (Some("storage_item]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-enum"), true),
                    (Some("en"), true),
                    (Some("enum"), true),
                    (Some("<-MyEnum"), true),
                    (Some("My"), true),
                    (Some("<-MyEnum"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Struct.
            (
                r#"
                    #[abc]
                    #[ink(event, anonymous)]
                    pub struct MyStruct {
                        // The struct declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink("), false),
                    (Some("(eve"), false),
                    (Some("(event,"), false),
                    (Some(", anon"), false),
                    (Some("anonymous)]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-struct"), true),
                    (Some("st"), true),
                    (Some("struct"), true),
                    (Some("<-MyStruct"), true),
                    (Some("My"), true),
                    (Some("<-MyStruct"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Union.
            (
                r#"
                    #[abc]
                    #[ink::storage_item]
                    pub union MyUnion {
                        // The union declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::storage"), false),
                    (Some("storage_item]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-union"), true),
                    (Some("un"), true),
                    (Some("union"), true),
                    (Some("<-MyUnion"), true),
                    (Some("My"), true),
                    (Some("<-MyUnion"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Fn.
            (
                r#"
                    #[abc]
                    #[ink(constructor, selector=1)]
                    #[ink(payable)]
                    pub fn my_fn() {
                        // The fn declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink("), false),
                    (Some("(con"), false),
                    (Some("(constructor,"), false),
                    (Some(", select"), false),
                    (Some("selector=1)]"), false),
                    (Some("(pay"), false),
                    (Some("payable)]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-fn"), true),
                    (Some("f"), true),
                    (Some("fn"), true),
                    (Some("<-my_fn"), true),
                    (Some("my_"), true),
                    (Some("<-my_fn"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
        ] {
            for (pat, expected_result) in test_cases {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
                let range = TextRange::new(offset, offset);

                let ast_item = InkFile::parse(code)
                    .syntax()
                    .descendants()
                    .filter_map(ast::Item::cast)
                    .next()
                    .unwrap();
                assert_eq!(
                    is_focused_on_ast_item_declaration(&ast_item, range),
                    expected_result,
                    "code: {code}"
                );
            }
        }
    }
}
