//! ink! attribute code/intent actions.

use ink_analyzer_ir::ast::HasAttrs;
use ink_analyzer_ir::syntax::{AstNode, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, TextSize};
use ink_analyzer_ir::{
    ast, ChainExtension, Contract, FromAST, FromInkAttribute, FromSyntax, InkAttribute,
    InkAttributeKind, InkFile, InkImpl, InkMacroKind, TraitDefinition,
};
use itertools::Itertools;

use super::utils;
use crate::analysis::text_edit;
use crate::TextEdit;

pub mod entity;

/// An ink! attribute code/intent action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Action {
    /// Label which identifies the action.
    pub label: String,
    /// The kind of the action (e.g quickfix or refactor).
    pub kind: ActionKind,
    /// Range where the action is activated.
    pub range: TextRange,
    /// Text edits that will performed by the action.
    pub edits: Vec<TextEdit>,
}

/// The kind of the action (e.g quickfix or refactor).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum ActionKind {
    QuickFix,
    Refactor,
}

/// Computes ink! attribute actions for the text range.
pub fn actions(file: &InkFile, range: TextRange) -> Vec<Action> {
    let mut results = Vec::new();

    // Compute AST item-based ink! attribute actions.
    ast_item_actions(&mut results, file, range);

    // Compute ink! attribute actions based on focused ink! attribute.
    ink_attribute_actions(&mut results, file, range);

    results
        .into_iter()
        // Deduplicate by edits.
        .unique_by(|item| item.edits.clone())
        // Format edits.
        .map(|item| Action {
            edits: text_edit::format_edits(item.edits, file).collect(),
            ..item
        })
        .collect()
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

/// Computes AST item-based ink! attribute actions at the given text range.
pub fn ast_item_actions(results: &mut Vec<Action>, file: &InkFile, range: TextRange) {
    match utils::focused_element(file, range) {
        // Computes actions based on focused element (if it can be determined).
        Some(focused_elem) => {
            // Computes an offset for inserting around the focused element
            // (i.e. insert at the end of the focused element except if it's whitespace,
            // in which case insert based on the passed text range).
            let focused_elem_insert_offset = || -> TextSize {
                if focused_elem.kind() == SyntaxKind::WHITESPACE
                    && focused_elem.text_range().contains_range(range)
                {
                    range
                } else {
                    focused_elem.text_range()
                }
                .end()
            };

            // Only computes actions if the focused element isn't part of an attribute.
            if utils::covering_attribute(file, range).is_none() {
                match utils::parent_ast_item(file, range) {
                    // Computes actions based on the parent AST item.
                    Some(ast_item) => {
                        // Gets the covering struct record field (if any) if the AST item is a struct.
                        let record_field: Option<ast::RecordField> =
                            matches!(&ast_item, ast::Item::Struct(_))
                                .then(|| ink_analyzer_ir::closest_ancestor_ast_type(&focused_elem))
                                .flatten();

                        // Only computes ink! attribute actions if the focus is on either a struct record field or
                        // an AST item's declaration (i.e not on attributes nor rustdoc nor inside the AST item's item list or body) for
                        // an item that can be annotated with ink! attributes.
                        if record_field.is_some()
                            || is_focused_on_ast_item_declaration(&ast_item, range)
                        {
                            // Retrieves the target syntax node as either the covering struct field (if present) or
                            // the parent AST item (for all other cases).
                            let target = record_field
                                .as_ref()
                                .map_or(ast_item.syntax(), AstNode::syntax);

                            // Determines text range for item "declaration" (fallbacks to range of the entire item).
                            let item_declaration_text_range = record_field
                                .as_ref()
                                .map(|it| it.syntax().text_range())
                                .or(utils::ast_item_declaration_range(&ast_item))
                                .unwrap_or(ast_item.syntax().text_range());

                            // Suggests ink! attribute macros based on the context.
                            ast_item_add_ink_macro_actions(
                                results,
                                target,
                                item_declaration_text_range,
                            );

                            // Suggests ink! attribute arguments based on the context.
                            ast_item_add_ink_arg_actions(
                                results,
                                target,
                                item_declaration_text_range,
                            );
                        }

                        // Only computes ink! entity actions if the focus is on either an AST item's "declaration" or body
                        // (i.e not on meta - attributes/rustdoc) for an item that can can have ink! attribute descendants.
                        let is_focused_on_body = is_focused_on_ast_item_body(&ast_item, range);
                        if is_focused_on_ast_item_declaration(&ast_item, range)
                            || is_focused_on_body
                        {
                            ast_item_add_ink_entity_actions(
                                results,
                                &ast_item,
                                is_focused_on_body.then_some(focused_elem_insert_offset()),
                            );
                        }
                    }
                    // Computes root-level ink! entity actions if focused element is whitespace in the root of the file (i.e. has no AST parent).
                    None => {
                        let is_in_file_root = focused_elem
                            .parent()
                            .map_or(false, |it| it.kind() == SyntaxKind::SOURCE_FILE);
                        if is_in_file_root {
                            root_ink_entity_actions(results, file, focused_elem_insert_offset());
                        }
                    }
                }
            }
        }
        // Computes root-level ink! entity actions if file is empty.
        None => {
            if file.syntax().text_range().is_empty()
                && file.syntax().text_range().contains_range(range)
            {
                root_ink_entity_actions(results, file, range.end());
            }
        }
    }
}

/// Computes AST item-based ink! attribute macro actions.
fn ast_item_add_ink_macro_actions(
    results: &mut Vec<Action>,
    target: &SyntaxNode,
    range: TextRange,
) {
    // Only suggest ink! attribute macros if the AST item has no other ink! attributes.
    if ink_analyzer_ir::ink_attrs(target).next().is_none() {
        // Suggests ink! attribute macros based on the context.
        let mut ink_macro_suggestions = utils::valid_ink_macros_by_syntax_kind(target.kind());

        // Filters out duplicate and invalid ink! attribute macro actions based on parent ink! scope (if any).
        utils::remove_duplicate_ink_macro_suggestions(&mut ink_macro_suggestions, target);
        utils::remove_invalid_ink_macro_suggestions_for_parent_ink_scope(
            &mut ink_macro_suggestions,
            target,
        );

        if !ink_macro_suggestions.is_empty() {
            // Determines the insertion offset and affixes for the action.
            let insert_offset = utils::ink_attribute_insert_offset(target);

            // Add ink! attribute macro actions to accumulator.
            for macro_kind in ink_macro_suggestions {
                results.push(Action {
                    label: format!("Add ink! {macro_kind} attribute macro."),
                    kind: ActionKind::Refactor,
                    range,
                    edits: vec![TextEdit::insert(
                        format!("#[{}]", macro_kind.path_as_str(),),
                        insert_offset,
                    )],
                });
            }
        }
    }
}

/// Computes AST item-based ink! attribute argument actions.
fn ast_item_add_ink_arg_actions(results: &mut Vec<Action>, target: &SyntaxNode, range: TextRange) {
    // Gets the primary ink! attribute candidate (if any).
    let primary_ink_attr_candidate =
        utils::primary_ink_attribute_candidate(ink_analyzer_ir::ink_attrs(target))
            .map(|(attr, ..)| attr);

    // Suggests ink! attribute arguments based on the context.
    let mut ink_arg_suggestions = match primary_ink_attr_candidate.as_ref() {
        // Make suggestions based on the "primary" valid ink! attribute (if any).
        Some(ink_attr) => utils::valid_sibling_ink_args(*ink_attr.kind()),
        // Otherwise make suggestions based on the AST item's syntax kind.
        None => utils::valid_ink_args_by_syntax_kind(target.kind()),
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
                        utils::ink_arg_insertion_offset_and_affixes(arg_kind, ink_attr).map(
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
                        (utils::ink_attribute_insert_offset(target), None, None),
                        false,
                    ));

            // Adds ink! attribute argument action to accumulator.
            let (edit, snippet) = utils::ink_arg_insertion_text(
                arg_kind,
                Some(insert_offset),
                is_extending
                    .then(|| {
                        primary_ink_attr_candidate
                            .as_ref()
                            .map(InkAttribute::syntax)
                    })
                    .flatten(),
            );
            results.push(Action {
                label: format!("Add ink! {arg_kind} attribute argument."),
                kind: ActionKind::Refactor,
                range: is_extending
                    .then(|| {
                        primary_ink_attr_candidate
                            .as_ref()
                            .map(|it| it.syntax().text_range())
                    })
                    .flatten()
                    .unwrap_or(range),
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

/// Computes AST item-based ink! entity macro actions.
fn ast_item_add_ink_entity_actions(
    results: &mut Vec<Action>,
    item: &ast::Item,
    insert_offset_option: Option<TextSize>,
) {
    let mut add_result = |action_option: Option<Action>| {
        // Add action to accumulator (if any).
        if let Some(action) = action_option {
            results.push(action);
        }
    };
    match item {
        ast::Item::Module(module) => {
            let contract_option = ink_analyzer_ir::ink_attrs(module.syntax())
                .find(|attr| *attr.kind() == InkAttributeKind::Macro(InkMacroKind::Contract))
                .and_then(Contract::cast);
            match contract_option {
                Some(contract) => {
                    // Adds ink! storage if it doesn't exist.
                    if contract.storage().is_none() {
                        add_result(entity::add_storage(
                            &contract,
                            ActionKind::Refactor,
                            insert_offset_option,
                        ));
                    }

                    // Adds ink! event.
                    add_result(entity::add_event(
                        &contract,
                        ActionKind::Refactor,
                        insert_offset_option,
                    ));

                    // Adds ink! constructor.
                    add_result(entity::add_constructor_to_contract(
                        &contract,
                        ActionKind::Refactor,
                        insert_offset_option,
                    ));

                    // Adds ink! message.
                    add_result(entity::add_message_to_contract(
                        &contract,
                        ActionKind::Refactor,
                        insert_offset_option,
                    ));
                }
                None => {
                    let is_cfg_test = module.attrs().any(|attr| {
                        attr.path().map_or(false, |path| path.to_string() == "cfg")
                            && attr.token_tree().map_or(false, |token_tree| {
                                let mut meta = token_tree.syntax().to_string();
                                meta.retain(|it| !it.is_whitespace());
                                meta.contains("(test")
                                    || token_tree.syntax().to_string().contains(",test")
                            })
                    });
                    if is_cfg_test {
                        // Adds ink! test.
                        add_result(entity::add_ink_test(
                            module,
                            ActionKind::Refactor,
                            insert_offset_option,
                        ));

                        let is_cfg_e2e_tests = module.attrs().any(|attr| {
                            attr.path().map_or(false, |path| path.to_string() == "cfg")
                                && attr.token_tree().map_or(false, |token_tree| {
                                    let mut meta = token_tree.syntax().to_string();
                                    meta.retain(|it| !it.is_whitespace());
                                    meta.contains(r#"feature="e2e-tests""#)
                                })
                        });

                        if is_cfg_e2e_tests {
                            // Adds ink! e2e test.
                            add_result(entity::add_ink_e2e_test(
                                module,
                                ActionKind::Refactor,
                                insert_offset_option,
                            ));
                        }
                    }
                }
            }
        }
        ast::Item::Impl(impl_item) => {
            // Only computes ink! entities if impl item either:
            // - has an ink! `impl` attribute.
            // - contains at least one ink! constructor or ink! message.
            // - has an ink! contract as the direct parent.
            if InkImpl::can_cast(impl_item.syntax())
                || ink_analyzer_ir::ink_parent::<Contract>(impl_item.syntax()).is_some()
            {
                // Adds ink! constructor.
                add_result(entity::add_constructor_to_impl(
                    impl_item,
                    ActionKind::Refactor,
                    insert_offset_option,
                ));

                // Adds ink! message.
                add_result(entity::add_message_to_impl(
                    impl_item,
                    ActionKind::Refactor,
                    insert_offset_option,
                ));
            }
        }
        ast::Item::Trait(trait_item) => {
            if let Some((attr, _)) = utils::primary_ink_attribute_candidate(
                ink_analyzer_ir::ink_attrs(trait_item.syntax()),
            ) {
                if let InkAttributeKind::Macro(macro_kind) = attr.kind() {
                    match macro_kind {
                        InkMacroKind::ChainExtension => {
                            if let Some(chain_extension) = ChainExtension::cast(attr) {
                                // Add `ErrorCode` if it doesn't exist.
                                if chain_extension.error_code().is_none() {
                                    add_result(entity::add_error_code(
                                        &chain_extension,
                                        ActionKind::Refactor,
                                        insert_offset_option,
                                    ));
                                }

                                // Adds ink! extension.
                                add_result(entity::add_extension(
                                    &chain_extension,
                                    ActionKind::Refactor,
                                    insert_offset_option,
                                ));
                            }
                        }
                        InkMacroKind::TraitDefinition => {
                            if let Some(trait_definition) = TraitDefinition::cast(attr) {
                                // Adds ink! message declaration.
                                add_result(entity::add_message_to_trait_definition(
                                    &trait_definition,
                                    ActionKind::Refactor,
                                    insert_offset_option,
                                ));
                            }
                        }
                        // Ignores other macros.
                        _ => (),
                    }
                }
            }
        }
        // Ignores other items.
        _ => (),
    }
}

/// Computes root-level ink! entity macro actions.
fn root_ink_entity_actions(results: &mut Vec<Action>, file: &InkFile, offset: TextSize) {
    if file.contracts().is_empty() {
        // Adds ink! contract.
        results.push(entity::add_contract(offset, ActionKind::Refactor, None));
    }

    // Adds ink! trait definition.
    results.push(entity::add_trait_definition(
        offset,
        ActionKind::Refactor,
        None,
    ));

    // Adds ink! chain extension.
    results.push(entity::add_chain_extension(
        offset,
        ActionKind::Refactor,
        None,
    ));
}

/// Computes ink! attribute-based actions at the given text range.
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
                    // Adds ink! attribute argument action to accumulator.
                    let (edit, snippet) = utils::ink_arg_insertion_text(
                        arg_kind,
                        Some(insert_offset),
                        Some(ink_attr.syntax()),
                    );
                    results.push(Action {
                        label: format!("Add ink! {arg_kind} attribute argument."),
                        kind: ActionKind::Refactor,
                        range: ink_attr.syntax().text_range(),
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

/// Determines if the selection range is in an AST item's declaration
/// (i.e not on meta - attributes/rustdoc - nor inside the AST item's item list or body)
/// for an item that can be annotated with ink! attributes or can have ink! attribute descendants.
fn is_focused_on_ast_item_declaration(item: &ast::Item, range: TextRange) -> bool {
    // Returns false for "unsupported" item types (see [`utils::ast_item_declaration_range`] doc and implementation).
    utils::ast_item_declaration_range(item).map_or(false, |declaration_range| {
        declaration_range.contains_range(range)
    }) || utils::ast_item_terminal_token(item)
        .map_or(false, |token| token.text_range().contains_range(range))
}

/// Determines if the selection range is in an AST item's body (i.e inside the AST item's item list or body)
/// for an item that can be annotated with ink! attributes or can have ink! attribute descendants.
fn is_focused_on_ast_item_body(item: &ast::Item, range: TextRange) -> bool {
    // Returns false for "unsupported" item types (see [`utils::ast_item_declaration_range`] doc and implementation).
    utils::ast_item_declaration_range(item)
        .zip(
            utils::ast_item_terminal_token(item)
                .as_ref()
                .map(SyntaxToken::text_range),
        )
        .map_or(false, |(declaration_range, terminal_range)| {
            // Verifies that
            declaration_range.end() < terminal_range.start()
                && TextRange::new(declaration_range.end(), terminal_range.start())
                    .contains_range(range)
        })
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

            // No AST item in focus.
            (
                "",
                None,
                vec![
                    ("#[ink::contract]", None, None),
                    ("#[ink::trait_definition]", None, None),
                    ("#[ink::chain_extension]", None, None),
                ],
            ),
            (
                " ",
                None,
                vec![
                    ("#[ink::contract]", Some(" "), Some(" ")),
                    ("#[ink::trait_definition]", Some(" "), Some(" ")),
                    ("#[ink::chain_extension]", Some(" "), Some(" ")),
                ],
            ),
            (
                "\n\n",
                Some("\n"),
                vec![
                    ("#[ink::contract]", Some("\n"), Some("\n")),
                    ("#[ink::trait_definition]", Some("\n"), Some("\n")),
                    ("#[ink::chain_extension]", Some("\n"), Some("\n")),
                ],
            ),
            (
                "// A comment in focus.",
                None,
                vec![
                    (
                        "#[ink::contract]",
                        Some("// A comment in focus."),
                        Some("// A comment in focus."),
                    ),
                    (
                        "#[ink::trait_definition]",
                        Some("// A comment in focus."),
                        Some("// A comment in focus."),
                    ),
                    (
                        "#[ink::chain_extension]",
                        Some("// A comment in focus."),
                        Some("// A comment in focus."),
                    ),
                ],
            ),
            // Module focus.
            (
                r#"
                    mod my_module {

                    }
                "#,
                Some("<-\n                    }"),
                vec![],
            ),
            (
                r#"
                    mod my_module {
                        // The module declaration is out of focus when this comment is in focus.
                    }
                "#,
                Some("<-//"),
                vec![],
            ),
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
                    (
                        "(env=crate::)",
                        Some("#[ink::contract"),
                        Some("#[ink::contract"),
                    ),
                    (
                        r#"(keep_attr="")"#,
                        Some("#[ink::contract"),
                        Some("#[ink::contract"),
                    ),
                    (
                        "#[ink(storage)]",
                        Some("mod my_contract {"),
                        Some("mod my_contract {"),
                    ),
                    (
                        "#[ink(event)]",
                        Some("mod my_contract {"),
                        Some("mod my_contract {"),
                    ),
                    (
                        "#[ink(constructor)]",
                        Some("mod my_contract {"),
                        Some("mod my_contract {"),
                    ),
                    (
                        "#[ink(message)]",
                        Some("mod my_contract {"),
                        Some("mod my_contract {"),
                    ),
                ],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {

                    }
                "#,
                Some("<-\n                    }"),
                vec![
                    (
                        "#[ink(storage)]",
                        Some("<-\n                    }"),
                        Some("<-\n                    }"),
                    ),
                    (
                        "#[ink(event)]",
                        Some("<-\n                    }"),
                        Some("<-\n                    }"),
                    ),
                    (
                        "#[ink(constructor)]",
                        Some("<-\n                    }"),
                        Some("<-\n                    }"),
                    ),
                    (
                        "#[ink(message)]",
                        Some("<-\n                    }"),
                        Some("<-\n                    }"),
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
                vec![
                    (
                        "type ErrorCode",
                        Some("pub trait MyTrait {"),
                        Some("pub trait MyTrait {"),
                    ),
                    (
                        "#[ink(extension=1)]",
                        Some("pub trait MyTrait {"),
                        Some("pub trait MyTrait {"),
                    ),
                ],
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
                        r#"(keep_attr="")"#,
                        Some("#[ink::trait_definition"),
                        Some("#[ink::trait_definition"),
                    ),
                    (
                        r#"(namespace="my_namespace")"#,
                        Some("#[ink::trait_definition"),
                        Some("#[ink::trait_definition"),
                    ),
                    (
                        "#[ink(message)]",
                        Some("pub trait MyTrait {"),
                        Some("pub trait MyTrait {"),
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
                    ("#[ink(extension=1)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(handle_status=true)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(message)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(payable)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(selector=1)]", Some("<-fn"), Some("<-fn")),
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
                        r#"(additional_contracts="")"#,
                        Some("#[ink_e2e::test"),
                        Some("#[ink_e2e::test"),
                    ),
                    (
                        "(environment=crate::)",
                        Some("#[ink_e2e::test"),
                        Some("#[ink_e2e::test"),
                    ),
                    (
                        r#"(keep_attr="")"#,
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
                        ", selector=1",
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
                results.len(),
                expected_results.len(),
                "code: {code} | {:#?}",
                pat
            );
            for (idx, action) in results.into_iter().enumerate() {
                let edit_text = remove_whitespace(action.edits[0].text.clone());
                let (expected_edit_text, pat_start, pat_end) = expected_results[idx];
                let expected_edit_text = remove_whitespace(expected_edit_text.to_string());
                assert!(
                    edit_text == expected_edit_text
                        || (!expected_edit_text.is_empty()
                            && edit_text.contains(expected_edit_text.as_str())),
                    "code: {code} | {:#?}",
                    pat
                );
                assert_eq!(
                    action.edits[0].range,
                    TextRange::new(
                        TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                        TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                    ),
                    "code: {code} | {:#?}",
                    pat
                );
            }
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
                    ("(env=crate::)", Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
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
                    ("(env=crate::)", Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
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
                    ("(env=crate::)", Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![(r#", keep_attr="""#, Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types,)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![(r#"keep_attr="""#, Some("<-)]"), Some("<-)]"))],
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
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
                    (r#"(namespace="my_namespace")"#, Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::trait_definition(namespace="my_namespace")]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![(r#", keep_attr="""#, Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    enum MyEnum {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=true)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=true)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    union MyUnion {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=true)", Some("<-]"), Some("<-]"))],
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
                    (r#"(additional_contracts="")"#, Some("<-]"), Some("<-]")),
                    (r#"(environment=crate::)"#, Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
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
                    (", selector=1", Some("<-)]->"), Some("<-)]->")),
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(extension=1)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![(", handle_status=true", Some("<-)]"), Some("<-)]"))],
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
    fn is_focused_on_ast_item_declaration_and_body_works() {
        for (code, test_cases) in [
            // (code, [(pat, declaration_result, body_result)]) where:
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
                    (Some("<-#[a"), false, false),
                    (Some("#[ab"), false, false),
                    (Some("abc]"), false, false),
                    (Some("<-#[ink"), false, false),
                    (Some("#[in"), false, false),
                    (Some("ink::"), false, false),
                    (Some("::con"), false, false),
                    (Some("contract]"), false, false),
                    (Some("<-mod"), true, false),
                    (Some("mo"), true, false),
                    (Some("mod"), true, false),
                    (Some("<-my_module"), true, false),
                    (Some("my_"), true, false),
                    (Some("<-my_module"), true, false),
                    (Some("<-{"), true, false),
                    (Some("{"), true, true),
                    (Some("<-//"), false, true),
                    (Some("<-\n                    }"), false, true),
                    (Some("<-}"), true, true),
                    (Some("}"), true, false),
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
                    (Some("<-#[a"), false, false),
                    (Some("#[ab"), false, false),
                    (Some("abc]"), false, false),
                    (Some("<-#[ink"), false, false),
                    (Some("#[in"), false, false),
                    (Some("ink::"), false, false),
                    (Some("::trait"), false, false),
                    (Some("definition]"), false, false),
                    (Some("<-pub"), true, false),
                    (Some("pu"), true, false),
                    (Some("pub"), true, false),
                    (Some("<-trait MyTrait"), true, false),
                    (Some("pub tr"), true, false),
                    (Some("pub trait"), true, false),
                    (Some("<-MyTrait"), true, false),
                    (Some("My"), true, false),
                    (Some("<-MyTrait"), true, false),
                    (Some("<-{"), true, false),
                    (Some("{"), true, true),
                    (Some("<-//"), false, true),
                    (Some("<-}"), true, true),
                    (Some("}"), true, false),
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
                    (Some("<-#[a"), false, false),
                    (Some("#[ab"), false, false),
                    (Some("abc]"), false, false),
                    (Some("<-#[ink"), false, false),
                    (Some("#[in"), false, false),
                    (Some("ink::"), false, false),
                    (Some("::storage"), false, false),
                    (Some("storage_item]"), false, false),
                    (Some("<-pub"), true, false),
                    (Some("pu"), true, false),
                    (Some("pub"), true, false),
                    (Some("<-enum"), true, false),
                    (Some("en"), true, false),
                    (Some("enum"), true, false),
                    (Some("<-MyEnum"), true, false),
                    (Some("My"), true, false),
                    (Some("<-MyEnum"), true, false),
                    (Some("<-{"), true, false),
                    (Some("{"), true, true),
                    (Some("<-//"), false, true),
                    (Some("<-}"), true, true),
                    (Some("}"), true, false),
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
                    (Some("<-#[a"), false, false),
                    (Some("#[ab"), false, false),
                    (Some("abc]"), false, false),
                    (Some("<-#[ink"), false, false),
                    (Some("#[in"), false, false),
                    (Some("ink("), false, false),
                    (Some("(eve"), false, false),
                    (Some("(event,"), false, false),
                    (Some(", anon"), false, false),
                    (Some("anonymous)]"), false, false),
                    (Some("<-pub"), true, false),
                    (Some("pu"), true, false),
                    (Some("pub"), true, false),
                    (Some("<-struct"), true, false),
                    (Some("st"), true, false),
                    (Some("struct"), true, false),
                    (Some("<-MyStruct"), true, false),
                    (Some("My"), true, false),
                    (Some("<-MyStruct"), true, false),
                    (Some("<-{"), true, false),
                    (Some("{"), true, true),
                    (Some("<-//"), false, true),
                    (Some("<-}"), true, true),
                    (Some("}"), true, false),
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
                    (Some("<-#[a"), false, false),
                    (Some("#[ab"), false, false),
                    (Some("abc]"), false, false),
                    (Some("<-#[ink"), false, false),
                    (Some("#[in"), false, false),
                    (Some("ink::"), false, false),
                    (Some("::storage"), false, false),
                    (Some("storage_item]"), false, false),
                    (Some("<-pub"), true, false),
                    (Some("pu"), true, false),
                    (Some("pub"), true, false),
                    (Some("<-union"), true, false),
                    (Some("un"), true, false),
                    (Some("union"), true, false),
                    (Some("<-MyUnion"), true, false),
                    (Some("My"), true, false),
                    (Some("<-MyUnion"), true, false),
                    (Some("<-{"), true, false),
                    (Some("{"), true, true),
                    (Some("<-//"), false, true),
                    (Some("<-}"), true, true),
                    (Some("}"), true, false),
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
                    (Some("<-#[a"), false, false),
                    (Some("#[ab"), false, false),
                    (Some("abc]"), false, false),
                    (Some("<-#[ink"), false, false),
                    (Some("#[in"), false, false),
                    (Some("ink("), false, false),
                    (Some("(con"), false, false),
                    (Some("(constructor,"), false, false),
                    (Some(", select"), false, false),
                    (Some("selector=1)]"), false, false),
                    (Some("(pay"), false, false),
                    (Some("payable)]"), false, false),
                    (Some("<-pub"), true, false),
                    (Some("pu"), true, false),
                    (Some("pub"), true, false),
                    (Some("<-fn"), true, false),
                    (Some("f"), true, false),
                    (Some("fn"), true, false),
                    (Some("<-my_fn"), true, false),
                    (Some("my_"), true, false),
                    (Some("<-my_fn"), true, false),
                    (Some("<-{"), true, false),
                    (Some("{"), true, true),
                    (Some("<-//"), false, true),
                    (Some("<-}"), true, true),
                    (Some("}"), true, false),
                ],
            ),
        ] {
            for (pat, expected_declaration_result, expected_body_result) in test_cases {
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
                    expected_declaration_result,
                    "code: {code} | {:#?}",
                    pat
                );
                assert_eq!(
                    is_focused_on_ast_item_body(&ast_item, range),
                    expected_body_result,
                    "code: {code} | {:#?}",
                    pat
                );
            }
        }
    }
}
