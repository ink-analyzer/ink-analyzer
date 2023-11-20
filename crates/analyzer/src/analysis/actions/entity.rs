//! ink! entity code/intent actions.

use ink_analyzer_ir::syntax::{AstNode, TextRange, TextSize};
use ink_analyzer_ir::{
    ast, ChainExtension, Contract, Event, InkEntity, IsInkStruct, IsInkTrait, TraitDefinition,
};

use super::{Action, ActionKind};
use crate::analysis::utils;
use crate::codegen::snippets::{
    CHAIN_EXTENSION_PLAIN, CHAIN_EXTENSION_SNIPPET, CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET,
    CONTRACT_PLAIN, CONTRACT_SNIPPET, ERROR_CODE_PLAIN, ERROR_CODE_SNIPPET, EVENT_PLAIN,
    EVENT_SNIPPET, EXTENSION_PLAIN, EXTENSION_SNIPPET, INK_E2E_TEST_PLAIN, INK_E2E_TEST_SNIPPET,
    INK_TEST_PLAIN, INK_TEST_SNIPPET, MESSAGE_PLAIN, MESSAGE_SNIPPET, STORAGE_ITEM_PLAIN,
    STORAGE_ITEM_SNIPPET, STORAGE_PLAIN, STORAGE_SNIPPET, TOPIC_PLAIN, TOPIC_SNIPPET,
    TRAIT_DEFINITION_PLAIN, TRAIT_DEFINITION_SNIPPET, TRAIT_MESSAGE_PLAIN, TRAIT_MESSAGE_SNIPPET,
};
use crate::TextEdit;

/// Adds an ink! storage `struct` to an ink! contract `mod` item.
pub fn add_storage(
    contract: &Contract,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    contract.module().and_then(|module| {
        // Sets insert offset or defaults to inserting at the beginning of the associated items list (if possible).
        insert_offset_option
            .or(module
                .item_list()
                .as_ref()
                .map(utils::item_insert_offset_start))
            .map(|insert_offset| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(module.syntax());
                // Gets the "resolved" contract name.
                let contract_name = utils::resolve_contract_name(contract);

                Action {
                    label: "Add ink! storage `struct`.".to_string(),
                    kind,
                    range: utils::contract_declaration_range(contract),
                    edits: vec![TextEdit::insert_with_snippet(
                        utils::apply_indenting(
                            contract_name
                                .as_deref()
                                .map(|name| STORAGE_PLAIN.replace("Storage", name))
                                .as_deref()
                                .unwrap_or(STORAGE_PLAIN),
                            &indent,
                        ),
                        insert_offset,
                        Some(utils::apply_indenting(
                            contract_name
                                .as_deref()
                                .map(|name| STORAGE_SNIPPET.replace("Storage", name))
                                .as_deref()
                                .unwrap_or(STORAGE_SNIPPET),
                            &indent,
                        )),
                    )],
                }
            })
    })
}

/// Adds an ink! event `struct` to an ink! contract `mod` item.
pub fn add_event(
    contract: &Contract,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    contract.module().and_then(|module| {
        // Sets insert offset or defaults to inserting after either the last struct or the beginning of the associated items list (if possible).
        insert_offset_option
            .or(module
                .item_list()
                .as_ref()
                .map(utils::item_insert_offset_after_last_struct_or_start))
            .map(|insert_offset| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(module.syntax());
                // Suggests an event name based on the "resolved" contract name.
                let suggested_event_name =
                    utils::resolve_contract_name(contract).map(|name| format!("My{name}Event"));

                Action {
                    label: "Add ink! event `struct`.".to_string(),
                    kind,
                    range: utils::contract_declaration_range(contract),
                    edits: vec![TextEdit::insert_with_snippet(
                        utils::apply_indenting(
                            suggested_event_name
                                .as_deref()
                                .map(|name| EVENT_PLAIN.replace("Event", name))
                                .as_deref()
                                .unwrap_or(EVENT_PLAIN),
                            &indent,
                        ),
                        insert_offset,
                        Some(utils::apply_indenting(
                            suggested_event_name
                                .as_deref()
                                .map(|name| EVENT_SNIPPET.replace("Event", name))
                                .as_deref()
                                .unwrap_or(EVENT_SNIPPET),
                            &indent,
                        )),
                    )],
                }
            })
    })
}

/// Adds an ink! topic to an ink! event `struct` item.
pub fn add_topic(
    event: &Event,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    event.struct_item().and_then(|struct_item| {
        // Sets insert offset or defaults to inserting at the end of the field list (if possible).
        insert_offset_option
            .map(|offset| (offset, None, None))
            .or(struct_item
                .field_list()
                .as_ref()
                .map(utils::field_insert_offset_end_and_affixes))
            .map(|(insert_offset, prefix, suffix)| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(struct_item.syntax());

                Action {
                    label: "Add ink! topic `field`.".to_string(),
                    kind,
                    range: utils::ast_item_declaration_range(&ast::Item::Struct(
                        struct_item.clone(),
                    ))
                    .unwrap_or(struct_item.syntax().text_range()),
                    edits: vec![TextEdit::insert_with_snippet(
                        format!(
                            "{}{}{}",
                            prefix.as_deref().unwrap_or_default(),
                            utils::apply_indenting(TOPIC_PLAIN, &indent),
                            suffix.as_deref().unwrap_or_default()
                        ),
                        insert_offset,
                        Some(format!(
                            "{}{}{}",
                            prefix.as_deref().unwrap_or_default(),
                            utils::apply_indenting(TOPIC_SNIPPET, &indent),
                            suffix.as_deref().unwrap_or_default()
                        )),
                    )],
                }
            })
    })
}

/// Adds an ink! callable `fn` to the first non-trait `impl` block or creates a new `impl` block if necessary.
fn add_callable_to_contract(
    contract: &Contract,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
    label: String,
    plain: &str,
    snippet: &str,
) -> Option<Action> {
    insert_offset_option
        .and_then(|offset| utils::parent_ast_item(contract, TextRange::new(offset, offset)))
        // Finds the parent `impl` block (if any).
        .and_then(|it| match it {
            ast::Item::Impl(impl_item) => Some(impl_item),
            _ => None,
        })
        // Validates that the `impl` block is inside the contract.
        .filter(|impl_item| {
            contract
                .syntax()
                .text_range()
                .contains_range(impl_item.syntax().text_range())
        })
        // Inserts in the parent `impl` (if any).
        .and_then(|impl_item| {
            add_callable_to_impl(
                &impl_item,
                kind,
                insert_offset_option,
                label.clone(),
                plain,
                snippet,
            )
        })
        // Otherwise inserts in contract root and creates `impl` block as needed.
        .or(insert_offset_option
            .zip(utils::callable_impl_indent_and_affixes(contract))
            .map(|(insert_offset, (indent, prefix, suffix))| {
                (insert_offset, indent, Some(prefix), Some(suffix))
            })
            // Defaults to inserting in the first non-trait `impl` block or creating a new `impl` block if necessary
            .or(utils::callable_insert_offset_indent_and_affixes(contract))
            .map(|(insert_offset, indent, prefix, suffix)| Action {
                label,
                kind,
                range: utils::contract_declaration_range(contract),
                edits: vec![TextEdit::insert_with_snippet(
                    format!(
                        "{}{}{}",
                        prefix.as_deref().unwrap_or_default(),
                        utils::apply_indenting(plain, &indent),
                        suffix.as_deref().unwrap_or_default()
                    ),
                    insert_offset,
                    Some(format!(
                        "{}{}{}",
                        prefix.as_deref().unwrap_or_default(),
                        utils::apply_indenting(snippet, &indent),
                        suffix.as_deref().unwrap_or_default()
                    )),
                )],
            }))
}

/// Adds an ink! constructor `fn` to the first non-trait `impl` block or creates a new `impl` block if necessary.
pub fn add_constructor_to_contract(
    contract: &Contract,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    add_callable_to_contract(
        contract,
        kind,
        insert_offset_option,
        "Add ink! constructor `fn`.".to_string(),
        CONSTRUCTOR_PLAIN,
        CONSTRUCTOR_SNIPPET,
    )
}

/// Adds an ink! message `fn` to the first non-trait `impl` block or creates a new `impl` block if necessary.
pub fn add_message_to_contract(
    contract: &Contract,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    add_callable_to_contract(
        contract,
        kind,
        insert_offset_option,
        "Add ink! message `fn`.".to_string(),
        MESSAGE_PLAIN,
        MESSAGE_SNIPPET,
    )
}

/// Adds an ink! callable `fn` to an `impl` block.
fn add_callable_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
    label: String,
    plain: &str,
    snippet: &str,
) -> Option<Action> {
    // Sets insert offset or defaults to inserting at the end of the associated items list (if possible).
    insert_offset_option
        .or(impl_item
            .assoc_item_list()
            .as_ref()
            .map(utils::assoc_item_insert_offset_end))
        .map(|insert_offset| {
            // Sets insert indent.
            let indent = utils::item_children_indenting(impl_item.syntax());

            Action {
                label,
                kind,
                range: utils::ast_item_declaration_range(&ast::Item::Impl(impl_item.clone()))
                    .unwrap_or(impl_item.syntax().text_range()),
                edits: vec![TextEdit::insert_with_snippet(
                    utils::apply_indenting(plain, &indent),
                    insert_offset,
                    Some(utils::apply_indenting(snippet, &indent)),
                )],
            }
        })
}

/// Adds an ink! constructor `fn` to an `impl` block.
pub fn add_constructor_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    add_callable_to_impl(
        impl_item,
        kind,
        insert_offset_option,
        "Add ink! constructor `fn`.".to_string(),
        CONSTRUCTOR_PLAIN,
        CONSTRUCTOR_SNIPPET,
    )
}

/// Adds an ink! message `fn` to an `impl` block.
pub fn add_message_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    add_callable_to_impl(
        impl_item,
        kind,
        insert_offset_option,
        "Add ink! message `fn`.".to_string(),
        MESSAGE_PLAIN,
        MESSAGE_SNIPPET,
    )
}

/// Adds an ink! message `fn` declaration to an ink! trait definition `trait` item.
pub fn add_message_to_trait_definition(
    trait_definition: &TraitDefinition,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    trait_definition.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the end of the associated items list (if possible).
        insert_offset_option
            .or(trait_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_end))
            .map(|insert_offset| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(trait_item.syntax());

                Action {
                    label: "Add ink! message `fn`.".to_string(),
                    kind,
                    range: utils::ink_trait_declaration_range(trait_definition),
                    edits: vec![TextEdit::insert_with_snippet(
                        utils::apply_indenting(TRAIT_MESSAGE_PLAIN, &indent),
                        insert_offset,
                        Some(utils::apply_indenting(TRAIT_MESSAGE_SNIPPET, &indent)),
                    )],
                }
            })
    })
}

/// Adds an `ErrorCode` type to an ink! chain extension `trait` item.
pub fn add_error_code(
    chain_extension: &ChainExtension,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    chain_extension.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the beginning of the associated items list (if possible).
        insert_offset_option
            .or(trait_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_start))
            .map(|insert_offset| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(trait_item.syntax());

                Action {
                    label: "Add `ErrorCode` type for ink! chain extension.".to_string(),
                    kind,
                    range: utils::ink_trait_declaration_range(chain_extension),
                    edits: vec![TextEdit::insert_with_snippet(
                        utils::apply_indenting(ERROR_CODE_PLAIN, &indent),
                        insert_offset,
                        Some(utils::apply_indenting(ERROR_CODE_SNIPPET, &indent)),
                    )],
                }
            })
    })
}

/// Adds an extension `fn` declaration to an ink! chain extension `trait` item.
pub fn add_extension(
    chain_extension: &ChainExtension,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    chain_extension.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the end of the associated items list (if possible).
        insert_offset_option
            .or(trait_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_end))
            .map(|insert_offset| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(trait_item.syntax());

                Action {
                    label: "Add ink! extension `fn`.".to_string(),
                    kind,
                    range: utils::ink_trait_declaration_range(chain_extension),
                    edits: vec![TextEdit::insert_with_snippet(
                        utils::apply_indenting(EXTENSION_PLAIN, &indent),
                        insert_offset,
                        Some(utils::apply_indenting(EXTENSION_SNIPPET, &indent)),
                    )],
                }
            })
    })
}

/// Adds an ink! test `fn` to a `mod` item.
pub fn add_ink_test(
    module: &ast::Module,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    // Sets insert offset or defaults to inserting at the end of the associated items list (if possible).
    insert_offset_option
        .or(module
            .item_list()
            .as_ref()
            .map(utils::item_insert_offset_end))
        .map(|insert_offset| {
            // Sets insert indent.
            let indent = utils::item_children_indenting(module.syntax());

            Action {
                label: "Add ink! test `fn`.".to_string(),
                kind,
                range: utils::ast_item_declaration_range(&ast::Item::Module(module.clone()))
                    .unwrap_or(module.syntax().text_range()),
                edits: vec![TextEdit::insert_with_snippet(
                    utils::apply_indenting(INK_TEST_PLAIN, &indent),
                    insert_offset,
                    Some(utils::apply_indenting(INK_TEST_SNIPPET, &indent)),
                )],
            }
        })
}

/// Adds an ink! e2e test `fn` to a `mod` item.
pub fn add_ink_e2e_test(
    module: &ast::Module,
    kind: ActionKind,
    insert_offset_option: Option<TextSize>,
) -> Option<Action> {
    // Sets insert offset or defaults to inserting at the end of the associated items list (if possible).
    insert_offset_option
        .or(module
            .item_list()
            .as_ref()
            .map(utils::item_insert_offset_end))
        .map(|insert_offset| {
            // Sets insert indent.
            let indent = utils::item_children_indenting(module.syntax());

            Action {
                label: "Add ink! e2e test `fn`.".to_string(),
                kind,
                range: utils::ast_item_declaration_range(&ast::Item::Module(module.clone()))
                    .unwrap_or(module.syntax().text_range()),
                edits: vec![TextEdit::insert_with_snippet(
                    utils::apply_indenting(INK_E2E_TEST_PLAIN, &indent),
                    insert_offset,
                    Some(utils::apply_indenting(INK_E2E_TEST_SNIPPET, &indent)),
                )],
            }
        })
}

/// Creates an insert edit with a snippet and indenting.
fn insert_edit_with_snippet_and_indent(
    text: &str,
    offset: TextSize,
    snippet_option: Option<&str>,
    indent_option: Option<&str>,
) -> TextEdit {
    TextEdit::insert_with_snippet(
        match indent_option {
            Some(indent) => utils::apply_indenting(text, indent),
            None => text.to_string(),
        },
        offset,
        snippet_option.map(|snippet| match indent_option {
            Some(indent) => utils::apply_indenting(snippet, indent),
            None => snippet.to_string(),
        }),
    )
}

/// Add an ink! contract `mod`.
pub fn add_contract(offset: TextSize, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add ink! contract `mod`.".to_string(),
        kind,
        range: TextRange::new(offset, offset),
        edits: vec![insert_edit_with_snippet_and_indent(
            CONTRACT_PLAIN,
            offset,
            Some(CONTRACT_SNIPPET),
            indent_option,
        )],
    }
}

/// Add an ink! trait definition `trait`.
pub fn add_trait_definition(
    offset: TextSize,
    kind: ActionKind,
    indent_option: Option<&str>,
) -> Action {
    Action {
        label: "Add ink! trait definition.".to_string(),
        kind,
        range: TextRange::new(offset, offset),
        edits: vec![insert_edit_with_snippet_and_indent(
            TRAIT_DEFINITION_PLAIN,
            offset,
            Some(TRAIT_DEFINITION_SNIPPET),
            indent_option,
        )],
    }
}

/// Add an ink! chain extension `trait`.
pub fn add_chain_extension(
    offset: TextSize,
    kind: ActionKind,
    indent_option: Option<&str>,
) -> Action {
    Action {
        label: "Add ink! chain extension `trait`.".to_string(),
        kind,
        range: TextRange::new(offset, offset),
        edits: vec![insert_edit_with_snippet_and_indent(
            CHAIN_EXTENSION_PLAIN,
            offset,
            Some(CHAIN_EXTENSION_SNIPPET),
            indent_option,
        )],
    }
}

/// Add an ink! storage item.
pub fn add_storage_item(offset: TextSize, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add ink! storage item `ADT` (i.e. `struct`, `enum` or `union`).".to_string(),
        kind,
        range: TextRange::new(offset, offset),
        edits: vec![insert_edit_with_snippet_and_indent(
            STORAGE_ITEM_PLAIN,
            offset,
            Some(STORAGE_ITEM_SNIPPET),
            indent_option,
        )],
    }
}
