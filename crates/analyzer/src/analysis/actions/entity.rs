//! ink! entity code/intent actions.

use std::collections::HashSet;

use ink_analyzer_ir::{
    ast::{self, HasModuleItem, HasName},
    syntax::{AstNode, TextRange},
    ChainExtension, Constructor, Contract, Extension, InkEntity, IsInkEvent, IsInkFn, IsInkTrait,
    Message, TraitDefinition,
};

use super::{Action, ActionKind};
use crate::analysis::utils;
use crate::codegen::snippets::{
    CHAIN_EXTENSION_PLAIN, CHAIN_EXTENSION_SNIPPET, CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET,
    CONTRACT_PLAIN, CONTRACT_SNIPPET, ENVIRONMENT_DEF, ENVIRONMENT_IMPL_PLAIN,
    ENVIRONMENT_IMPL_SNIPPET, ERROR_CODE_PLAIN, ERROR_CODE_SNIPPET, EVENT_PLAIN, EVENT_SNIPPET,
    EVENT_V2_PLAIN, EVENT_V2_SNIPPET, EXTENSION_PLAIN, EXTENSION_SNIPPET, INK_E2E_TEST_PLAIN,
    INK_E2E_TEST_SNIPPET, INK_TEST_PLAIN, INK_TEST_SNIPPET, MESSAGE_PLAIN, MESSAGE_SNIPPET,
    STORAGE_ITEM_PLAIN, STORAGE_ITEM_SNIPPET, STORAGE_PLAIN, STORAGE_SNIPPET, TOPIC_PLAIN,
    TOPIC_SNIPPET, TRAIT_DEFINITION_PLAIN, TRAIT_DEFINITION_SNIPPET, TRAIT_MESSAGE_PLAIN,
    TRAIT_MESSAGE_SNIPPET,
};
use crate::TextEdit;

/// Adds an ink! storage `struct` to an ink! contract `mod` item.
pub fn add_storage(
    contract: &Contract,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    contract.module().and_then(|module| {
        // Sets insert offset or defaults to inserting at the beginning of the
        // associated items list (if possible).
        range_option
            .or(module
                .item_list()
                .as_ref()
                .map(utils::item_insert_offset_start)
                .map(|offset| TextRange::new(offset, offset)))
            .map(|range| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(module.syntax());
                // Gets the "resolved" contract name.
                let contract_name = utils::resolve_contract_name(contract);

                Action {
                    label: "Add ink! storage `struct`.".to_owned(),
                    kind,
                    range: utils::contract_declaration_range(contract),
                    edits: vec![TextEdit::replace_with_snippet(
                        utils::apply_indenting(
                            contract_name
                                .as_deref()
                                .map(|name| STORAGE_PLAIN.replace("Storage", name))
                                .as_deref()
                                .unwrap_or(STORAGE_PLAIN),
                            &indent,
                        ),
                        range,
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

macro_rules! mod_item_names {
    ($mod: ident, $ty: ident) => {
        $mod.item_list()
            .into_iter()
            .flat_map(|item_list| {
                item_list.items().filter_map(|item| match item {
                    ast::Item::$ty(item) => item.name().as_ref().map(ToString::to_string),
                    _ => None,
                })
            })
            .collect()
    };
}

fn text_and_snippet(
    text: &str,
    snippet: &str,
    preferred_name: &str,
    unavailable_names: &HashSet<String>,
) -> (String, String) {
    let suggested_name = utils::suggest_unique_name(preferred_name, unavailable_names);
    (
        text.replace(preferred_name, &suggested_name),
        snippet.replace(preferred_name, &suggested_name),
    )
}

/// Adds an ink! event `struct` to an ink! contract `mod` item.
pub fn add_event(
    contract: &Contract,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    contract.module().and_then(|module| {
        // Sets insert offset or defaults to inserting after either the last struct or
        // the beginning of the associated items list (if possible).
        range_option
            .or(module
                .item_list()
                .as_ref()
                .map(utils::item_insert_offset_after_last_struct_or_start)
                .map(|offset| TextRange::new(offset, offset)))
            .map(|range| {
                // Sets insert indent and suggested name.
                let indent = utils::item_children_indenting(module.syntax());
                let names = mod_item_names!(module, Struct);
                let (text, snippet) = text_and_snippet(EVENT_PLAIN, EVENT_SNIPPET, "Event", &names);

                Action {
                    label: "Add ink! event `struct`.".to_owned(),
                    kind,
                    range: utils::contract_declaration_range(contract),
                    edits: vec![TextEdit::replace_with_snippet(
                        utils::apply_indenting(&text, &indent),
                        range,
                        Some(utils::apply_indenting(&snippet, &indent)),
                    )],
                }
            })
    })
}

/// Adds an ink! event 2.0 `struct`.
pub fn add_event_v2(range: TextRange, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add ink! event 2.0 `struct`.".to_owned(),
        kind,
        range,
        edits: vec![compose_edit_with_snippet_and_indent(
            EVENT_V2_PLAIN,
            range,
            Some(EVENT_V2_SNIPPET),
            indent_option,
        )],
    }
}

/// Adds an ink! topic to an ink! event `struct` item.
pub fn add_topic<T>(event: &T, kind: ActionKind, range_option: Option<TextRange>) -> Option<Action>
where
    T: IsInkEvent,
{
    event.struct_item().and_then(|struct_item| {
        // Sets insert offset or defaults to inserting at the end of the field list (if possible).
        range_option
            .map(|offset| (offset, None, None))
            .or(struct_item
                .field_list()
                .as_ref()
                .map(utils::field_insert_offset_end_and_affixes)
                .map(|(offset, prefix, suffix)| (TextRange::new(offset, offset), prefix, suffix)))
            .map(|(range, prefix, suffix)| {
                // Sets insert indent and suggested name.
                let indent = utils::item_children_indenting(struct_item.syntax());
                let names: HashSet<_> = struct_item
                    .field_list()
                    .and_then(|field_list| match field_list {
                        ast::FieldList::RecordFieldList(record_list) => Some(record_list),
                        _ => None,
                    })
                    .into_iter()
                    .flat_map(|field_list| {
                        field_list
                            .fields()
                            .filter_map(|field| field.name().as_ref().map(ToString::to_string))
                    })
                    .collect();
                let (text, snippet) =
                    text_and_snippet(TOPIC_PLAIN, TOPIC_SNIPPET, "my_topic", &names);

                Action {
                    label: "Add ink! topic `field`.".to_owned(),
                    kind,
                    range: utils::ast_item_declaration_range(&ast::Item::Struct(
                        struct_item.clone(),
                    ))
                    .unwrap_or(struct_item.syntax().text_range()),
                    edits: vec![TextEdit::replace_with_snippet(
                        format!(
                            "{}{}{}",
                            prefix.as_deref().unwrap_or_default(),
                            utils::apply_indenting(&text, &indent),
                            suffix.as_deref().unwrap_or_default()
                        ),
                        range,
                        Some(format!(
                            "{}{}{}",
                            prefix.as_deref().unwrap_or_default(),
                            utils::apply_indenting(&snippet, &indent),
                            suffix.as_deref().unwrap_or_default()
                        )),
                    )],
                }
            })
    })
}

/// Adds an ink! callable `fn` to the first non-trait `impl` block or
/// creates a new `impl` block if necessary.
fn add_callable_to_contract(
    contract: &Contract,
    kind: ActionKind,
    range_option: Option<TextRange>,
    label: String,
    plain: &str,
    snippet: &str,
) -> Option<Action> {
    range_option
        .and_then(|range| utils::parent_ast_item(contract, range))
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
                range_option,
                label.clone(),
                plain,
                snippet,
            )
        })
        // Otherwise inserts in contract root and creates `impl` block as needed.
        .or(range_option
            .zip(utils::callable_impl_indent_and_affixes(contract))
            .map(|(insert_offset, (indent, prefix, suffix))| {
                (insert_offset, indent, Some(prefix), Some(suffix))
            })
            // Defaults to inserting in the first non-trait `impl` block or
            // creating a new `impl` block if necessary
            .or(
                utils::callable_insert_offset_indent_and_affixes(contract).map(
                    |(offset, ident, prefix, suffix)| {
                        (TextRange::new(offset, offset), ident, prefix, suffix)
                    },
                ),
            )
            .map(|(range, indent, prefix, suffix)| Action {
                label,
                kind,
                range: utils::contract_declaration_range(contract),
                edits: vec![TextEdit::replace_with_snippet(
                    format!(
                        "{}{}{}",
                        prefix.as_deref().unwrap_or_default(),
                        utils::apply_indenting(plain, &indent),
                        suffix.as_deref().unwrap_or_default()
                    ),
                    range,
                    Some(format!(
                        "{}{}{}",
                        prefix.as_deref().unwrap_or_default(),
                        utils::apply_indenting(snippet, &indent),
                        suffix.as_deref().unwrap_or_default()
                    )),
                )],
            }))
}

fn fn_names<'a>(fns: impl Iterator<Item = &'a ast::Fn>) -> HashSet<String> {
    fns.filter_map(|fn_item| fn_item.name().as_ref().map(ToString::to_string))
        .collect()
}

fn contract_fn_names(contract: &Contract) -> HashSet<String> {
    let fns = contract
        .constructors()
        .iter()
        .filter_map(Constructor::fn_item)
        .chain(contract.messages().iter().filter_map(Message::fn_item));
    fn_names(fns)
}

/// Adds an ink! constructor `fn` to the first non-trait `impl` block or
/// creates a new `impl` block if necessary.
pub fn add_constructor_to_contract(
    contract: &Contract,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    let names = contract_fn_names(contract);
    let (text, snippet) = text_and_snippet(CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, "new", &names);
    add_callable_to_contract(
        contract,
        kind,
        range_option,
        "Add ink! constructor `fn`.".to_owned(),
        &text,
        &snippet,
    )
}

/// Adds an ink! message `fn` to the first non-trait `impl` block or
/// creates a new `impl` block if necessary.
pub fn add_message_to_contract(
    contract: &Contract,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    let names = contract_fn_names(contract);
    let (text, snippet) = text_and_snippet(MESSAGE_PLAIN, MESSAGE_SNIPPET, "my_message", &names);
    add_callable_to_contract(
        contract,
        kind,
        range_option,
        "Add ink! message `fn`.".to_owned(),
        &text,
        &snippet,
    )
}

/// Adds an ink! message `fn` with the specified selector to the first non-trait `impl` block or
/// creates a new `impl` block if necessary.
pub fn add_message_selector_to_contract(
    contract: &Contract,
    kind: ActionKind,
    selector: &str,
    fn_name: &str,
    range_option: Option<TextRange>,
    label_option: Option<String>,
) -> Option<Action> {
    let names = contract_fn_names(contract);
    let (mut text, mut snippet) = text_and_snippet(MESSAGE_PLAIN, MESSAGE_SNIPPET, fn_name, &names);
    let selector_attr = format!("#[ink(message, selector = {selector})]");
    text = text.replace("#[ink(message)]", &selector_attr);
    snippet = snippet.replace("#[ink(message)]", &selector_attr);
    add_callable_to_contract(
        contract,
        kind,
        range_option,
        label_option
            .unwrap_or_else(|| format!("Add ink! message `fn` with `selector = {selector}`.")),
        &text,
        &snippet,
    )
}

/// Adds an ink! callable `fn` to an `impl` block.
fn add_callable_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    range_option: Option<TextRange>,
    label: String,
    plain: &str,
    snippet: &str,
) -> Option<Action> {
    // Sets insert offset or defaults to inserting at the end of the
    // associated items list (if possible).
    range_option
        .or(impl_item
            .assoc_item_list()
            .as_ref()
            .map(utils::assoc_item_insert_offset_end)
            .map(|offset| TextRange::new(offset, offset)))
        .map(|range| {
            // Sets insert indent.
            let indent = utils::item_children_indenting(impl_item.syntax());

            Action {
                label,
                kind,
                range: utils::ast_item_declaration_range(&ast::Item::Impl(impl_item.clone()))
                    .unwrap_or(impl_item.syntax().text_range()),
                edits: vec![TextEdit::replace_with_snippet(
                    utils::apply_indenting(plain, &indent),
                    range,
                    Some(utils::apply_indenting(snippet, &indent)),
                )],
            }
        })
}

fn impl_fn_names(impl_item: &ast::Impl) -> HashSet<String> {
    impl_item
        .assoc_item_list()
        .into_iter()
        .flat_map(|assoc_item_list| {
            assoc_item_list
                .assoc_items()
                .filter_map(|assoc_item| match assoc_item {
                    ast::AssocItem::Fn(fn_item) => fn_item.name().as_ref().map(ToString::to_string),
                    _ => None,
                })
        })
        .collect()
}

/// Adds an ink! constructor `fn` to an `impl` block.
pub fn add_constructor_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    let names = impl_fn_names(impl_item);
    let (text, snippet) = text_and_snippet(CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, "new", &names);
    add_callable_to_impl(
        impl_item,
        kind,
        range_option,
        "Add ink! constructor `fn`.".to_owned(),
        &text,
        &snippet,
    )
}

/// Adds an ink! message `fn` to an `impl` block.
pub fn add_message_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    let names = impl_fn_names(impl_item);
    let (text, snippet) = text_and_snippet(MESSAGE_PLAIN, MESSAGE_SNIPPET, "my_message", &names);
    add_callable_to_impl(
        impl_item,
        kind,
        range_option,
        "Add ink! message `fn`.".to_owned(),
        &text,
        &snippet,
    )
}

/// Adds an ink! message `fn` declaration to an ink! trait definition `trait` item.
pub fn add_message_to_trait_definition(
    trait_definition: &TraitDefinition,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    trait_definition.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the end of the
        // associated items list (if possible).
        range_option
            .or(trait_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_end)
                .map(|offset| TextRange::new(offset, offset)))
            .map(|range| {
                // Sets insert indent and suggested name.
                let indent = utils::item_children_indenting(trait_item.syntax());
                let names = fn_names(
                    trait_definition
                        .messages()
                        .iter()
                        .filter_map(Message::fn_item),
                );
                let (text, snippet) = text_and_snippet(
                    TRAIT_MESSAGE_PLAIN,
                    TRAIT_MESSAGE_SNIPPET,
                    "my_message",
                    &names,
                );

                Action {
                    label: "Add ink! message `fn`.".to_owned(),
                    kind,
                    range: utils::ink_trait_declaration_range(trait_definition),
                    edits: vec![TextEdit::replace_with_snippet(
                        utils::apply_indenting(&text, &indent),
                        range,
                        Some(utils::apply_indenting(&snippet, &indent)),
                    )],
                }
            })
    })
}

/// Adds an `ErrorCode` type to an ink! chain extension `trait` item.
pub fn add_error_code(
    chain_extension: &ChainExtension,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    chain_extension.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the beginning of the
        // associated items list (if possible).
        range_option
            .or(trait_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_start)
                .map(|offset| TextRange::new(offset, offset)))
            .map(|range| {
                // Sets insert indent.
                let indent = utils::item_children_indenting(trait_item.syntax());

                Action {
                    label: "Add `ErrorCode` type for ink! chain extension.".to_owned(),
                    kind,
                    range: utils::ink_trait_declaration_range(chain_extension),
                    edits: vec![TextEdit::replace_with_snippet(
                        utils::apply_indenting(ERROR_CODE_PLAIN, &indent),
                        range,
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
    range_option: Option<TextRange>,
) -> Option<Action> {
    chain_extension.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the end of the
        // associated items list (if possible).
        range_option
            .or(trait_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_end)
                .map(|offset| TextRange::new(offset, offset)))
            .map(|range| {
                // Sets insert indent and suggested name.
                let indent = utils::item_children_indenting(trait_item.syntax());
                let names = fn_names(
                    chain_extension
                        .extensions()
                        .iter()
                        .filter_map(Extension::fn_item),
                );
                let (mut text, mut snippet) =
                    text_and_snippet(EXTENSION_PLAIN, EXTENSION_SNIPPET, "my_extension", &names);
                let unavailable_ids = chain_extension
                    .extensions()
                    .iter()
                    .filter_map(Extension::id)
                    .collect();
                let id = utils::suggest_unique_id(None, &unavailable_ids).unwrap_or(1);
                if id > 1 {
                    text = text.replace("1)]", &format!("{id})]"));
                    snippet = snippet.replace("1})]", &format!("{id}}})]"));
                }

                Action {
                    label: "Add ink! extension `fn`.".to_owned(),
                    kind,
                    range: utils::ink_trait_declaration_range(chain_extension),
                    edits: vec![TextEdit::replace_with_snippet(
                        utils::apply_indenting(&text, &indent),
                        range,
                        Some(utils::apply_indenting(&snippet, &indent)),
                    )],
                }
            })
    })
}

/// Adds an ink! test `fn` to a `mod` item.
pub fn add_ink_test(
    module: &ast::Module,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    // Sets insert offset or defaults to inserting at the end of the
    // associated items list (if possible).
    range_option
        .or(module
            .item_list()
            .as_ref()
            .map(utils::item_insert_offset_end)
            .map(|offset| TextRange::new(offset, offset)))
        .map(|range| {
            // Sets insert indent and suggested name.
            let indent = utils::item_children_indenting(module.syntax());
            let names = mod_item_names!(module, Fn);
            let (text, snippet) =
                text_and_snippet(INK_TEST_PLAIN, INK_TEST_SNIPPET, "it_works", &names);

            Action {
                label: "Add ink! test `fn`.".to_owned(),
                kind,
                range: utils::ast_item_declaration_range(&ast::Item::Module(module.clone()))
                    .unwrap_or(module.syntax().text_range()),
                edits: vec![TextEdit::replace_with_snippet(
                    utils::apply_indenting(&text, &indent),
                    range,
                    Some(utils::apply_indenting(&snippet, &indent)),
                )],
            }
        })
}

/// Adds an ink! e2e test `fn` to a `mod` item.
pub fn add_ink_e2e_test(
    module: &ast::Module,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    // Sets insert offset or defaults to inserting at the end of the
    // associated items list (if possible).
    range_option
        .or(module
            .item_list()
            .as_ref()
            .map(utils::item_insert_offset_end)
            .map(|offset| TextRange::new(offset, offset)))
        .map(|range| {
            // Sets insert indent and suggested name.
            let indent = utils::item_children_indenting(module.syntax());
            let names = mod_item_names!(module, Fn);
            let (text, snippet) =
                text_and_snippet(INK_E2E_TEST_PLAIN, INK_E2E_TEST_SNIPPET, "it_works", &names);

            Action {
                label: "Add ink! e2e test `fn`.".to_owned(),
                kind,
                range: utils::ast_item_declaration_range(&ast::Item::Module(module.clone()))
                    .unwrap_or(module.syntax().text_range()),
                edits: vec![TextEdit::replace_with_snippet(
                    utils::apply_indenting(&text, &indent),
                    range,
                    Some(utils::apply_indenting(&snippet, &indent)),
                )],
            }
        })
}

/// Creates an insert edit with a snippet and indenting.
fn compose_edit_with_snippet_and_indent(
    text: &str,
    range: TextRange,
    snippet_option: Option<&str>,
    indent_option: Option<&str>,
) -> TextEdit {
    TextEdit::replace_with_snippet(
        match indent_option {
            Some(indent) => utils::apply_indenting(text, indent),
            None => text.to_owned(),
        },
        range,
        snippet_option.map(|snippet| match indent_option {
            Some(indent) => utils::apply_indenting(snippet, indent),
            None => snippet.to_owned(),
        }),
    )
}

/// Add an ink! contract `mod`.
pub fn add_contract(range: TextRange, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add ink! contract `mod`.".to_owned(),
        kind,
        range,
        edits: vec![compose_edit_with_snippet_and_indent(
            CONTRACT_PLAIN,
            range,
            Some(CONTRACT_SNIPPET),
            indent_option,
        )],
    }
}

/// Add an ink! trait definition `trait`.
pub fn add_trait_definition(
    range: TextRange,
    kind: ActionKind,
    indent_option: Option<&str>,
) -> Action {
    Action {
        label: "Add ink! trait definition.".to_owned(),
        kind,
        range,
        edits: vec![compose_edit_with_snippet_and_indent(
            TRAIT_DEFINITION_PLAIN,
            range,
            Some(TRAIT_DEFINITION_SNIPPET),
            indent_option,
        )],
    }
}

/// Add an ink! chain extension `trait`.
pub fn add_chain_extension(
    range: TextRange,
    kind: ActionKind,
    indent_option: Option<&str>,
) -> Action {
    Action {
        label: "Add ink! chain extension `trait`.".to_owned(),
        kind,
        range,
        edits: vec![compose_edit_with_snippet_and_indent(
            CHAIN_EXTENSION_PLAIN,
            range,
            Some(CHAIN_EXTENSION_SNIPPET),
            indent_option,
        )],
    }
}

/// Add an ink! storage item.
pub fn add_storage_item(range: TextRange, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add ink! storage item `ADT` (i.e. `struct`, `enum` or `union`).".to_owned(),
        kind,
        range,
        edits: vec![compose_edit_with_snippet_and_indent(
            STORAGE_ITEM_PLAIN,
            range,
            Some(STORAGE_ITEM_SNIPPET),
            indent_option,
        )],
    }
}

/// Add an ink! environment.
pub fn add_environment(range: TextRange, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add custom ink! environment implementation.".to_owned(),
        kind,
        range,
        edits: vec![compose_edit_with_snippet_and_indent(
            &format!("{ENVIRONMENT_DEF}\n\n{ENVIRONMENT_IMPL_PLAIN}"),
            range,
            Some(&format!("{ENVIRONMENT_DEF}\n\n{ENVIRONMENT_IMPL_SNIPPET}")),
            indent_option,
        )],
    }
}
