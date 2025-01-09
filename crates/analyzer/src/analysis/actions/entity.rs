//! ink! entity code/intent actions.

use std::collections::HashSet;

use ink_analyzer_ir::{
    ast::{self, HasName},
    syntax::{AstNode, TextRange},
    ChainExtension, Constructor, Contract, InkEntity, InkFile, IsInkEvent, IsInkFn, IsInkTrait,
    Message, TraitDefinition, Version,
};

use super::{Action, ActionKind};
use crate::analysis::{
    text_edit::{self, TextEdit},
    utils,
};
use crate::codegen::snippets::{
    CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, MESSAGE_PLAIN, MESSAGE_SNIPPET,
};

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
            .or_else(|| {
                module
                    .item_list()
                    .as_ref()
                    .map(utils::item_insert_offset_start)
                    .map(|offset| TextRange::new(offset, offset))
            })
            .map(|range| Action {
                label: "Add ink! storage `struct`.".to_owned(),
                kind,
                range: utils::contract_declaration_range(contract),
                edits: vec![text_edit::add_storage(contract, range)],
            })
    })
}

/// Adds an ink! event `struct` to an ink! contract `mod` item.
pub fn add_event_v1(
    contract: &Contract,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    contract.module().and_then(|module| {
        // Sets insert offset or defaults to inserting after either the last struct or
        // the beginning of the associated items list (if possible).
        range_option
            .or_else(|| {
                module
                    .item_list()
                    .as_ref()
                    .map(utils::item_insert_offset_after_last_struct_or_start)
                    .map(|offset| TextRange::new(offset, offset))
            })
            .map(|range| Action {
                label: "Add ink! event `struct`.".to_owned(),
                kind,
                range: utils::contract_declaration_range(contract),
                edits: vec![text_edit::add_event_v1(module, range)],
            })
    })
}

/// Adds an ink! event 2.0 `struct`.
pub fn add_event_v2(range: TextRange, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add ink! event 2.0 `struct`.".to_owned(),
        kind,
        range,
        edits: vec![text_edit::add_event_v2(range, indent_option, None)],
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
            .or_else(|| {
                struct_item
                    .field_list()
                    .as_ref()
                    .map(utils::field_insert_offset_end_and_affixes)
                    .map(|(offset, prefix, suffix)| {
                        (TextRange::new(offset, offset), prefix, suffix)
                    })
            })
            .map(|(range, prefix, suffix)| Action {
                label: "Add ink! topic `field`.".to_owned(),
                kind,
                range: utils::ast_item_declaration_range(&ast::Item::Struct(struct_item.clone()))
                    .unwrap_or(struct_item.syntax().text_range()),
                edits: vec![text_edit::add_topic(
                    struct_item,
                    range,
                    prefix.as_deref(),
                    suffix.as_deref(),
                )],
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
        .or_else(|| {
            range_option
                .zip(utils::callable_impl_indent_and_affixes(contract))
                .map(|(insert_offset, (indent, prefix, suffix))| {
                    (insert_offset, indent, Some(prefix), Some(suffix))
                })
                // Defaults to inserting in the first non-trait `impl` block or
                // creating a new `impl` block if necessary
                .or_else(|| {
                    utils::callable_insert_offset_indent_and_affixes(contract).map(
                        |(offset, ident, prefix, suffix)| {
                            (TextRange::new(offset, offset), ident, prefix, suffix)
                        },
                    )
                })
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
                })
        })
}

fn contract_fn_names(contract: &Contract) -> HashSet<String> {
    contract
        .constructors()
        .iter()
        .filter_map(Constructor::fn_item)
        .chain(contract.messages().iter().filter_map(Message::fn_item))
        .filter_map(|fn_item| fn_item.name().as_ref().map(ToString::to_string))
        .collect()
}

/// Adds an ink! constructor `fn` to the first non-trait `impl` block or
/// creates a new `impl` block if necessary.
pub fn add_constructor_to_contract(
    contract: &Contract,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    let names = contract_fn_names(contract);
    let (text, snippet) =
        text_edit::unique_text_and_snippet(CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, "new", &names);
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
    let (text, snippet) =
        text_edit::unique_text_and_snippet(MESSAGE_PLAIN, MESSAGE_SNIPPET, "my_message", &names);
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
    let (mut text, mut snippet) =
        text_edit::unique_text_and_snippet(MESSAGE_PLAIN, MESSAGE_SNIPPET, fn_name, &names);
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
        .or_else(|| {
            impl_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_end)
                .map(|offset| TextRange::new(offset, offset))
        })
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

/// Adds an ink! constructor `fn` to an `impl` block.
pub fn add_constructor_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    range_option
        .or_else(|| {
            impl_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_end)
                .map(|offset| TextRange::new(offset, offset))
        })
        .map(|range| Action {
            label: "Add ink! constructor `fn`.".to_owned(),
            kind,
            range: utils::ast_item_declaration_range(&ast::Item::Impl(impl_item.clone()))
                .unwrap_or(impl_item.syntax().text_range()),
            edits: vec![text_edit::add_constructor_to_impl(impl_item, range)],
        })
}

/// Adds an ink! message `fn` to an `impl` block.
pub fn add_message_to_impl(
    impl_item: &ast::Impl,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    range_option
        .or_else(|| {
            impl_item
                .assoc_item_list()
                .as_ref()
                .map(utils::assoc_item_insert_offset_end)
                .map(|offset| TextRange::new(offset, offset))
        })
        .map(|range| Action {
            label: "Add ink! message `fn`.".to_owned(),
            kind,
            range: utils::ast_item_declaration_range(&ast::Item::Impl(impl_item.clone()))
                .unwrap_or(impl_item.syntax().text_range()),
            edits: vec![text_edit::add_message_to_impl(impl_item, range)],
        })
}

/// Adds an ink! message `fn` declaration to an ink! trait definition `trait` item.
pub fn add_message_to_trait_def(
    trait_definition: &TraitDefinition,
    kind: ActionKind,
    range_option: Option<TextRange>,
) -> Option<Action> {
    trait_definition.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the end of the
        // associated items list (if possible).
        range_option
            .or_else(|| {
                trait_item
                    .assoc_item_list()
                    .as_ref()
                    .map(utils::assoc_item_insert_offset_end)
                    .map(|offset| TextRange::new(offset, offset))
            })
            .map(|range| Action {
                label: "Add ink! message `fn`.".to_owned(),
                kind,
                range: utils::ink_trait_declaration_range(trait_definition),
                edits: vec![text_edit::add_message_to_trait_def(trait_definition, range)],
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
            .or_else(|| {
                trait_item
                    .assoc_item_list()
                    .as_ref()
                    .map(utils::assoc_item_insert_offset_start)
                    .map(|offset| TextRange::new(offset, offset))
            })
            .map(|range| Action {
                label: "Add `ErrorCode` type for ink! chain extension.".to_owned(),
                kind,
                range: utils::ink_trait_declaration_range(chain_extension),
                edits: vec![text_edit::add_error_code_type(chain_extension, range)],
            })
    })
}

/// Adds an extension `fn` declaration to an ink! chain extension `trait` item.
pub fn add_extension(
    chain_extension: &ChainExtension,
    kind: ActionKind,
    range_option: Option<TextRange>,
    version: Version,
) -> Option<Action> {
    chain_extension.trait_item().and_then(|trait_item| {
        // Sets insert offset or defaults to inserting at the end of the
        // associated items list (if possible).
        range_option
            .or_else(|| {
                trait_item
                    .assoc_item_list()
                    .as_ref()
                    .map(utils::assoc_item_insert_offset_end)
                    .map(|offset| TextRange::new(offset, offset))
            })
            .map(|range| {
                // Sets insert indent and suggested name.
                Action {
                    label: format!(
                        "Add ink! {} `fn`.",
                        if version.is_v5() {
                            "function"
                        } else {
                            "extension"
                        }
                    ),
                    kind,
                    range: utils::ink_trait_declaration_range(chain_extension),
                    edits: vec![text_edit::add_extension(chain_extension, range, version)],
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
        .or_else(|| {
            module
                .item_list()
                .as_ref()
                .map(utils::item_insert_offset_end)
                .map(|offset| TextRange::new(offset, offset))
        })
        .map(|range| Action {
            label: "Add ink! test `fn`.".to_owned(),
            kind,
            range: utils::ast_item_declaration_range(&ast::Item::Module(module.clone()))
                .unwrap_or(module.syntax().text_range()),
            edits: vec![text_edit::add_test(module, range)],
        })
}

/// Adds an ink! e2e test `fn` to a `mod` item.
pub fn add_ink_e2e_test(
    module: &ast::Module,
    kind: ActionKind,
    range_option: Option<TextRange>,
    version: Version,
) -> Option<Action> {
    // Sets insert offset or defaults to inserting at the end of the
    // associated items list (if possible).
    range_option
        .or_else(|| {
            module
                .item_list()
                .as_ref()
                .map(utils::item_insert_offset_end)
                .map(|offset| TextRange::new(offset, offset))
        })
        .map(|range| Action {
            label: "Add ink! e2e test `fn`.".to_owned(),
            kind,
            range: utils::ast_item_declaration_range(&ast::Item::Module(module.clone()))
                .unwrap_or(module.syntax().text_range()),
            edits: vec![text_edit::add_e2e_test(module, range, version)],
        })
}

/// Add an ink! contract `mod`.
pub fn add_contract(
    range: TextRange,
    kind: ActionKind,
    indent_option: Option<&str>,
    version: Version,
) -> Action {
    Action {
        label: "Add ink! contract `mod`.".to_owned(),
        kind,
        range,
        edits: vec![text_edit::add_contract(range, indent_option, version)],
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
        edits: vec![text_edit::add_trait_def(range, indent_option)],
    }
}

/// Add an ink! chain extension `trait`.
pub fn add_chain_extension(
    range: TextRange,
    kind: ActionKind,
    indent_option: Option<&str>,
    version: Version,
) -> Action {
    Action {
        label: "Add ink! chain extension `trait`.".to_owned(),
        kind,
        range,
        edits: vec![text_edit::add_chain_extension(
            range,
            indent_option,
            version,
        )],
    }
}

/// Add an ink! combine extensions definition.
pub fn add_combine_extensions(
    range: TextRange,
    kind: ActionKind,
    indent_option: Option<&str>,
    file: Option<&InkFile>,
) -> Action {
    Action {
        label: "Add ink! combine extensions definition.".to_owned(),
        kind,
        range,
        edits: vec![text_edit::add_combine_extensions(
            range,
            indent_option,
            file,
        )],
    }
}

/// Add an ink! storage item.
pub fn add_storage_item(range: TextRange, kind: ActionKind, indent_option: Option<&str>) -> Action {
    Action {
        label: "Add ink! storage item `ADT` (i.e. `struct`, `enum` or `union`).".to_owned(),
        kind,
        range,
        edits: vec![text_edit::add_storage_item(range, indent_option)],
    }
}

/// Add an ink! environment.
pub fn add_environment(
    range: TextRange,
    kind: ActionKind,
    indent_option: Option<&str>,
    version: Version,
) -> Action {
    Action {
        label: "Add custom ink! environment implementation.".to_owned(),
        kind,
        range,
        edits: vec![text_edit::add_environment(range, indent_option, version)],
    }
}
