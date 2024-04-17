//! Utilities for composing text edits.

use std::collections::HashSet;

use ink_analyzer_ir::ast::{HasModuleItem, HasName};
use ink_analyzer_ir::syntax::{AstNode, TextRange};
use ink_analyzer_ir::{
    ast, ChainExtension, Contract, Extension, InkEntity, IsInkFn, Message, TraitDefinition, Version,
};

use super::{utils, TextEdit};
use crate::codegen::snippets::{
    CHAIN_EXTENSION_PLAIN, CHAIN_EXTENSION_PLAIN_V5, CHAIN_EXTENSION_SNIPPET,
    CHAIN_EXTENSION_SNIPPET_V5, CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, CONTRACT_PLAIN,
    CONTRACT_PLAIN_V5, CONTRACT_SNIPPET, CONTRACT_SNIPPET_V5, ENVIRONMENT_PLAIN,
    ENVIRONMENT_PLAIN_V5, ENVIRONMENT_SNIPPET, ENVIRONMENT_SNIPPET_V5, ERROR_CODE_PLAIN,
    ERROR_CODE_SNIPPET, EVENT_PLAIN, EVENT_PLAIN_V2, EVENT_SNIPPET, EVENT_SNIPPET_V2,
    EXTENSION_FN_PLAIN, EXTENSION_FN_PLAIN_V5, EXTENSION_FN_SNIPPET, EXTENSION_FN_SNIPPET_V5,
    INK_E2E_TEST_PLAIN, INK_E2E_TEST_PLAIN_V5, INK_E2E_TEST_SNIPPET, INK_E2E_TEST_SNIPPET_V5,
    INK_TEST_PLAIN, INK_TEST_SNIPPET, MESSAGE_PLAIN, MESSAGE_SNIPPET, STORAGE_ITEM_PLAIN,
    STORAGE_ITEM_SNIPPET, STORAGE_PLAIN, STORAGE_SNIPPET, TOPIC_PLAIN, TOPIC_SNIPPET,
    TRAIT_DEFINITION_PLAIN, TRAIT_DEFINITION_SNIPPET, TRAIT_MESSAGE_PLAIN, TRAIT_MESSAGE_SNIPPET,
};

pub fn unique_text_and_snippet(
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

/// Creates a text edit with a snippet and indenting.
pub fn text_edit_with_indent(
    text: &str,
    range: TextRange,
    snippet: Option<&str>,
    indent: Option<&str>,
) -> TextEdit {
    TextEdit::replace_with_snippet(
        match indent {
            Some(indent) => utils::apply_indenting(text, indent),
            None => text.to_owned(),
        },
        range,
        snippet.map(|snippet| match indent {
            Some(indent) => utils::apply_indenting(snippet, indent),
            None => snippet.to_owned(),
        }),
    )
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

/// Creates text edit for ink! contract.
pub fn add_contract(range: TextRange, indent: Option<&str>, version: Version) -> TextEdit {
    text_edit_with_indent(
        if version == Version::V5 {
            CONTRACT_PLAIN_V5
        } else {
            CONTRACT_PLAIN
        },
        range,
        Some(if version == Version::V5 {
            CONTRACT_SNIPPET_V5
        } else {
            CONTRACT_SNIPPET
        }),
        indent,
    )
}

/// Creates text edit for ink! storage.
pub fn add_storage(contract: &Contract, range: TextRange) -> TextEdit {
    // Sets insert indent.
    let indent = utils::item_children_indenting(contract.syntax());
    // Gets the "resolved" contract name.
    let contract_name = utils::resolve_contract_name(contract);

    TextEdit::replace_with_snippet(
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
    )
}

/// Creates text edit for ink! event.
pub fn add_event_v1(module: &ast::Module, range: TextRange) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(module.syntax());
    let names = mod_item_names!(module, Struct);
    let (text, snippet) = unique_text_and_snippet(EVENT_PLAIN, EVENT_SNIPPET, "Event", &names);

    text_edit_with_indent(
        &text,
        range,
        Some(&snippet),
        (!indent.is_empty()).then_some(&indent),
    )
}

/// Creates text edit for ink! event 2.0.
pub fn add_event_v2(
    range: TextRange,
    indent: Option<&str>,
    module: Option<&ast::Module>,
) -> TextEdit {
    // Sets insert indent and suggested name.
    let (text, snippet) = if let Some(module) = module {
        let names = mod_item_names!(module, Struct);
        unique_text_and_snippet(EVENT_PLAIN_V2, EVENT_SNIPPET_V2, "Event", &names)
    } else {
        (EVENT_PLAIN_V2.to_owned(), EVENT_SNIPPET_V2.to_owned())
    };

    text_edit_with_indent(&text, range, Some(&snippet), indent)
}

/// Creates text edit for ink! topic.
pub fn add_topic(
    struct_item: &ast::Struct,
    range: TextRange,
    prefix: Option<&str>,
    suffix: Option<&str>,
) -> TextEdit {
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
    let (text, snippet) = unique_text_and_snippet(TOPIC_PLAIN, TOPIC_SNIPPET, "my_topic", &names);

    TextEdit::replace_with_snippet(
        format!(
            "{}{}{}",
            prefix.unwrap_or_default(),
            utils::apply_indenting(&text, &indent),
            suffix.unwrap_or_default()
        ),
        range,
        Some(format!(
            "{}{}{}",
            prefix.unwrap_or_default(),
            utils::apply_indenting(&snippet, &indent),
            suffix.unwrap_or_default()
        )),
    )
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

/// Creates text edit for ink! constructor.
pub fn add_constructor_to_impl(impl_item: &ast::Impl, range: TextRange) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(impl_item.syntax());
    let names = impl_fn_names(impl_item);
    let (text, snippet) =
        unique_text_and_snippet(CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, "new", &names);

    TextEdit::replace_with_snippet(
        utils::apply_indenting(&text, &indent),
        range,
        Some(utils::apply_indenting(&snippet, &indent)),
    )
}

/// Creates text edit for ink! message.
pub fn add_message_to_impl(impl_item: &ast::Impl, range: TextRange) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(impl_item.syntax());
    let names = impl_fn_names(impl_item);
    let (text, snippet) =
        unique_text_and_snippet(MESSAGE_PLAIN, MESSAGE_SNIPPET, "my_message", &names);

    TextEdit::replace_with_snippet(
        utils::apply_indenting(&text, &indent),
        range,
        Some(utils::apply_indenting(&snippet, &indent)),
    )
}

/// Creates text edit for ink! trait definition.
pub fn add_trait_def(range: TextRange, indent: Option<&str>) -> TextEdit {
    text_edit_with_indent(
        TRAIT_DEFINITION_PLAIN,
        range,
        Some(TRAIT_DEFINITION_SNIPPET),
        indent,
    )
}

/// Creates text edit for ink! trait definition message declaration.
pub fn add_message_to_trait_def(trait_def: &TraitDefinition, range: TextRange) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(trait_def.syntax());
    let names = trait_def
        .messages()
        .iter()
        .filter_map(Message::fn_item)
        .filter_map(|fn_item| fn_item.name().as_ref().map(ToString::to_string))
        .collect();
    let (text, snippet) = unique_text_and_snippet(
        TRAIT_MESSAGE_PLAIN,
        TRAIT_MESSAGE_SNIPPET,
        "my_message",
        &names,
    );

    TextEdit::replace_with_snippet(
        utils::apply_indenting(&text, &indent),
        range,
        Some(utils::apply_indenting(&snippet, &indent)),
    )
}

/// Creates text edit for ink! chain extension.
pub fn add_chain_extension(range: TextRange, indent: Option<&str>, version: Version) -> TextEdit {
    text_edit_with_indent(
        if version == Version::V5 {
            CHAIN_EXTENSION_PLAIN_V5
        } else {
            CHAIN_EXTENSION_PLAIN
        },
        range,
        Some(if version == Version::V5 {
            CHAIN_EXTENSION_SNIPPET_V5
        } else {
            CHAIN_EXTENSION_SNIPPET
        }),
        indent,
    )
}

/// Creates text edit for ink! chain extension `ErrorCode` type.
pub fn add_error_code_type(chain_extension: &ChainExtension, range: TextRange) -> TextEdit {
    // Sets insert indent.
    let indent = utils::item_children_indenting(chain_extension.syntax());

    TextEdit::replace_with_snippet(
        utils::apply_indenting(ERROR_CODE_PLAIN, &indent),
        range,
        Some(utils::apply_indenting(ERROR_CODE_SNIPPET, &indent)),
    )
}

/// Creates text edit for ink! chain extension associated `fn`.
pub fn add_extension(
    chain_extension: &ChainExtension,
    range: TextRange,
    version: Version,
) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(chain_extension.syntax());
    let names = chain_extension
        .extensions()
        .iter()
        .filter_map(Extension::fn_item)
        .filter_map(|fn_item| fn_item.name().as_ref().map(ToString::to_string))
        .collect();
    let (mut text, mut snippet) = if version == Version::V5 {
        let preferred_name = "my_function";
        let suggested_name = utils::suggest_unique_name(preferred_name, &names);
        (
            EXTENSION_FN_PLAIN_V5.replace(preferred_name, &suggested_name),
            EXTENSION_FN_SNIPPET_V5.replace(preferred_name, &suggested_name),
        )
    } else {
        let preferred_name = "my_extension";
        let suggested_name = utils::suggest_unique_name(preferred_name, &names);
        (
            EXTENSION_FN_PLAIN.replace(preferred_name, &suggested_name),
            EXTENSION_FN_SNIPPET.replace(preferred_name, &suggested_name),
        )
    };
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

    TextEdit::replace_with_snippet(
        utils::apply_indenting(&text, &indent),
        range,
        Some(utils::apply_indenting(&snippet, &indent)),
    )
}

/// Creates text edit for ink! storage item.
pub fn add_storage_item(range: TextRange, indent: Option<&str>) -> TextEdit {
    text_edit_with_indent(
        STORAGE_ITEM_PLAIN,
        range,
        Some(STORAGE_ITEM_SNIPPET),
        indent,
    )
}

/// Creates text edit custom ink! environment.
pub fn add_environment(range: TextRange, indent: Option<&str>, version: Version) -> TextEdit {
    text_edit_with_indent(
        if version == Version::V5 {
            ENVIRONMENT_PLAIN_V5
        } else {
            ENVIRONMENT_PLAIN
        },
        range,
        Some(if version == Version::V5 {
            ENVIRONMENT_SNIPPET_V5
        } else {
            ENVIRONMENT_SNIPPET
        }),
        indent,
    )
}

/// Creates text edit for ink! test.
pub fn add_test(module: &ast::Module, range: TextRange) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(module.syntax());
    let names = mod_item_names!(module, Fn);
    let (text, snippet) =
        unique_text_and_snippet(INK_TEST_PLAIN, INK_TEST_SNIPPET, "it_works", &names);

    TextEdit::replace_with_snippet(
        utils::apply_indenting(&text, &indent),
        range,
        Some(utils::apply_indenting(&snippet, &indent)),
    )
}

/// Creates text edit for ink! test.
pub fn add_e2e_test(module: &ast::Module, range: TextRange, version: Version) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(module.syntax());
    let names = mod_item_names!(module, Fn);
    let (text, snippet) = unique_text_and_snippet(
        if version == Version::V5 {
            INK_E2E_TEST_PLAIN_V5
        } else {
            INK_E2E_TEST_PLAIN
        },
        if version == Version::V5 {
            INK_E2E_TEST_SNIPPET_V5
        } else {
            INK_E2E_TEST_SNIPPET
        },
        "it_works",
        &names,
    );

    TextEdit::replace_with_snippet(
        utils::apply_indenting(&text, &indent),
        range,
        Some(utils::apply_indenting(&snippet, &indent)),
    )
}
