//! Utilities for composing text edits.

use std::collections::HashSet;

use ink_analyzer_ir::ast::{HasModuleItem, HasName};
use ink_analyzer_ir::syntax::{AstNode, TextRange};
use ink_analyzer_ir::{
    ast, ChainExtension, Contract, ContractRef, Extension, InkEntity, InkFile, IsInkFn, IsInkTrait,
    Message, TraitDefinition, Version,
};

use super::{utils, TextEdit};
use crate::codegen::snippets::{
    CHAIN_EXTENSION_PLAIN_V4, CHAIN_EXTENSION_PLAIN_V5, CHAIN_EXTENSION_SNIPPET_V4,
    CHAIN_EXTENSION_SNIPPET_V5, COMBINE_EXTENSIONS_PLAIN, COMBINE_EXTENSIONS_SNIPPET,
    CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, CONTRACT_PLAIN, CONTRACT_PLAIN_V4, CONTRACT_PLAIN_V5,
    CONTRACT_REF_MESSAGE_PLAIN, CONTRACT_REF_MESSAGE_SNIPPET, CONTRACT_REF_PLAIN,
    CONTRACT_REF_SNIPPET, CONTRACT_SNIPPET, CONTRACT_SNIPPET_V4, CONTRACT_SNIPPET_V5,
    ENVIRONMENT_PLAIN, ENVIRONMENT_PLAIN_V4, ENVIRONMENT_PLAIN_V5, ENVIRONMENT_SNIPPET,
    ENVIRONMENT_SNIPPET_V4, ENVIRONMENT_SNIPPET_V5, ERROR_CODE_PLAIN, ERROR_CODE_SNIPPET,
    ERROR_ENUM_PLAIN, ERROR_ENUM_SNIPPET, ERROR_STRUCT_PLAIN, ERROR_STRUCT_SNIPPET, EVENT_PLAIN,
    EVENT_PLAIN_V2, EVENT_SNIPPET, EVENT_SNIPPET_V2, EXTENSION_FN_PLAIN, EXTENSION_FN_PLAIN_V4,
    EXTENSION_FN_SNIPPET, EXTENSION_FN_SNIPPET_V4, INK_E2E_TEST_PLAIN, INK_E2E_TEST_PLAIN_V4,
    INK_E2E_TEST_SNIPPET, INK_E2E_TEST_SNIPPET_V4, INK_TEST_PLAIN, INK_TEST_SNIPPET, MESSAGE_PLAIN,
    MESSAGE_SNIPPET, STORAGE_ITEM_PLAIN, STORAGE_ITEM_SNIPPET, STORAGE_PLAIN, STORAGE_SNIPPET,
    TOPIC_PLAIN, TOPIC_SNIPPET, TRAIT_DEFINITION_PLAIN, TRAIT_DEFINITION_SNIPPET,
    TRAIT_MESSAGE_PLAIN, TRAIT_MESSAGE_SNIPPET,
};
use crate::resolution;

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
        if version.is_legacy() {
            CONTRACT_PLAIN_V4
        } else if version.is_v5() {
            CONTRACT_PLAIN_V5
        } else {
            CONTRACT_PLAIN
        },
        range,
        Some(if version.is_legacy() {
            CONTRACT_SNIPPET_V4
        } else if version.is_v5() {
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

/// Creates text edit for ink! contract reference.
pub fn add_contract_ref(range: TextRange, indent: Option<&str>) -> TextEdit {
    text_edit_with_indent(
        CONTRACT_REF_PLAIN,
        range,
        Some(CONTRACT_REF_SNIPPET),
        indent,
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

/// Creates text edit for ink! contract reference message declaration.
pub fn add_message_to_contract_ref(contract_ref: &ContractRef, range: TextRange) -> TextEdit {
    // Sets insert indent and suggested name.
    let indent = utils::item_children_indenting(contract_ref.syntax());
    let names = contract_ref
        .messages()
        .iter()
        .filter_map(Message::fn_item)
        .filter_map(|fn_item| fn_item.name().as_ref().map(ToString::to_string))
        .collect();
    let (text, snippet) = unique_text_and_snippet(
        CONTRACT_REF_MESSAGE_PLAIN,
        CONTRACT_REF_MESSAGE_SNIPPET,
        "message",
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
        if version.is_legacy() {
            CHAIN_EXTENSION_PLAIN_V4
        } else {
            CHAIN_EXTENSION_PLAIN_V5
        },
        range,
        Some(if version.is_legacy() {
            CHAIN_EXTENSION_SNIPPET_V4
        } else {
            CHAIN_EXTENSION_SNIPPET_V5
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
    let (mut text, mut snippet) = if version.is_legacy() {
        let preferred_name = "my_extension";
        let suggested_name = utils::suggest_unique_name(preferred_name, &names);
        (
            EXTENSION_FN_PLAIN_V4.replace(preferred_name, &suggested_name),
            EXTENSION_FN_SNIPPET_V4.replace(preferred_name, &suggested_name),
        )
    } else {
        let preferred_name = "my_function";
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

/// Creates text edit for ink! combine extensions definition.
pub fn add_combine_extensions(
    range: TextRange,
    indent: Option<&str>,
    file: Option<&InkFile>,
) -> TextEdit {
    if let Some(file) = file {
        // Attempts to create a snippet including existing local chain extensions.
        let extensions = file.chain_extensions();
        if !extensions.is_empty() {
            let mut fields_plain = Vec::new();
            let mut fields_snippet = Vec::new();
            let field_indent = "        ";
            // Save one tab stop for the struct name.
            let tab_stop_offset = 1;

            let ext_paths = extensions
                .iter()
                .filter_map(|ext| ext.trait_item().and_then(resolution::item_path));
            for (idx, ext_path) in ext_paths.enumerate() {
                let field_idx = idx + 1;
                let tab_stop_1 = tab_stop_offset + (idx * 2) + 1;
                let tab_stop_2 = tab_stop_1 + 1;
                fields_plain.push(format!("{field_indent}pub ext{field_idx}: {ext_path},"));
                fields_snippet.push(format!(
                    "{field_indent}pub ${{{tab_stop_1}:ext{field_idx}}}: ${{{tab_stop_2}:{ext_path}}},"
                ));
            }

            if !fields_plain.is_empty() {
                let text = COMBINE_EXTENSIONS_PLAIN.replace(
                    if fields_plain.len() > 1 {
                        "        pub ext1: Extension1,\n        pub ext2: Extension2,"
                    } else {
                        "        pub ext1: Extension1,"
                    },
                    &fields_plain.join("\n"),
                );
                let snippet = COMBINE_EXTENSIONS_SNIPPET.replace(
                    if fields_snippet.len() > 1 {
                        "        pub ${2:ext1}: ${3:Extension1},\n        pub ${4:ext2}: ${5:Extension2},"
                    } else {
                        "        pub ${2:ext1}: ${3:Extension1},"
                    },
                    &fields_snippet.join("\n"),
                );
                return text_edit_with_indent(&text, range, Some(&snippet), indent);
            }
        }
    }

    // Defaults to generic snippets.
    text_edit_with_indent(
        COMBINE_EXTENSIONS_PLAIN,
        range,
        Some(COMBINE_EXTENSIONS_SNIPPET),
        indent,
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
        if version.is_legacy() {
            ENVIRONMENT_PLAIN_V4
        } else if version.is_v5() {
            ENVIRONMENT_PLAIN_V5
        } else {
            ENVIRONMENT_PLAIN
        },
        range,
        Some(if version.is_legacy() {
            ENVIRONMENT_SNIPPET_V4
        } else if version.is_v5() {
            ENVIRONMENT_SNIPPET_V5
        } else {
            ENVIRONMENT_SNIPPET
        }),
        indent,
    )
}

/// Creates text edit for ink! error `enum`.
pub fn add_error_enum(range: TextRange, indent: Option<&str>) -> TextEdit {
    text_edit_with_indent(ERROR_ENUM_PLAIN, range, Some(ERROR_ENUM_SNIPPET), indent)
}

/// Creates text edit for ink! error `struct`.
pub fn add_error_struct(range: TextRange, indent: Option<&str>) -> TextEdit {
    text_edit_with_indent(
        ERROR_STRUCT_PLAIN,
        range,
        Some(ERROR_STRUCT_SNIPPET),
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
        if version.is_legacy() {
            INK_E2E_TEST_PLAIN_V4
        } else {
            INK_E2E_TEST_PLAIN
        },
        if version.is_legacy() {
            INK_E2E_TEST_SNIPPET_V4
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
