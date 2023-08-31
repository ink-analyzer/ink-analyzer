//! ink! contract diagnostics.

use ink_analyzer_ir::meta::MetaValue;
use ink_analyzer_ir::syntax::{AstNode, SyntaxKind, SyntaxNode, SyntaxToken, TextRange};
use ink_analyzer_ir::{
    ast, Contract, FromInkAttribute, FromSyntax, InkArg, InkArgKind, InkAttributeKind,
    InkMacroKind, IsInkCallable, IsInkEntity, Selector, SelectorArg, Storage,
};
use std::collections::HashSet;

use super::{constructor, event, ink_e2e_test, ink_impl, ink_test, message, storage, utils};
use crate::analysis::snippets::{
    CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, MESSAGE_PLAIN, MESSAGE_SNIPPET, STORAGE_PLAIN,
    STORAGE_SNIPPET,
};
use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils as analysis_utils;
use crate::{Action, Diagnostic, Severity};

/// Runs all ink! contract diagnostics.
///
/// The entry point for finding ink! contract semantic rules is the contract module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/contract.rs#L47-L73>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, contract: &Contract) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, contract);

    // Ensures that ink! contract is an inline `mod` item, see `ensure_inline_module` doc.
    if let Some(diagnostic) = ensure_inline_module(contract) {
        results.push(diagnostic);
    }

    // Ensures that exactly one ink! storage definition, see `ensure_storage_quantity` doc.
    ensure_storage_quantity(results, contract);

    // Runs ink! storage diagnostics, see `storage::diagnostics` doc.
    ink_analyzer_ir::ink_closest_descendants::<Storage>(contract.syntax())
        .for_each(|item| storage::diagnostics(results, &item));

    // Runs ink! event diagnostics, see `event::diagnostics` doc.
    contract
        .events()
        .iter()
        .for_each(|item| event::diagnostics(results, item));

    // Runs ink! impl diagnostics, see `ink_impl::diagnostics` doc.
    contract
        .impls()
        .iter()
        .for_each(|item| ink_impl::diagnostics(results, item, true));

    // Ensures that at least one ink! constructor, see `ensure_contains_constructor` doc.
    if let Some(diagnostic) = ensure_contains_constructor(contract) {
        results.push(diagnostic);
    }

    // Runs ink! constructor diagnostics, see `constructor::diagnostics` doc.
    contract
        .constructors()
        .iter()
        .for_each(|item| constructor::diagnostics(results, item));

    // Ensures that at least one ink! message, see `ensure_contains_message` doc.
    if let Some(diagnostic) = ensure_contains_message(contract) {
        results.push(diagnostic);
    }

    // Runs ink! message diagnostics, see `message::diagnostics` doc.
    contract
        .messages()
        .iter()
        .for_each(|item| message::diagnostics(results, item));

    // Ensures that no ink! message or constructor selectors are overlapping,
    // see `ensure_no_overlapping_selectors` doc.
    ensure_no_overlapping_selectors(results, contract);

    // Ensures that at most one wildcard selector exists among ink! messages, as well as ink! constructors,
    // see `ensure_at_most_one_wildcard_selector` doc.
    ensure_at_most_one_wildcard_selector(results, contract);

    // Ensures that ink! storage, ink! events and ink! impls are defined in the root of the ink! contract,
    // see `ensure_root_items` doc.
    ensure_root_items(results, contract);

    // Ensures that ink! messages and constructors are defined in the root of an `impl` item,
    // see `ensure_impl_parent_for_callables` doc.
    ensure_impl_parent_for_callables(results, contract);

    // Runs ink! test diagnostics, see `ink_test::diagnostics` doc.
    contract
        .tests()
        .iter()
        .for_each(|item| ink_test::diagnostics(results, item));

    // Runs ink! e2e test diagnostics, see `ink_e2e_test::diagnostics` doc.
    contract
        .e2e_tests()
        .iter()
        .for_each(|item| ink_e2e_test::diagnostics(results, item));

    // Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // See `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, contract);
}

/// Ensures that ink! contract attribute is applied to an inline `mod` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L301-L309>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L298>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/contract.rs#L66>.
fn ensure_inline_module(contract: &Contract) -> Option<Diagnostic> {
    match contract.module() {
        Some(module) => module.item_list().is_none().then(|| {
            let semicolon_token = contract.module().and_then(ast::Module::semicolon_token);
            // Edit range for quickfix.
            let range = semicolon_token
                .as_ref()
                .map(SyntaxToken::text_range)
                .unwrap_or(contract.syntax().text_range());
            Diagnostic {
                message: "The content of an ink! contract's `mod` item must be defined inline."
                    .to_string(),
                range: contract.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Add inline body to ink! contract `mod`.".to_string(),
                    range,
                    edits: vec![TextEdit::replace(
                        format!("{}{{}}", if semicolon_token.is_some() { " " } else { "" }),
                        range,
                    )],
                }]),
            }
        }),
        None => Some(Diagnostic {
            message: "ink! contracts must be inline `mod` items".to_string(),
            range: contract.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: Some(if contract.syntax().kind() == SyntaxKind::ITEM_LIST {
                vec![Action::remove_attribute(contract.ink_attr())]
            } else {
                vec![
                    Action::remove_attribute(contract.ink_attr()),
                    Action::remove_item(contract.syntax()),
                ]
            }),
        }),
    }
}

/// Ensures that ink! storage is not missing and there are not multiple ink! storage definitions.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L328>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L328>.
fn ensure_storage_quantity(results: &mut Vec<Diagnostic>, contract: &Contract) {
    utils::ensure_exactly_one_item(
        results,
        // All storage definitions.
        &ink_analyzer_ir::ink_closest_descendants::<Storage>(contract.syntax())
            .collect::<Vec<Storage>>(),
        Diagnostic {
            message: "Missing ink! storage definition.".to_string(),
            range: contract.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: contract
                .module()
                .and_then(|mod_item| Some(mod_item).zip(mod_item.item_list()))
                .map(|(mod_item, item_list)| {
                    // Set quickfix insertion offset at the beginning of the associated items list.
                    let insert_offset = analysis_utils::item_insert_offset_start(&item_list);
                    // Set quickfix insertion indent.
                    let indent = analysis_utils::item_children_indenting(mod_item.syntax());
                    // Gets the "resolved" contract name.
                    let contract_name = analysis_utils::resolve_contract_name(contract);

                    vec![Action {
                        label: "Add ink! storage `struct`.".to_string(),
                        range: TextRange::new(insert_offset, insert_offset),
                        edits: vec![TextEdit::insert_with_snippet(
                            analysis_utils::apply_indenting(
                                contract_name
                                    .as_deref()
                                    .map(|name| STORAGE_PLAIN.replace("Storage", name))
                                    .as_deref()
                                    .unwrap_or(STORAGE_PLAIN),
                                &indent,
                            ),
                            insert_offset,
                            Some(analysis_utils::apply_indenting(
                                contract_name
                                    .as_deref()
                                    .map(|name| STORAGE_SNIPPET.replace("Storage", name))
                                    .as_deref()
                                    .unwrap_or(STORAGE_SNIPPET),
                                &indent,
                            )),
                        )],
                    }]
                }),
        },
        "Only one ink! storage definition can be defined for an ink! contract.",
        Severity::Error,
    );
}

/// Ensures that at least one ink! constructor.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L330>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L145-L165>.
fn ensure_contains_constructor(contract: &Contract) -> Option<Diagnostic> {
    utils::ensure_at_least_one_item(
        contract.constructors(),
        Diagnostic {
            message: "At least one ink! constructor must be defined for an ink! contract."
                .to_string(),
            range: contract.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: analysis_utils::callable_insert_offset_indent_and_affixes(contract).map(
                |(insert_offset, indent, prefix, suffix)| {
                    // Adds an ink! constructor to the first non-trait `impl` block or creates a new `impl` block if necessary.
                    vec![Action {
                        label: "Add ink! constructor `fn`.".to_string(),
                        range: TextRange::new(insert_offset, insert_offset),
                        edits: vec![TextEdit::insert_with_snippet(
                            format!(
                                "{}{}{}",
                                prefix.as_deref().unwrap_or_default(),
                                analysis_utils::apply_indenting(CONSTRUCTOR_PLAIN, &indent),
                                suffix.as_deref().unwrap_or_default()
                            ),
                            insert_offset,
                            Some(format!(
                                "{}{}{}",
                                prefix.as_deref().unwrap_or_default(),
                                analysis_utils::apply_indenting(CONSTRUCTOR_SNIPPET, &indent),
                                suffix.as_deref().unwrap_or_default()
                            )),
                        )],
                    }]
                },
            ),
        },
    )
}

/// Ensures that at least one ink! message.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L329>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L123-L143>.
fn ensure_contains_message(contract: &Contract) -> Option<Diagnostic> {
    utils::ensure_at_least_one_item(
        contract.messages(),
        Diagnostic {
            message: "At least one ink! message must be defined for an ink! contract.".to_string(),
            range: contract.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: analysis_utils::callable_insert_offset_indent_and_affixes(contract).map(
                |(insert_offset, indent, prefix, suffix)| {
                    // Adds an ink! message to the first non-trait `impl` block or creates a new `impl` block if necessary.
                    vec![Action {
                        label: "Add ink! message `fn`.".to_string(),
                        range: TextRange::new(insert_offset, insert_offset),
                        edits: vec![TextEdit::insert_with_snippet(
                            format!(
                                "{}{}{}",
                                prefix.as_deref().unwrap_or_default(),
                                analysis_utils::apply_indenting(MESSAGE_PLAIN, &indent),
                                suffix.as_deref().unwrap_or_default()
                            ),
                            insert_offset,
                            Some(format!(
                                "{}{}{}",
                                prefix.as_deref().unwrap_or_default(),
                                analysis_utils::apply_indenting(MESSAGE_SNIPPET, &indent),
                                suffix.as_deref().unwrap_or_default()
                            )),
                        )],
                    }]
                },
            ),
        },
    )
}

/// Returns composed selectors for a list of ink! callable entities.
fn get_composed_selectors<T>(items: &[T]) -> Vec<(Selector, SyntaxNode, Option<SelectorArg>)>
where
    T: IsInkCallable,
{
    items
        .iter()
        .filter_map(|item| {
            item.composed_selector()
                .map(|selector| (selector, item.syntax().clone(), item.selector_arg()))
        })
        .collect()
}

/// Ensures that no ink! message or constructor selectors are overlapping.
///
/// Overlaps between ink! constructor and message selectors are allowed.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L331>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L167-L240>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L336-L337>.
fn ensure_no_overlapping_selectors(results: &mut Vec<Diagnostic>, contract: &Contract) {
    for (selectors, name, mut unavailable_ids) in [
        (
            get_composed_selectors(contract.constructors()),
            "constructor",
            contract
                .constructors()
                .iter()
                .filter_map(|it| it.composed_selector().map(Selector::into_be_u32))
                .collect::<HashSet<u32>>(),
        ),
        (
            get_composed_selectors(contract.messages()),
            "message",
            contract
                .messages()
                .iter()
                .filter_map(|it| it.composed_selector().map(Selector::into_be_u32))
                .collect::<HashSet<u32>>(),
        ),
    ] {
        let mut seen_selectors: HashSet<u32> = HashSet::new();
        for (idx, (selector, node, selector_arg)) in selectors.iter().enumerate() {
            let selector_value = selector.into_be_u32();

            if seen_selectors.get(&selector_value).is_some() {
                // Determines text range for the argument value.
                let value_range_option = selector_arg
                    .as_ref()
                    .map(SelectorArg::arg)
                    .and_then(InkArg::value)
                    .map(MetaValue::text_range);
                results.push(Diagnostic {
                    message: format!("Selector values must be unique across all ink! {name}s in an ink! contract."),
                    range: value_range_option.unwrap_or(node.text_range()),
                    severity: Severity::Error,
                    quickfixes: value_range_option.map(|range| {
                        let suggested_id = analysis_utils::suggest_unique_id(
                            Some(idx as u32 + 1),
                            &mut unavailable_ids,
                        );
                        vec![Action {
                            label: "Replace with a unique selector.".to_string(),
                            range,
                            edits: vec![TextEdit::replace_with_snippet(
                                format!("{suggested_id}"),
                                range,
                                Some(format!("${{1:{suggested_id}}}")),
                            )],
                        }]
                    }),
                });
            }

            seen_selectors.insert(selector_value);
        }
    }
}

/// Returns all ink! selector arguments for a list of ink! callable entities.
fn get_selector_args<T>(items: &[T]) -> Vec<SelectorArg>
where
    T: IsInkEntity,
{
    items
        .iter()
        .flat_map(|item| {
            item.tree()
                .ink_args_by_kind(InkArgKind::Selector)
                .filter_map(SelectorArg::cast)
        })
        .collect()
}

/// Ensures that at most one wildcard selector exists among ink! messages, as well as ink! constructors.
///
/// At most one wildcard is allowed for each group
/// (i.e a single message and a single constructor each with a wildcard selector is a valid configuration).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L332>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L242-L293>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L336-L337>.
fn ensure_at_most_one_wildcard_selector(results: &mut Vec<Diagnostic>, contract: &Contract) {
    for (selectors, name) in [
        (get_selector_args(contract.constructors()), "constructor"),
        (get_selector_args(contract.messages()), "message"),
    ] {
        let mut has_seen_wildcard = false;
        for selector in selectors {
            if selector.is_wildcard() {
                if has_seen_wildcard {
                    // Edit range for quickfix.
                    let range =
                        analysis_utils::ink_arg_and_delimiter_removal_range(selector.arg(), None);
                    results.push(Diagnostic {
                        message: format!("At most one wildcard (`_`) selector can be defined across all ink! {name}s in an ink! contract."),
                        range: selector.text_range(),
                        severity: Severity::Error,
                        quickfixes: Some(vec![Action {
                            label: "Remove wildcard selector.".to_string(),
                            range,
                            edits: vec![TextEdit::delete(range)],
                        }]),
                    });
                } else {
                    has_seen_wildcard = true;
                }
            }
        }
    }
}

/// Ensures that item is defined in the root of this specific ink! contract.
fn ensure_parent_contract<T>(
    contract: &Contract,
    item: &T,
    ink_scope_name: &str,
) -> Option<Diagnostic>
where
    T: FromSyntax,
{
    let is_parent = match ink_analyzer_ir::ink_parent::<Contract>(item.syntax()) {
        Some(parent_contract) => parent_contract.syntax() == contract.syntax(),
        None => false,
    };

    (!is_parent).then_some(Diagnostic {
        message: format!(
            "ink! {ink_scope_name}s must be defined in the root of the ink! contract's `mod` item."
        ),
        range: item.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: contract
            .module()
            .and_then(ast::Module::item_list)
            .map(|item_list| {
                // Moves the item to the root of the ink! contract's `mod` item.
                vec![Action::move_item(
                    item.syntax(),
                    analysis_utils::item_insert_offset_by_scope_name(&item_list, ink_scope_name),
                    "Move item to the root of the ink! contract's `mod` item.".to_string(),
                    Some(analysis_utils::item_children_indenting(contract.syntax()).as_str()),
                )]
            }),
    })
}

/// Ensures that ink! storage, ink! events and ink! impls are defined in the root of the ink! contract.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L377-L379>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L28-L29>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L74>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L475>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L79>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L88-L97>.
fn ensure_root_items(results: &mut Vec<Diagnostic>, contract: &Contract) {
    ink_analyzer_ir::ink_closest_descendants::<Storage>(contract.syntax()) // All storage definitions.
        .filter_map(|item| ensure_parent_contract(contract, &item, "storage"))
        .chain(
            contract
                .events()
                .iter()
                .filter_map(|item| ensure_parent_contract(contract, item, "event")),
        )
        .chain(
            contract
                .impls()
                .iter()
                .filter_map(|item| ensure_parent_contract(contract, item, "impl")),
        )
        .for_each(|diagnostic| results.push(diagnostic));
}

/// Ensures that ink! messages and constructors are defined in the root of an `impl` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L36-L66>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L66-L96>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/impl_item.rs#L64-L87>.
fn ensure_impl_parent_for_callables(results: &mut Vec<Diagnostic>, contract: &Contract) {
    for diagnostic in contract
        .constructors()
        .iter()
        .filter_map(|item| utils::ensure_impl_parent(item, "constructor"))
        .chain(
            contract
                .messages()
                .iter()
                .filter_map(|item| utils::ensure_impl_parent(item, "message")),
        )
    {
        results.push(diagnostic)
    }
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L98-L114>.
fn ensure_valid_quasi_direct_ink_descendants(results: &mut Vec<Diagnostic>, contract: &Contract) {
    utils::ensure_valid_quasi_direct_ink_descendants(results, contract, |attr| {
        matches!(
            attr.kind(),
            InkAttributeKind::Arg(
                InkArgKind::Storage
                    | InkArgKind::Event
                    | InkArgKind::Anonymous
                    | InkArgKind::Impl
                    | InkArgKind::Namespace
                    | InkArgKind::Constructor
                    | InkArgKind::Message
                    | InkArgKind::Payable
                    | InkArgKind::Default
                    | InkArgKind::Selector
            ) | InkAttributeKind::Macro(
                InkMacroKind::ChainExtension
                    | InkMacroKind::StorageItem
                    | InkMacroKind::Test
                    | InkMacroKind::TraitDefinition
                    | InkMacroKind::E2ETest
            )
        )
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::verify_actions;
    use ink_analyzer_ir::syntax::TextSize;
    use ink_analyzer_ir::InkFile;
    use quote::{format_ident, quote};
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_contract(code: &str) -> Contract {
        InkFile::parse(code).contracts().to_owned()[0].clone()
    }

    // List of valid minimal ink! contracts used for positive(`works`) tests for ink! contract verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L593-L640>.
    macro_rules! valid_contracts {
        () => {
            [
                // Minimal.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        impl Minimal {
                            #[ink(constructor)]
                            pub fn new() -> Self {}

                            #[ink(message)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
                // Minimal + Mutable message receiver.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        impl Minimal {
                            #[ink(constructor)]
                            pub fn new() -> Self {}

                            #[ink(message)]
                            pub fn minimal_message(&self) {}

                            #[ink(message)]
                            pub fn minimal_message_mut(&mut self) {}
                        }
                    }
                },
                // Minimal + Event + Args.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default, selector=1)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default, selector=1)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default, selector=0x1)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default, selector=0x1)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default, selector=_)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default, selector=_)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
                // Minimal + Event + Multiple Selectors.
                // Overlaps between constructors and messages are ok.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L838-L857>.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default, selector=1)]
                            pub fn new() -> Self {}

                            #[ink(constructor, payable, default, selector=2)]
                            pub fn new2() -> Self {}

                            #[ink(message, payable, default, selector=1)]
                            pub fn minimal_message(&self) {}

                            #[ink(message, payable, default, selector=2)]
                            pub fn minimal_message2(&self) {}
                        }
                    }
                },
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default, selector=0x1)]
                            pub fn new() -> Self {}

                            #[ink(constructor, payable, default, selector=0x2)]
                            pub fn new2() -> Self {}

                            #[ink(message, payable, default, selector=0x1)]
                            pub fn minimal_message(&self) {}

                            #[ink(message, payable, default, selector=0x2)]
                            pub fn minimal_message2(&self) {}
                        }
                    }
                },
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new() -> Self {}

                            #[ink(constructor, payable, default, selector=_)]
                            pub fn new2() -> Self {}

                            #[ink(constructor, payable, default, selector=3)]
                            pub fn new3() -> Self {}

                            #[ink(constructor, payable, default, selector=0x4)]
                            pub fn new4() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message(&self) {}

                            #[ink(message, payable, default, selector=_)]
                            pub fn minimal_message2(&self) {}

                            #[ink(message, payable, default, selector=3)]
                            pub fn minimal_message3(&self) {}

                            #[ink(message, payable, default, selector=0x4)]
                            pub fn minimal_message4(&self) {}
                        }
                    }
                },
                // Minimal + Event + Multiple Selectors + Traits + Namespace + Impl attribute.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message(&self) {}

                            #[ink(constructor, payable, default, selector=_)]
                            pub fn new2() -> Self {}

                            #[ink(message, payable, default, selector=_)]
                            pub fn minimal_message2(&self) {}

                            #[ink(constructor, payable, default, selector=3)]
                            pub fn new3() -> Self {}

                            #[ink(constructor, payable, default, selector=0x4)]
                            pub fn new4() -> Self {}

                            #[ink(message, payable, default, selector=3)]
                            pub fn minimal_message3(&self) {}

                            #[ink(message, payable, default, selector=0x4)]
                            pub fn minimal_message4(&self) {}
                        }

                        impl MyTrait for Minimal {
                            #[ink(constructor, payable, default)]
                            fn new5() -> Self {}

                            #[ink(message, payable, default)]
                            fn minimal_message5(&self) {}
                        }

                        impl ::my_full::long_path::MyTrait for Minimal {
                            #[ink(constructor, payable, default)]
                            fn new6() -> Self {}

                            #[ink(message, payable, default)]
                            fn minimal_message6(&self) {}
                        }

                        impl relative_path::MyTrait for Minimal {
                            #[ink(constructor, payable, default)]
                            fn new7() -> Self {}

                            #[ink(message, payable, default)]
                            fn minimal_message7(&self) {}
                        }

                        #[ink(namespace="my_namespace")]
                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new8() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message8(&self) {}
                        }

                        #[ink(impl)]
                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new9() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message9(&self) {}
                        }

                        #[ink(impl, namespace="my_namespace")]
                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new10() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message10(&self) {}
                        }

                        // An ink! impl with no ink! constructors or ink! messages is valid
                        // as long as it has an ink! impl annotation.
                        // Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/tests.rs#L212-L215>.
                        #[ink(impl)]
                        impl Minimal {

                        }
                    }
                },
                // Minimal + Tests.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        impl Minimal {
                            #[ink(constructor)]
                            pub fn new() -> Self {}

                            #[ink(message)]
                            pub fn minimal_message(&self) {}
                        }

                        #[cfg(test)]
                        mod tests {
                            #[ink::test]
                            fn it_works() {
                            }
                        }
                    }
                },
                // Flipper.
                quote! {
                    mod flipper {
                        #[ink(storage)]
                        pub struct Flipper {
                            value: bool,
                        }

                        impl Default for Flipper {
                            #[ink(constructor)]
                            fn default() -> Self {
                                Self { value: false }
                            }
                        }

                        impl Flipper {
                            #[ink(message)]
                            pub fn flip(&mut self) {
                                self.value = !self.value
                            }

                            #[ink(message)]
                            pub fn get(&self) -> bool {
                                self.value
                            }
                        }
                    }
                },
                // Flipper + Event + Args.
                quote! {
                    mod flipper {
                        #[ink(storage)]
                        pub struct Flipper {
                            value: bool,
                        }

                        #[ink(event, anonymous)]
                        pub struct Flip {
                            #[ink(topic)]
                            flipped: bool,
                        }

                        impl Default for Flipper {
                            #[ink(constructor, payable, default, selector=1)]
                            fn default() -> Self {
                                Self { value: false }
                            }
                        }

                        impl Flipper {
                            #[ink(message, payable, default, selector=1)]
                            pub fn flip(&mut self) {
                                self.value = !self.value
                            }

                            #[ink(message, selector=2)]
                            pub fn get(&self) -> bool {
                                self.value
                            }
                        }
                    }
                },
            ]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #[ink::contract]
                        #code
                    },
                    // Env.
                    quote! {
                        #[ink::contract(env=my::env::Types)]
                        #code
                    },
                    // Keep Attr.
                    quote! {
                        #[ink::contract(keep_attr="foo,bar")]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                        #code
                    },
                ]
            })
        };
    }

    #[test]
    fn inline_mod_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let result = ensure_inline_module(&contract);
            assert!(result.is_none(), "contract: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L724-L732>.
    fn out_of_line_mod_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract]
            mod my_contract;
        };
        let contract = parse_first_contract(&code);

        let result = ensure_inline_module(&contract);

        // Verifies diagnostics.
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
        // Verifies quickfixes.
        let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
        assert!(fix.label.contains("inline body"));
        assert!(&fix.edits[0].text.contains("{}"));
        assert_eq!(
            fix.edits[0].range,
            TextRange::new(
                TextSize::from(parse_offset_at(&code, Some("<-;")).unwrap() as u32),
                TextSize::from(parse_offset_at(&code, Some(";")).unwrap() as u32)
            )
        );
    }

    #[test]
    fn non_mod_fails() {
        for code in [
            quote! {
                fn my_contract() {
                }
            },
            quote! {
                struct MyContract {}
            },
            quote! {
                enum MyContract {
                }
            },
            quote! {
                trait MyContract {
                }
            },
        ] {
            let code = quote_as_pretty_string! {
                #[ink::contract]
                #code
            };
            let contract = parse_first_contract(&code);

            let result = ensure_inline_module(&contract);

            // Verifies diagnostics.
            assert!(result.is_some(), "contract: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "contract: {code}"
            );
            // Verifies quickfixes.
            let expected_quickfixes = vec![
                TestResultAction {
                    label: "Remove `#[ink::contract]`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                    }],
                },
                TestResultAction {
                    label: "Remove item",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("}"),
                    }],
                },
            ];
            let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn one_storage_item_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_storage_quantity(&mut results, &contract);
            assert!(results.is_empty(), "contract: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L667-L686>.
    fn missing_storage_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract]
            mod my_contract {
            }
        };
        let contract = parse_first_contract(&code);

        let mut results = Vec::new();
        ensure_storage_quantity(&mut results, &contract);

        // Verifies diagnostics.
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
        // Verifies quickfixes.
        let expected_quickfixes = vec![TestResultAction {
            label: "Add",
            edits: vec![TestResultTextRange {
                text: "#[ink(storage)]",
                start_pat: Some("mod my_contract {"),
                end_pat: Some("mod my_contract {"),
            }],
        }];
        let quickfixes = results[0].quickfixes.as_ref().unwrap();
        verify_actions(&code, quickfixes, &expected_quickfixes);
    }

    #[test]
    fn multiple_storage_items_fails() {
        // Tests snippets with btn 2 and 5 storage definitions.
        for idx in 2..=5 {
            // Creates multiple storage definitions.
            let storage_items = (1..=idx).map(|i| {
                let name = format_ident!("MyContract{}", i);
                quote! {
                    #[ink(storage)]
                    pub struct #name {
                    }
                }
            });

            // Creates contract with multiple storage definitions.
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    #( #storage_items )*
                }
            });

            let mut results = Vec::new();
            ensure_storage_quantity(&mut results, &contract);
            // There should be `idx-1` extraneous storage definitions.
            assert_eq!(results.len(), idx - 1);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                idx - 1
            );
            // All quickfixes should be for removal.
            for item in results {
                let fix = &item.quickfixes.as_ref().unwrap()[0];
                assert!(fix.label.contains("Remove"));
                for edit in &fix.edits {
                    assert_eq!(&edit.text, "");
                }
            }
        }
    }

    #[test]
    fn one_or_multiple_constructors_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let result = ensure_contains_constructor(&contract);
            assert!(result.is_none(), "contract: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L688-L704>.
    fn missing_constructor_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract]
            mod my_contract {
            }
        };
        let contract = parse_first_contract(&code);

        let result = ensure_contains_constructor(&contract);

        // Verifies diagnostics.
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
        // Verifies quickfixes.
        let expected_quickfixes = vec![TestResultAction {
            label: "Add",
            edits: vec![TestResultTextRange {
                text: "#[ink(constructor)]",
                start_pat: Some("mod my_contract {"),
                end_pat: Some("mod my_contract {"),
            }],
        }];
        let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
        verify_actions(&code, quickfixes, &expected_quickfixes);
    }

    #[test]
    fn one_or_multiple_messages_works() {
        // Tests snippets with btn 2 and 5 messages.
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let result = ensure_contains_message(&contract);
            assert!(result.is_none(), "contract: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L706-L722>.
    fn missing_message_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract]
            mod my_contract {
            }
        };
        let contract = parse_first_contract(&code);

        let result = ensure_contains_message(&contract);

        // Verifies diagnostics.
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
        // Verifies quickfixes.
        let expected_quickfixes = vec![TestResultAction {
            label: "Add",
            edits: vec![TestResultTextRange {
                text: "#[ink(message)]",
                start_pat: Some("mod my_contract {"),
                end_pat: Some("mod my_contract {"),
            }],
        }];
        let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
        verify_actions(&code, quickfixes, &expected_quickfixes);
    }

    #[test]
    fn non_overlapping_selectors_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_no_overlapping_selectors(&mut results, &contract);
            assert!(results.is_empty(), "contract: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L754-L780>
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L782-L808>
    fn overlapping_selectors_fails() {
        for code in [
            // Overlapping decimal.
            quote! {
                #[ink(constructor, selector=1)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=1)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=2)]
                pub fn my_message(&mut self) {
                }

                #[ink(message, selector=2)]
                pub fn my_message2(&mut self) {
                }
            },
            // Overlapping hexadecimal.
            quote! {
                #[ink(constructor, selector=0xA)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=0xA)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=0xB)]
                pub fn my_message(&mut self) {
                }

                #[ink(message, selector=0xB)]
                pub fn my_message2(&mut self) {
                }
            },
            // Overlapping detected across decimal and hex representations.
            quote! {
                #[ink(constructor, selector=10)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=0xA)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=11)]
                pub fn my_message(&mut self) {
                }

                #[ink(message, selector=0xB)]
                pub fn my_message2(&mut self) {
                }
            },
        ]
        .iter()
        .map(|item| {
            quote! {
                impl MyContract {
                    #item
                }
            }
        })
        .chain([
            // Overlapping trait implementations.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L810-L836>.
            quote! {
                impl first::MyTrait for MyContract {
                    #[ink(constructor)]
                    fn my_constructor() -> Self {}

                    #[ink(message)]
                    fn my_message(&self) {}
                }

                impl second::MyTrait for MyContract {
                    #[ink(constructor)]
                    fn my_constructor() -> Self {}

                    #[ink(message)]
                    fn my_message(&self) {}
                }
            },
        ]) {
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    #code
                }
            });

            let mut results = Vec::new();
            ensure_no_overlapping_selectors(&mut results, &contract);
            // 2 errors, 1 each for constructors and messages (i.e `my_constructor2` and `my_message2` are the overlapping selectors).
            assert_eq!(results.len(), 2);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                2
            );
            // Verifies quickfixes.
            if let Some(quickfixes) = &results[0].quickfixes {
                for fix in quickfixes {
                    assert!(fix.label.contains("Replace") && fix.label.contains("unique selector"));
                }
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L883-L902>.
    fn one_or_no_wildcard_selectors_works() {
        for code in valid_contracts!() {
            // At most one wildcard is allowed for each group i.e there can be messages and constructors
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_at_most_one_wildcard_selector(&mut results, &contract);
            assert!(results.is_empty(), "contract: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L859-L881>.
    fn multiple_wildcard_selectors_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(constructor, selector = _)]
                    pub fn my_constructor() -> Self {
                    }

                    #[ink(constructor, selector = _)]
                    pub fn my_constructor2() -> Self {
                    }

                    #[ink(message, selector = _)]
                    pub fn my_message(&mut self) {
                    }

                    #[ink(message, selector = _)]
                    pub fn my_message2(&mut self) {
                    }
                }
            }
        };
        let contract = parse_first_contract(&code);

        let mut results = Vec::new();
        ensure_at_most_one_wildcard_selector(&mut results, &contract);
        // 2 errors, 1 each for constructors and messages (i.e `my_constructor2` and `my_message2` are the extraneous wildcard selectors).
        assert_eq!(results.len(), 2);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            2
        );
        // Verifies quickfixes.
        // FIXME: Should also remove leading comma.
        let expected_quickfixes = vec![
            vec![TestResultAction {
                label: "Remove wildcard",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-selector = _)]\n        pub fn my_constructor2"),
                    end_pat: Some("<-)]\n        pub fn my_constructor2"),
                }],
            }],
            vec![TestResultAction {
                label: "Remove wildcard",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-selector = _)]\n        pub fn my_message2"),
                    end_pat: Some("<-)]\n        pub fn my_message2"),
                }],
            }],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    fn impl_parent_for_callables_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_impl_parent_for_callables(&mut results, &contract);
            assert!(results.is_empty(), "contract: {code}");
        }
    }

    #[test]
    fn non_impl_parent_for_callables_fails() {
        for (items, expected_quickfixes) in [
            (
                quote! {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {}

                    #[ink(message)]
                    pub fn my_message() {}

                    impl MyContract {}
                },
                vec![
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            TestResultTextRange {
                                text: "#[ink(constructor)]",
                                start_pat: Some("impl MyContract {"),
                                end_pat: Some("impl MyContract {"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(constructor)]"),
                                end_pat: Some("pub fn my_constructor() -> Self {}"),
                            },
                        ],
                    }],
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("impl MyContract {"),
                                end_pat: Some("impl MyContract {"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(message)]"),
                                end_pat: Some("pub fn my_message() {}"),
                            },
                        ],
                    }],
                ],
            ),
            (
                quote! {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {}

                    #[ink(message)]
                    pub fn my_message() {}
                },
                vec![
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            TestResultTextRange {
                                text: "#[ink(constructor)]",
                                start_pat: Some("<-}->"),
                                end_pat: Some("<-}->"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(constructor)]"),
                                end_pat: Some("pub fn my_constructor() -> Self {}"),
                            },
                        ],
                    }],
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("<-}->"),
                                end_pat: Some("<-}->"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(message)]"),
                                end_pat: Some("pub fn my_message() {}"),
                            },
                        ],
                    }],
                ],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::contract]
                mod my_contract {
                    #items
                }
            };
            let contract = parse_first_contract(&code);

            let mut results = Vec::new();
            ensure_impl_parent_for_callables(&mut results, &contract);

            // There should be 2 errors (i.e for the `constructor` and `message`).
            assert_eq!(results.len(), 2);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                2
            );
            // Verifies quickfixes.
            for (idx, item) in results.iter().enumerate() {
                let quickfixes = item.quickfixes.as_ref().unwrap();
                verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
            }
        }
    }

    #[test]
    fn root_items_in_root_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_root_items(&mut results, &contract);
            assert!(results.is_empty(), "contract: {code}");
        }
    }

    #[test]
    fn root_items_not_in_root_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract]
            mod my_contract {
                fn my_contract_container() {
                    #[ink(storage)]
                    pub struct MyContract {}

                    #[ink(event)]
                    pub struct MyEvent {}

                    impl MyContract {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {
                        }

                        #[ink(message)]
                        pub fn my_message() {}
                    }

                    #[ink(impl)]
                    impl MyContract {}
                }
            }
        };
        let contract = parse_first_contract(&code);

        let mut results = Vec::new();
        ensure_root_items(&mut results, &contract);

        // There should be 4 errors (i.e `storage`, `contract` and one for each of the 2 impls).
        assert_eq!(results.len(), 4);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            4
        );
        // Verifies quickfixes.
        let expected_quickfixes = vec![
            vec![TestResultAction {
                label: "Move item",
                edits: vec![
                    TestResultTextRange {
                        text: "#[ink(storage)]",
                        start_pat: Some("mod my_contract {"),
                        end_pat: Some("mod my_contract {"),
                    },
                    TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("pub struct MyContract {}"),
                    },
                ],
            }],
            vec![TestResultAction {
                label: "Move item",
                edits: vec![
                    TestResultTextRange {
                        text: "#[ink(event)]",
                        start_pat: Some("mod my_contract {"),
                        end_pat: Some("mod my_contract {"),
                    },
                    TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("pub struct MyEvent {}"),
                    },
                ],
            }],
            vec![TestResultAction {
                label: "Move item",
                edits: vec![
                    TestResultTextRange {
                        text: "#[ink(constructor)]",
                        start_pat: Some("<-}->"),
                        end_pat: Some("<-}->"),
                    },
                    TestResultTextRange {
                        text: "",
                        start_pat: Some("<-impl MyContract {"),
                        end_pat: Some("pub fn my_message() {}\n        }"),
                    },
                ],
            }],
            vec![TestResultAction {
                label: "Move item",
                edits: vec![
                    TestResultTextRange {
                        text: "#[ink(impl)]",
                        start_pat: Some("<-}->"),
                        end_pat: Some("<-}->"),
                    },
                    TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(impl)]"),
                        end_pat: Some("impl MyContract {}"),
                    },
                ],
            }],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &contract);
            assert!(results.is_empty(), "contract: {code}");
        }
    }

    #[test]
    fn invalid_quasi_direct_descendant_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract]
            mod my_contract {
                pub struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }

                #[ink(extension = 1, handle_status = false)]
                fn my_extension();
            }
        };
        let contract = parse_first_contract(&code);

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &contract);

        // There should be 2 errors (i.e `topic`, `extension/handle_status`).
        assert_eq!(results.len(), 2);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            2
        );
        // Verifies quickfixes.
        let expected_quickfixes = vec![
            vec![TestResultAction {
                label: "Remove `#[ink(topic)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[ink(topic)]"),
                    end_pat: Some("#[ink(topic)]"),
                }],
            }],
            vec![
                TestResultAction {
                    label: "Remove `#[ink(extension = 1, handle_status = false)]`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(extension = 1, handle_status = false)]"),
                        end_pat: Some("#[ink(extension = 1, handle_status = false)]"),
                    }],
                },
                TestResultAction {
                    label: "Remove item",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(extension = 1, handle_status = false)]"),
                        end_pat: Some("fn my_extension();"),
                    }],
                },
            ],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L593-L640>.
    fn compound_diagnostic_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &contract);
            assert!(results.is_empty(), "contract: {code}");
        }
    }
}
