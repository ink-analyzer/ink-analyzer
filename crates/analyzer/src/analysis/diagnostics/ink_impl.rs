//! ink! impl diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasName, HasVisibility, Trait};
use ink_analyzer_ir::syntax::{SyntaxNode, TextRange};
use ink_analyzer_ir::{
    ast, HasInkImplParent, InkArg, InkArgKind, InkArgValueKind, InkAttributeKind, InkEntity,
    InkImpl, IsInkFn, IsInkTrait, Message,
};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::iter;

use super::{constructor, message, utils};
use crate::analysis::actions::entity as entity_actions;
use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils as analysis_utils;
use crate::{Action, ActionKind, Diagnostic, Severity, Version};

const SCOPE_NAME: &str = "impl";

/// Runs all ink! impl diagnostics.
///
/// The entry point for finding ink! impl semantic rules is the `item_impl` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221-L334>.
pub fn diagnostics(
    results: &mut Vec<Diagnostic>,
    ink_impl: &InkImpl,
    version: Version,
    skip_callable_diagnostics: bool,
) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, ink_impl, version);

    // Ensures that ink! impl is an `impl` item, see `ensure_impl` doc.
    if let Some(diagnostic) = ensure_impl(ink_impl) {
        results.push(diagnostic);
    }

    // Ensures that `impl` item satisfies all invariants of an ink! impl,
    // see `ensure_impl_invariants` doc.
    ensure_impl_invariants(results, ink_impl);

    // Ensures that impl block either has an ink! impl annotation or
    // contains at least one ink! constructor or ink! message, see `ensure_contains_callable` doc.
    if let Some(diagnostic) = ensure_annotation_or_contains_callable(ink_impl) {
        results.push(diagnostic);
    }

    if !skip_callable_diagnostics {
        // Runs ink! constructor diagnostics, see `constructor::diagnostics` doc.
        for item in ink_impl.constructors() {
            constructor::diagnostics(results, item, version);
        }

        // Runs ink! message diagnostics, see `message::diagnostics` doc.
        for item in ink_impl.messages() {
            message::diagnostics(results, item, version);
        }
    }

    // Ensures that ink! messages and constructors are defined in the root of an `impl` item,
    // see `ensure_impl_parent_for_callables` doc.
    ensure_callables_in_root(results, ink_impl);

    // Ensures that ink! impl is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L88-L97>.
    if let Some(diagnostic) = utils::ensure_contract_parent(ink_impl, SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that `impl` satisfies all invariants of the ink! trait definition it implements (if any).
    ensure_trait_definition_impl_invariants(results, ink_impl, version);

    // Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // See `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, ink_impl, version);
}

/// Ensures that ink! impl is an `impl` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221>.
fn ensure_impl(ink_impl: &InkImpl) -> Option<Diagnostic> {
    ink_impl.impl_item().is_none().then(|| Diagnostic {
        message: "ink! impl must be an `impl` item.".to_owned(),
        range: analysis_utils::ink_impl_declaration_range(ink_impl),
        severity: Severity::Error,
        quickfixes: ink_impl
            .impl_attr()
            .map(|attr| vec![Action::remove_attribute(&attr)]),
    })
}

/// Ensures that `impl` satisfies all invariants of an ink! impl.
///
/// See references below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221-L334>.
pub fn ensure_impl_invariants(results: &mut Vec<Diagnostic>, ink_impl: &InkImpl) {
    if let Some(impl_item) = ink_impl.impl_item() {
        if let Some(default_token) = impl_item.default_token() {
            // Edit range for quickfix.
            let range = analysis_utils::token_and_trivia_range(&default_token);
            results.push(Diagnostic {
                message: "ink! impl must not be `default`.".to_owned(),
                range: default_token.text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Remove `default` keyword.".to_owned(),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::delete(range)],
                }]),
            });
        }

        if let Some(unsafe_token) = impl_item.unsafe_token() {
            // Edit range for quickfix.
            let range = analysis_utils::token_and_trivia_range(&unsafe_token);
            results.push(Diagnostic {
                message: "ink! impl must not be `unsafe`.".to_owned(),
                range: unsafe_token.text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Remove `unsafe` keyword.".to_owned(),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::delete(range)],
                }]),
            });
        }

        if let Some(diagnostic) = utils::ensure_no_generics(impl_item, SCOPE_NAME) {
            results.push(diagnostic);
        }

        if let Some(ast::Type::PathType(path_type)) = impl_item.self_ty() {
            if let Some(path) = path_type.path() {
                results.append(
                    &mut path
                        .segments()
                        .filter_map(|arg| {
                            arg.generic_arg_list().map(|generic_arg_list| Diagnostic {
                                message: "Generic types on an ink! impl are not supported."
                                    .to_owned(),
                                range: generic_arg_list.syntax().text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: "Remove generic types.".to_owned(),
                                    kind: ActionKind::QuickFix,
                                    range: generic_arg_list.syntax().text_range(),
                                    edits: vec![TextEdit::delete(
                                        generic_arg_list.syntax().text_range(),
                                    )],
                                }]),
                            })
                        })
                        .collect(),
                );
            }
        }

        if let Some((_, arg)) = ink_impl.trait_type().zip(ink_impl.namespace_arg()) {
            // Edit range for quickfix.
            let range = analysis_utils::ink_arg_and_delimiter_removal_range(&arg, None);
            results.push(Diagnostic {
                message: "ink! namespace argument is not allowed on trait ink! impl blocks."
                    .to_owned(),
                range: arg.text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Remove ink! namespace argument.".to_owned(),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::delete(range)],
                }]),
            });
        }

        let constructor_fns: Vec<&ast::Fn> = ink_impl
            .constructors()
            .iter()
            .filter_map(IsInkFn::fn_item)
            .collect();
        let message_fns: Vec<&ast::Fn> = ink_impl
            .messages()
            .iter()
            .filter_map(IsInkFn::fn_item)
            .collect();
        for (fns, name) in [(constructor_fns, "constructor"), (message_fns, "message")] {
            for fn_item in fns {
                if impl_item.trait_().is_some() {
                    // Callables must have inherent visibility for trait implementation blocks.
                    if let Some(visibility) = fn_item.visibility() {
                        // Edit range for quickfix.
                        let range = analysis_utils::node_and_trivia_range(visibility.syntax());
                        results.push(Diagnostic {
                            message: format!("ink! {name}s in trait ink! impl blocks must have inherited visibility."),
                            range: visibility.syntax().text_range(),
                            severity: Severity::Error,
                            quickfixes: Some(vec![Action {
                                label: format!("Remove visibility `{}`.", visibility.syntax()),
                                kind: ActionKind::QuickFix,
                                range,
                                edits: vec![TextEdit::delete(range)],
                            }]),
                        });
                    }
                } else {
                    // Callables must have `pub` visibility for inherent implementation blocks.
                    let (has_pub_visibility, visibility) = match fn_item.visibility() {
                        // Check `pub` visibility.
                        Some(visibility) => {
                            (visibility.syntax().to_string() == "pub", Some(visibility))
                        }
                        // Inherited visibility.
                        None => (false, None),
                    };

                    if !has_pub_visibility {
                        // Gets the declaration range for the `fn` item.
                        let fn_declaration_range = || {
                            analysis_utils::ast_item_declaration_range(&ast::Item::Fn(
                                fn_item.clone(),
                            ))
                            .unwrap_or(fn_item.syntax().text_range())
                        };
                        results.push(Diagnostic {
                            message: format!(
                                "ink! {name}s in inherent ink! impl blocks must have `pub` visibility."
                            ),
                            range: visibility
                                .as_ref()
                                .map(|it| it.syntax().text_range()).unwrap_or_else(fn_declaration_range),
                            severity: Severity::Error,
                            quickfixes: visibility
                                .as_ref()
                                .map(|vis| vis.syntax().text_range())
                                .or(fn_item
                                    .default_token()
                                    .or(fn_item.const_token())
                                    .or(fn_item.async_token())
                                    .or(fn_item.unsafe_token())
                                    .or(fn_item.abi().and_then(|abi| abi.syntax().first_token()))
                                    .or(fn_item.fn_token())
                                    .map(|it| {
                                        TextRange::new(
                                            it.text_range().start(),
                                            it.text_range().start(),
                                        )
                                    }))
                                .map(|range| {
                                    vec![Action {
                                        label: "Change visibility to `pub`.".to_owned(),
                                        kind: ActionKind::QuickFix,
                                        range: visibility
                                            .as_ref()
                                            .map(|it| {
                                                it.syntax().text_range()
                                            }).unwrap_or_else(fn_declaration_range),
                                        edits: vec![TextEdit::replace(
                                            format!(
                                                "pub{}",
                                                if visibility.is_none() { " " } else { "" }
                                            ),
                                            range,
                                        )],
                                    }]
                                }),
                        });
                    }
                }
            }
        }
    }
}

/// Ensures that impl block either has an ink! impl annotation or contains at least one ink! constructor or ink! message.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L119-L210>.
fn ensure_annotation_or_contains_callable(ink_impl: &InkImpl) -> Option<Diagnostic> {
    // Gets the declaration range for the item.
    let range = analysis_utils::ink_impl_declaration_range(ink_impl);
    (ink_impl.impl_attr().is_none()
        && ink_impl.constructors().is_empty()
        && ink_impl.messages().is_empty())
    .then(|| Diagnostic {
        message: "At least one ink! constructor or ink! message \
        must be defined for an ink! impl without an `#[ink(impl)]` annotation."
            .to_owned(),
        range,
        severity: Severity::Error,
        quickfixes: ink_impl.impl_item().as_ref().map(|impl_item| {
            // Adds ink! callables if possible.
            [
                entity_actions::add_constructor_to_impl(impl_item, ActionKind::QuickFix, None),
                entity_actions::add_message_to_impl(impl_item, ActionKind::QuickFix, None),
            ]
            .into_iter()
            .flatten()
            .collect()
        }),
    })
}

/// Ensures that item is defined in the root of this specific `impl` item.
fn ensure_parent_impl<T>(ink_impl: &InkImpl, item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: HasInkImplParent + IsInkFn,
{
    let is_parent = item
        .parent_impl_item()
        .is_some_and(|parent_impl_item| parent_impl_item.syntax() == ink_impl.syntax());

    (!is_parent).then(|| Diagnostic {
        message: format!(
            "ink! {ink_scope_name}s must be defined in the root of an ink! contract's `impl` block."
        ),
        range: analysis_utils::ink_impl_declaration_range(ink_impl),
        severity: Severity::Error,
        quickfixes: ink_impl
            .impl_item()
            .and_then(|it| it.assoc_item_list())
            .map(|assoc_item_list| {
                // Moves the item to the root of the `impl` block.
                vec![Action::move_item(
                    item.syntax(),
                    analysis_utils::assoc_item_insert_offset_end(&assoc_item_list),
                    "Move item to the root of the ink! contract's `impl` block.".to_owned(),
                    Some(analysis_utils::item_children_indenting(ink_impl.syntax()).as_str()),
                )]
            }),
    })
}

/// Ensures that ink! messages and constructors are defined in the root of the `impl` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L36-L66>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L66-L96>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/impl_item.rs#L64-L87>.
fn ensure_callables_in_root(results: &mut Vec<Diagnostic>, ink_impl: &InkImpl) {
    results.extend(
        ink_impl
            .constructors()
            .iter()
            .filter_map(|item| ensure_parent_impl(ink_impl, item, "constructor"))
            .chain(
                ink_impl
                    .messages()
                    .iter()
                    .filter_map(|item| ensure_parent_impl(ink_impl, item, "message")),
            ),
    );
}

/// Ensures that `impl` satisfies all invariants of the ink! trait definition it implements (if any).
///
/// (i.e. ink! trait definition `impl` block must implement all associated methods (with expected signatures)
/// and have no other associated items).
fn ensure_trait_definition_impl_invariants(
    results: &mut Vec<Diagnostic>,
    ink_impl: &InkImpl,
    version: Version,
) {
    if let Some((impl_item, trait_definition)) =
        ink_impl.impl_item().zip(ink_impl.trait_definition())
    {
        // Maps message name to declaration.
        let message_index: HashMap<String, &Message> = trait_definition
            .messages()
            .iter()
            .filter_map(|message| {
                message
                    .fn_item()
                    .and_then(HasName::name)
                    .as_ref()
                    .map(ToString::to_string)
                    .and_then(|name| (!name.is_empty()).then_some((name, message)))
            })
            .collect();
        let method_names = trait_definition
            .trait_item()
            .and_then(Trait::assoc_item_list)
            .map(|item_list| {
                item_list
                    .assoc_items()
                    .filter_map(|item| match item {
                        ast::AssocItem::Fn(fn_item) => {
                            fn_item.name().as_ref().map(ToString::to_string)
                        }
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or(HashSet::new());
        let mut seen_messages: HashSet<String> = HashSet::new();

        if let Some(assoc_item_list) = impl_item.assoc_item_list() {
            for item in assoc_item_list.assoc_items() {
                match &item {
                    // Handles associated methods.
                    ast::AssocItem::Fn(fn_item) => {
                        if let Some(fn_name) = fn_item.name() {
                            let fn_name_text = fn_name.to_string();
                            match message_index.get(&fn_name_text) {
                                // Handles unexpected methods.
                                None => {
                                    if !method_names.contains(&fn_name_text) {
                                        results.push(Diagnostic {
                                            message: format!("A `{fn_name_text}` method isn't declared in the ink! trait definition."),
                                            range: item.syntax().text_range(),
                                            severity: Severity::Error,
                                            quickfixes: Some(vec![Action::remove_item(item.syntax())]),
                                        });
                                    }
                                }
                                // Verifies that method signatures and ink! attribute arguments match the declaration.
                                Some(message_declaration) => {
                                    seen_messages.insert(fn_name_text);

                                    // Verifies that `fn` item has attributes that match the equivalent ink! trait definition method.
                                    ensure_trait_definition_impl_message_args(
                                        results,
                                        fn_item,
                                        message_declaration,
                                        version,
                                    );

                                    if let Some(fn_declaration) = message_declaration.fn_item() {
                                        // Verifies that param list matches the declaration.
                                        let diagnostic_range = fn_item
                                            .param_list()
                                            .map(|it| it.syntax().text_range())
                                            .unwrap_or_else(|| fn_name.syntax().text_range());
                                        let replace_range = fn_item
                                            .param_list()
                                            .map(|it| it.syntax().text_range())
                                            .unwrap_or_else(|| {
                                                TextRange::new(
                                                    fn_name.syntax().text_range().end(),
                                                    fn_name.syntax().text_range().end(),
                                                )
                                            });
                                        verify_signature_part_match(
                                            results,
                                            fn_declaration
                                                .param_list()
                                                .as_ref()
                                                .map(|it| it.syntax()),
                                            fn_item.param_list().as_ref().map(|it| it.syntax()),
                                            diagnostic_range,
                                            replace_range,
                                            "parameter list",
                                            Some("parameters"),
                                        );

                                        // Verifies that return type matches the declaration.
                                        let fallback_insert_offset = fn_item
                                            .param_list()
                                            .map(|it| it.syntax().text_range())
                                            .unwrap_or_else(|| fn_name.syntax().text_range())
                                            .end();
                                        let diagnostic_range = fn_item
                                            .ret_type()
                                            .map(|it| it.syntax().text_range())
                                            .unwrap_or_else(|| {
                                                TextRange::new(
                                                    fn_name.syntax().text_range().start(),
                                                    fallback_insert_offset,
                                                )
                                            });
                                        let replace_range = fn_item
                                            .ret_type()
                                            .map(|it| it.syntax().text_range())
                                            .unwrap_or_else(|| {
                                                TextRange::new(
                                                    fallback_insert_offset,
                                                    fallback_insert_offset,
                                                )
                                            });
                                        verify_signature_part_match(
                                            results,
                                            fn_declaration
                                                .ret_type()
                                                .as_ref()
                                                .map(|it| it.syntax()),
                                            fn_item.ret_type().as_ref().map(|it| it.syntax()),
                                            diagnostic_range,
                                            replace_range,
                                            "return type",
                                            None,
                                        );
                                    }
                                }
                            }
                        }
                    }
                    // Handles all other associated item types.
                    _ => {
                        results.push(Diagnostic {
                            message: "An ink! trait definition's implementation \
                            can only contain ink! messages."
                                .to_owned(),
                            range: item.syntax().text_range(),
                            severity: Severity::Error,
                            quickfixes: Some(vec![Action::remove_item(item.syntax())]),
                        });
                    }
                }
            }
        }

        // Computes diagnostic for missing messages (if any).
        let mut missing_messages = message_index.values().filter_map(|message| {
            // Filters missing messages (with valid names).
            message.fn_item().and_then(|fn_item| {
                fn_item
                    .name()
                    .is_some_and(|name| {
                        let name_text = name.to_string();
                        !name_text.is_empty() && !seen_messages.contains(&name_text)
                    })
                    .then_some(fn_item)
            })
        });
        if let Some(fn_item) = missing_messages.next() {
            let (insert_offset, indent_option, prefix_option, suffix_option) = impl_item
                .assoc_item_list()
                .map(|assoc_item_list| {
                    (
                        analysis_utils::assoc_item_insert_offset_end(&assoc_item_list),
                        Some(analysis_utils::item_children_indenting(impl_item.syntax())),
                        None,
                        None,
                    )
                })
                .unwrap_or_else(|| {
                    (
                        impl_item.syntax().text_range().end(),
                        Some(analysis_utils::item_children_indenting(impl_item.syntax())),
                        Some(" {"),
                        Some(format!(
                            "{}}}",
                            analysis_utils::item_indenting(impl_item.syntax())
                                .as_deref()
                                .unwrap_or_default()
                        )),
                    )
                });
            let range = analysis_utils::ink_impl_declaration_range(ink_impl);
            let mut edit_option = None;
            let mut snippet_option = None;
            let mut snippet_idx = 1;
            for fn_item in iter::once(fn_item).chain(missing_messages) {
                // Only create quickfixes if the method name is declared properly.
                if let Some(fn_name) = fn_item.name().as_ref().map(ToString::to_string) {
                    let params = fn_item.param_list().map(|param_list| {
                        let self_param = param_list.self_param().as_ref().map(ToString::to_string);
                        let other_params = param_list.params().join(", ");
                        format!(
                            "{}{}{}",
                            self_param.as_deref().unwrap_or_default(),
                            if self_param.is_some() && !other_params.is_empty() {
                                ", "
                            } else {
                                ""
                            },
                            other_params
                        )
                    });
                    let ret_type = fn_item
                        .ret_type()
                        .as_ref()
                        .and_then(ast::RetType::ty)
                        .as_ref()
                        .map(ToString::to_string);
                    let fn_sig = format!(
                        "fn {fn_name}({}){}{}",
                        params.unwrap_or_default(),
                        if ret_type.is_some() { " -> " } else { "" },
                        ret_type.unwrap_or_default()
                    );
                    let item_spacing = if snippet_idx > 1 { "\n\n" } else { "" };

                    edit_option = Some(format!(
                        "{}{item_spacing}{fn_sig} {{\n    todo!()\n}}",
                        edit_option.unwrap_or_default()
                    ));
                    snippet_option = Some(format!(
                        "{}{item_spacing}{fn_sig} {{\n    ${{{snippet_idx}:todo!()}}\n}}",
                        snippet_option.unwrap_or_default()
                    ));
                    snippet_idx += 1;
                }
            }
            let format_edit = |edit: String| {
                format!(
                    "{}{}{}",
                    prefix_option.unwrap_or_default(),
                    indent_option
                        .as_ref()
                        .map(|indent| { analysis_utils::apply_indenting(&edit, indent) })
                        .unwrap_or_else(|| edit.clone()),
                    suffix_option.as_deref().unwrap_or_default()
                )
            };
            results.push(Diagnostic {
                message: "Missing message(s) for ink! trait definition implementation.".to_owned(),
                range,
                severity: Severity::Error,
                quickfixes: edit_option.map(|edit| {
                    vec![Action {
                        label: "Add missing message(s) to ink! trait definition implementation."
                            .to_owned(),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::insert_with_snippet(
                            format_edit(edit),
                            insert_offset,
                            snippet_option.map(format_edit),
                        )],
                    }]
                }),
            })
        }
    }
}

/// Verifies that two signature parts "match" or creates an appropriate diagnostic and quickfix.
fn verify_signature_part_match(
    results: &mut Vec<Diagnostic>,
    declared_option: Option<&SyntaxNode>,
    implemented_option: Option<&SyntaxNode>,
    diagnostic_range: TextRange,
    replace_range: TextRange,
    replace_label: &str,
    remove_label_option: Option<&str>,
) {
    match (declared_option, implemented_option) {
        // Handles all cases with a declared option.
        (Some(declared), _) => {
            if implemented_option.map_or(true, |implemented| {
                !analysis_utils::is_trivia_insensitive_eq(implemented, declared)
            }) {
                results.push(Diagnostic {
                    message: format!(
                        "The {replace_label} for this method doesn't match \
                        the ink! trait definition declaration for the method."
                    ),
                    range: diagnostic_range,
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action {
                        label: format!(
                            "Change {replace_label} to match the \
                            ink! trait definition declaration for the method."
                        ),
                        kind: ActionKind::QuickFix,
                        range: diagnostic_range,
                        edits: vec![TextEdit::replace(declared.to_string(), replace_range)],
                    }]),
                });
            }
        }
        // Handles cases with no declared option and an implemented option.
        (None, Some(implemented)) => {
            let range = implemented.text_range();
            let remove_label = remove_label_option.unwrap_or(replace_label);
            results.push(Diagnostic {
                message: format!(
                    "No {remove_label} {} declared on \
                    the ink! trait definition declaration for the method.",
                    if remove_label.ends_with('s') {
                        "are"
                    } else {
                        "is"
                    }
                ),
                range,
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: format!(
                        "Remove {remove_label} to match \
                        the ink! trait definition declaration for the method."
                    ),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::delete(range)],
                }]),
            });
        }
        // Only other case is a match of no option in both the declaration and implementation.
        (None, None) => (),
    }
}

/// Ensures that `fn` item has attributes that match the equivalent ink! trait definition method.
fn ensure_trait_definition_impl_message_args(
    results: &mut Vec<Diagnostic>,
    fn_item: &ast::Fn,
    message_declaration: &Message,
    version: Version,
) {
    // Only ink! message (and it's complementary arguments) are allowed.
    let allowed_arg_kinds: HashSet<InkArgKind> = [InkArgKind::Message]
        .into_iter()
        .chain(analysis_utils::valid_sibling_ink_args(
            InkAttributeKind::Arg(InkArgKind::Message),
            version,
        ))
        .collect();

    // Verifies that `fn` item has attributes that match the equivalent ink! trait definition method.
    let ink_arg_index: HashMap<InkArgKind, InkArg> =
        ink_analyzer_ir::ink_args(message_declaration.syntax())
            .filter_map(|arg| (*arg.kind() != InkArgKind::Unknown).then(|| (*arg.kind(), arg)))
            .collect();
    let mut seen_arg_kinds: HashSet<InkArgKind> = HashSet::new();
    for attr in ink_analyzer_ir::ink_attrs(fn_item.syntax()) {
        let is_macro_attr = matches!(attr.kind(), InkAttributeKind::Macro(_));
        if is_macro_attr {
            results.push(Diagnostic {
                message:
                    "An ink! trait definition's implementation can only contain ink! messages."
                        .to_owned(),
                range: attr.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action::remove_attribute(&attr)]),
            });
        }

        for arg in attr.args() {
            if let Some(arg_declaration) = ink_arg_index.get(arg.kind()) {
                seen_arg_kinds.insert(*arg.kind());

                // Verifies the argument value (if any or expected).
                // NOTE: This doesn't perform argument value validation
                // (that's done by `utils::run_generic_diagnostics`),
                // it only checks that the argument value for the implementation
                // matches the declaration if a value is expected for the attribute kind.
                if let Some(value_declaration) = arg_declaration.value() {
                    if InkArgValueKind::from(*arg.kind()) != InkArgValueKind::None {
                        match arg.value() {
                            // Adds missing value.
                            None => {
                                results.push(Diagnostic {
                                    message: format!(
                                        "Missing ink! {} argument value: {value_declaration}, \
                                        based on the trait definition declaration for this method.",
                                        arg.kind()
                                    ),
                                    range: arg.text_range(),
                                    severity: Severity::Error,
                                    quickfixes: Some(vec![Action {
                                        label: format!("Add missing value: {value_declaration}."),
                                        kind: ActionKind::QuickFix,
                                        range: arg.text_range(),
                                        edits: vec![TextEdit::replace(
                                            arg_declaration.to_string(),
                                            arg.text_range(),
                                        )],
                                    }]),
                                });
                            }
                            // Replaces value that doesn't match declaration.
                            Some(value) => {
                                if !value.trivia_insensitive_eq(value_declaration) {
                                    results.push(Diagnostic {
                                        message: format!("ink! {} argument value mismatch: {value} vs {value_declaration}, \
                                        based on the trait definition declaration for this method.", arg.kind()),
                                        range: arg.text_range(),
                                        severity: Severity::Error,
                                        quickfixes: Some(vec![
                                            Action {
                                                label: format!("Replace missing value (i.e. changes {value} to {value_declaration}."),
                                                kind: ActionKind::QuickFix,
                                                range: arg.text_range(),
                                                edits: vec![
                                                    TextEdit::replace(arg_declaration.to_string(), arg.text_range())
                                                ],
                                            }
                                        ]),
                                    });
                                }
                            }
                        }
                    }
                }
            } else if !is_macro_attr {
                // Edit range for quickfix.
                let range = analysis_utils::ink_arg_and_delimiter_removal_range(arg, Some(&attr));

                results.push(Diagnostic {
                    message: format!(
                        "The ink! trait definition declaration \
                        for this method doesn't have a `{}` argument.",
                        arg
                    ),
                    range: arg.text_range(),
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action {
                        label: format!("Remove `{}` argument.", arg),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::delete(range)],
                    }]),
                });
            }
        }
    }

    // Computes diagnostic for missing "allowed" attribute arguments (if any).
    let mut missing_args = ink_arg_index
        .values()
        .filter(|arg| {
            !seen_arg_kinds.contains(arg.kind()) && allowed_arg_kinds.contains(arg.kind())
        })
        .sorted();
    if let Some(first_arg) = missing_args.next() {
        // Determines a "valid" attribute to extend (if possible).
        let ink_attr_option = ink_analyzer_ir::ink_attrs(fn_item.syntax())
            .filter(|attr| match attr.kind() {
                InkAttributeKind::Arg(arg_kind) => allowed_arg_kinds.contains(arg_kind),
                InkAttributeKind::Macro(_) => false,
            })
            .sorted()
            .next();

        let missing_args_help = [first_arg]
            .into_iter()
            .chain(missing_args.clone())
            .map(|arg| format!("`{arg}`"))
            .join(", ");
        let range = analysis_utils::ast_item_declaration_range(&ast::Item::Fn(fn_item.clone()))
            .unwrap_or(fn_item.syntax().text_range());
        let missing_arg_edits = ink_attr_option
            .and_then(|attr| match first_arg.kind() {
                InkArgKind::Message => analysis_utils::first_ink_arg_insert_offset_and_affixes(
                    &attr,
                )
                .map(|(insert_offset, prefix, suffix)| {
                    let message_arg_edit = TextEdit::insert(
                        format!(
                            "{}{first_arg}{}",
                            prefix.unwrap_or_default(),
                            suffix.unwrap_or_default(),
                        ),
                        insert_offset,
                    );
                    let other_args = missing_args.join(", ");
                    if other_args.is_empty() {
                        vec![message_arg_edit]
                    } else {
                        analysis_utils::ink_arg_insert_offset_and_affixes(&attr, None)
                            .map(|(other_insert_offset, other_prefix, other_suffix)| {
                                vec![
                                    message_arg_edit,
                                    TextEdit::insert(
                                        format!(
                                            "{}{other_args}{}",
                                            other_prefix.unwrap_or_default(),
                                            other_suffix.unwrap_or_default(),
                                        ),
                                        other_insert_offset,
                                    ),
                                ]
                            })
                            .unwrap_or(vec![TextEdit::insert(
                                format!(
                                    "{}{}{}",
                                    prefix.unwrap_or_default(),
                                    [first_arg.to_string(), other_args].join(", "),
                                    suffix.unwrap_or_default(),
                                ),
                                insert_offset,
                            )])
                    }
                }),
                _ => analysis_utils::ink_arg_insert_offset_and_affixes(&attr, None).map(
                    |(insert_offset, prefix, suffix)| {
                        vec![TextEdit::insert(
                            format!(
                                "{}{}{}",
                                prefix.unwrap_or_default(),
                                [first_arg]
                                    .into_iter()
                                    .chain(missing_args.clone())
                                    .join(", "),
                                suffix.unwrap_or_default(),
                            ),
                            insert_offset,
                        )]
                    },
                ),
            })
            .unwrap_or(vec![TextEdit::insert(
                format!(
                    "#[ink({})]",
                    [first_arg].into_iter().chain(missing_args).join(", ")
                ),
                match first_arg.kind() {
                    InkArgKind::Message => {
                        analysis_utils::first_ink_attribute_insert_offset(fn_item.syntax())
                    }
                    _ => analysis_utils::ink_attribute_insert_offset(fn_item.syntax()),
                },
            )]);
        results.push(Diagnostic {
            message: format!(
                "Missing ink! argument(s): {missing_args_help}, \
                which are present ink! trait definition declaration for this method.",
            ),
            range,
            severity: Severity::Error,
            quickfixes: (!missing_arg_edits.is_empty()).then(|| {
                vec![Action {
                    label: format!("Add missing ink! argument(s): {missing_args_help}."),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: missing_arg_edits,
                }]
            }),
        });
    }
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e. ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/impl_item.rs#L62-L106>.
fn ensure_valid_quasi_direct_ink_descendants(
    results: &mut Vec<Diagnostic>,
    ink_impl: &InkImpl,
    version: Version,
) {
    utils::ensure_valid_quasi_direct_ink_descendants_by_kind(
        results,
        ink_impl,
        InkAttributeKind::Arg(InkArgKind::Impl),
        version,
        SCOPE_NAME,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use ink_analyzer_ir::syntax::TextSize;
    use ink_analyzer_ir::InkFile;
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_ink_impl(code: &str) -> InkImpl {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! impls used for positive(`works`) tests for ink! impl verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/tests.rs#L211-L235>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/tests.rs#L37-L91>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/tests.rs#L240-L248>.
    macro_rules! valid_ink_impls {
        () => {
            [
                // Simple.
                quote! {
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn new() -> Self {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(message)]
                        pub fn minimal_message(&self) {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn new() -> Self {}

                        #[ink(message)]
                        pub fn minimal_message(&self) {}
                    }
                },
                // Args.
                quote! {
                    impl MyContract {
                        #[ink(constructor, payable, default, selector=1)]
                        pub fn new() -> Self {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(message, payable, default, selector=1)]
                        pub fn minimal_message(&self) {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(constructor, payable, default, selector=1)]
                        pub fn new() -> Self {}

                        #[ink(message, payable, default, selector=1)]
                        pub fn minimal_message(&self) {}
                    }
                },
            ]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #code
                    },
                    // Impl attribute.
                    quote! {
                        #[ink(impl)]
                        #code
                    },
                    // Namespace Attr.
                    quote! {
                        #[ink(namespace="my_namespace")]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink(impl, namespace="my_namespace")]
                        #code
                    },
                ]
            })
            .chain(
                [
                    // Traits.
                    (
                        quote! {
                            #[ink::trait_definition]
                            pub trait MyTrait {
                                #[ink(message)]
                                fn minimal_message(&self);
                            }
                        },
                        quote! {
                            impl MyTrait for MyContract {
                                #[ink(message)]
                                fn minimal_message(&self) {}
                            }
                        }
                    ),
                    // Traits + Args.
                    (
                        quote! {
                            #[ink::trait_definition]
                            pub trait MyTrait {
                                #[ink(message, payable, default, selector=1)]
                                fn minimal_message(&self);
                            }
                        },
                        quote! {
                            impl MyTrait for MyContract {
                                #[ink(message, payable, default, selector=1)]
                                fn minimal_message(&self) {}
                            }
                        }
                    ),
                ]
                .iter()
                .flat_map(|(trait_code, impl_code)| {
                    // Namespace shouldn't be used for trait implementations.
                    [
                        // Simple.
                        quote! {
                            #trait_code

                            #impl_code
                        },
                        // Impl attribute.
                        quote! {
                            #trait_code

                            #[ink(impl)]
                            #impl_code
                        },
                    ]
                }).chain(
                    [
                        // Empty.
                        // An ink! impl with no ink! constructors or ink! messages is valid
                        // as long as it has an ink! impl annotation.
                        // Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/tests.rs#L212-L215>.
                        quote! {
                            #[ink(impl)]
                            impl MyContract {

                            }
                        },
                    ]
                ),
            )
            // Wrap in contract for context sensitive tests.
            .map(|items| {
                quote! {
                    #[ink::contract]
                    mod my_contract {
                        #items
                    }
                }
            })
        };
    }

    #[test]
    fn impl_works() {
        for code in valid_ink_impls!() {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let result = ensure_impl(&ink_impl);
            assert!(result.is_none());
        }
    }

    #[test]
    fn non_impl_fails() {
        for code in [
            quote! {
                mod my_impl {
                }
            },
            quote! {
                fn my_impl() {
                }
            },
            quote! {
                struct MyImpl;
            },
            quote! {
                enum MyImpl {
                }
            },
            quote! {
                trait MyImpl {
                }
            },
        ] {
            let code = quote_as_pretty_string! {
                #[ink(impl)]
                #code
            };
            let ink_impl = parse_first_ink_impl(&code);

            let result = ensure_impl(&ink_impl);

            // Verifies diagnostics.
            assert!(result.is_some(), "impl: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "impl: {code}"
            );
            // Verifies quickfixes.
            let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
            assert!(fix.label.contains("Remove `#[ink(impl)]`"));
            assert!(fix.edits[0].text.is_empty());
            assert_eq!(
                fix.edits[0].range,
                TextRange::new(
                    TextSize::from(parse_offset_at(&code, Some("<-#[ink(impl)]")).unwrap() as u32),
                    TextSize::from(parse_offset_at(&code, Some("#[ink(impl)]")).unwrap() as u32)
                )
            );
        }
    }

    #[test]
    fn valid_impl_properties_works() {
        for code in valid_ink_impls!() {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_impl_invariants(&mut results, &ink_impl);
            assert!(results.is_empty(), "impl: {code}");
        }
    }

    #[test]
    fn invalid_impl_properties_fails() {
        for (code, expected_quickfixes) in [
            // Default.
            (
                quote! {
                    default impl MyContract {}
                },
                vec![TestResultAction {
                    label: "Remove `default`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-default"),
                        end_pat: Some("default "),
                    }],
                }],
            ),
            // Unsafe.
            (
                quote! {
                    unsafe impl MyContract {}
                },
                vec![TestResultAction {
                    label: "Remove `unsafe`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-unsafe"),
                        end_pat: Some("unsafe "),
                    }],
                }],
            ),
            // Generic.
            (
                quote! {
                    impl MyContract<T> {}
                },
                vec![TestResultAction {
                    label: "Remove generic",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-<T>"),
                        end_pat: Some("<T>"),
                    }],
                }],
            ),
            // Trait implementations with namespace.
            (
                quote! {
                    #[ink(namespace = "my_namespace")]
                    impl MyTrait for MyContract {}
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-#[ink(namespace = "my_namespace")]"#),
                        end_pat: Some(r#"#[ink(namespace = "my_namespace")]"#),
                    }],
                }],
            ),
            // Trait implementations pub visibility for callables.
            (
                quote! {
                    impl MyTrait for MyContract {
                        #[ink(constructor)]
                        pub fn new() -> Self {}
                    }
                },
                vec![TestResultAction {
                    label: "Remove visibility",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-pub"),
                        end_pat: Some("pub "),
                    }],
                }],
            ),
            (
                quote! {
                    impl MyTrait for MyContract {
                        #[ink(message)]
                        pub fn minimal_message(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "Remove visibility",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-pub"),
                        end_pat: Some("pub "),
                    }],
                }],
            ),
            // Inherent implementations inherited visibility for callables.
            (
                quote! {
                    impl MyContract {
                        #[ink(constructor)]
                        fn new() -> Self {}
                    }
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-fn new()"),
                        end_pat: Some("<-fn new()"),
                    }],
                }],
            ),
            (
                quote! {
                    impl MyContract {
                        #[ink(message)]
                        fn minimal_message(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-fn minimal_message"),
                        end_pat: Some("<-fn minimal_message"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(impl)] // needed for this to be parsed as an ink! impl without messages and constructors.
                #code
            };
            let ink_impl = parse_first_ink_impl(&code);

            let mut results = Vec::new();
            ensure_impl_invariants(&mut results, &ink_impl);

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "impl: {code}");
            assert_eq!(results[0].severity, Severity::Error, "impl: {code}");
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn annotated_or_contains_callables_works() {
        for code in valid_ink_impls!() {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let result = ensure_annotation_or_contains_callable(&ink_impl);
            assert!(result.is_none(), "impl: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L688-L704>.
    fn missing_annotation_and_no_callables_ignored() {
        let contract = InkFile::parse(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                }
            }

        })
        .contracts()
        .first()
        .unwrap()
        .clone();

        assert!(contract.impls().is_empty());
    }

    #[test]
    fn impl_parent_for_callables_works() {
        for code in valid_ink_impls!() {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_callables_in_root(&mut results, &ink_impl);
            assert!(results.is_empty(), "impl: {code}");
        }
    }

    #[test]
    fn non_impl_parent_for_callables_fails() {
        for (code, expected_quickfixes) in [
            (
                quote! {
                    fn callable_container() {
                        #[ink(constructor)]
                        pub fn my_constructor() -> i32 {}

                        #[ink(message)]
                        pub fn my_message() {}
                    }
                },
                vec![
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            // Inserts at the end of the `impl MyContract` block
                            TestResultTextRange {
                                text: "pub fn my_constructor",
                                start_pat: Some("pub fn my_message() {}\n    }"),
                                end_pat: Some("pub fn my_message() {}\n    }"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(constructor)]"),
                                end_pat: Some("i32 {}"),
                            },
                        ],
                    }],
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            // Inserts at the end of the `impl MyContract` block
                            TestResultTextRange {
                                text: "pub fn my_message",
                                start_pat: Some("pub fn my_message() {}\n    }"),
                                end_pat: Some("pub fn my_message() {}\n    }"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(message)]"),
                                end_pat: Some("my_message() {}"),
                            },
                        ],
                    }],
                ],
            ),
            (
                quote! {
                    fn callable_container() {
                        struct MyStruct;

                        impl MyStruct {
                            #[ink(constructor)]
                            pub fn my_constructor() -> i32 {}

                            #[ink(message)]
                            pub fn my_message() {}
                        }
                    }
                },
                vec![
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            // Inserts at the end of the `impl MyContract` block
                            TestResultTextRange {
                                text: "pub fn my_constructor",
                                start_pat: Some("pub fn my_message() {}\n        }\n    }"),
                                end_pat: Some("pub fn my_message() {}\n        }\n    }"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(constructor)]"),
                                end_pat: Some("i32 {}"),
                            },
                        ],
                    }],
                    vec![TestResultAction {
                        label: "Move item",
                        edits: vec![
                            // Inserts at the end of the `impl MyContract` block
                            TestResultTextRange {
                                text: "pub fn my_message",
                                start_pat: Some("pub fn my_message() {}\n        }\n    }"),
                                end_pat: Some("pub fn my_message() {}\n        }\n    }"),
                            },
                            TestResultTextRange {
                                text: "",
                                start_pat: Some("<-#[ink(message)]"),
                                end_pat: Some("my_message() {}"),
                            },
                        ],
                    }],
                ],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(impl)] // needed for this to be parsed as an ink! impl without messages and constructors.
                impl MyContract {
                    #code
                }
            };
            let ink_impl = parse_first_ink_impl(&code);

            let mut results = Vec::new();
            ensure_callables_in_root(&mut results, &ink_impl);

            // There should be 2 errors (i.e for the `constructor` and `message`).
            assert_eq!(results.len(), 2, "impl: {code}");
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                2,
                "impl: {code}"
            );
            // Verifies quickfixes.
            for (idx, item) in results.iter().enumerate() {
                let quickfixes = item.quickfixes.as_ref().unwrap();
                verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
            }
        }
    }

    #[test]
    fn valid_trait_definition_impl_works() {
        for code in valid_ink_impls!().chain([
            // Order and trivia (i.e. whitespace and comments) don't matter for matching
            // ink! attribute arguments.
            quote! {
                #[ink::trait_definition]
                pub trait MyTrait {
                    #[ink(message, payable, default, selector=0x1 /* i.e. 1 */)]
                    fn message(&self);
                }

                impl MyTrait for MyContract {
                    #[ink(message, selector = 0x1, default, payable)]
                    fn message(&self) {}
                }
            },
        ]) {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_trait_definition_impl_invariants(&mut results, &ink_impl, Version::V4);
            assert!(results.is_empty(), "impl: {code}");
        }
    }

    #[test]
    fn invalid_trait_definition_impl_fails() {
        for (code, expected_quickfixes) in [
            // Missing methods.
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self);
                    }

                    impl MyTrait for MyContract {}
                },
                vec![TestResultAction {
                    label: "missing message",
                    edits: vec![TestResultTextRange {
                        text: "fn my_message(&self) {",
                        start_pat: Some("impl MyTrait for MyContract {"),
                        end_pat: Some("impl MyTrait for MyContract {"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self, value: u8) -> u8;
                    }

                    impl MyTrait for MyContract {}
                },
                vec![TestResultAction {
                    label: "missing message",
                    edits: vec![TestResultTextRange {
                        text: "fn my_message(&self, value: u8) -> u8 {",
                        start_pat: Some("impl MyTrait for MyContract {"),
                        end_pat: Some("impl MyTrait for MyContract {"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self);

                        #[ink(message)]
                        fn my_message_2(&self);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "missing message",
                    edits: vec![TestResultTextRange {
                        text: "fn my_message_2(&self) {",
                        start_pat: Some("fn my_message(&self) {}"),
                        end_pat: Some("fn my_message(&self) {}"),
                    }],
                }],
            ),
            // Undeclared method.
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self) {}

                        #[ink(message)]
                        fn my_message_2(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(message)]->"),
                        end_pat: Some("fn my_message_2(&self) {}"),
                    }],
                }],
            ),
            // Mismatching parameter list.
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self, a: u8);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "parameter list",
                    edits: vec![TestResultTextRange {
                        text: "(&self, a: u8)",
                        start_pat: Some("<-(&self)"),
                        end_pat: Some("(&self)"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self, a: u8) {}
                    }
                },
                vec![TestResultAction {
                    label: "parameter list",
                    edits: vec![TestResultTextRange {
                        text: "(&self)",
                        start_pat: Some("<-(&self, a: u8)"),
                        end_pat: Some("(&self, a: u8)"),
                    }],
                }],
            ),
            // Mismatching return type.
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self) -> u8;
                    }

                    impl MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "return type",
                    edits: vec![TestResultTextRange {
                        text: "-> u8",
                        start_pat: Some("(&self)->"),
                        end_pat: Some("(&self)->"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self) -> u8 {}
                    }
                },
                vec![TestResultAction {
                    label: "return type",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<--> u8"),
                        end_pat: Some(" -> u8"),
                    }],
                }],
            ),
            // Mismatching ink! attribute arguments.
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, payable)]
                        fn my_message(&self);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "missing ink! argument",
                    edits: vec![TestResultTextRange {
                        text: ", payable",
                        start_pat: Some("#[ink(message->"),
                        end_pat: Some("#[ink(message->"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message, selector = 0xA)]
                        fn my_message(&self) {}
                    }
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, selector = 0xA"),
                        end_pat: Some("selector = 0xA"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #code
            };
            let ink_impl = parse_first_ink_impl(&code);

            for version in [Version::V4, Version::V5] {
                let mut results = Vec::new();
                ensure_trait_definition_impl_invariants(&mut results, &ink_impl, version);

                // Verifies diagnostics.
                assert_eq!(results.len(), 1, "impl: {code}");
                assert_eq!(results[0].severity, Severity::Error, "impl: {code}");
                // Verifies quickfixes.
                verify_actions(
                    &code,
                    results[0].quickfixes.as_ref().unwrap(),
                    &expected_quickfixes,
                );
            }
        }
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for code in valid_ink_impls!() {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &ink_impl, Version::V4);
            assert!(results.is_empty(), "impl: {code}");
        }
    }

    #[test]
    fn invalid_quasi_direct_descendant_fails() {
        let code = quote_as_pretty_string! {
            #[ink(impl)] // needed for this to be parsed as an ink! impl without messages and constructors.
            impl MyContract {
                #[ink(storage)]
                fn my_storage() {}

                #[ink(event)]
                fn my_event() {}

                #[ink::trait_definition]
                fn my_trait_definition() {}

                #[ink::chain_extension]
                fn my_chain_extension() {}

                #[ink::storage_item]
                fn my_storage_item() {}
            }
        };
        let ink_impl = parse_first_ink_impl(&code);

        for version in [Version::V4, Version::V5] {
            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &ink_impl, version);

            // There should be 5 errors (i.e `storage`, `event`, `trait_definition`, `chain_extension` and `storage_item`).
            assert_eq!(results.len(), 5);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                5
            );
            // Verifies quickfixes.
            let expected_quickfixes = vec![
                vec![
                    TestResultAction {
                        label: "Remove `#[ink(storage)]`",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("#[ink(storage)]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove item",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("fn my_storage() {}"),
                        }],
                    },
                ],
                vec![
                    TestResultAction {
                        label: "Remove `#[ink(event)]`",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove item",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("fn my_event() {}"),
                        }],
                    },
                ],
                vec![
                    TestResultAction {
                        label: "Remove `#[ink::trait_definition]`",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink::trait_definition]"),
                            end_pat: Some("#[ink::trait_definition]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove item",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink::trait_definition]"),
                            end_pat: Some("fn my_trait_definition() {}"),
                        }],
                    },
                ],
                vec![
                    TestResultAction {
                        label: "Remove `#[ink::chain_extension]`",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink::chain_extension]"),
                            end_pat: Some("#[ink::chain_extension]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove item",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink::chain_extension]"),
                            end_pat: Some("fn my_chain_extension() {}"),
                        }],
                    },
                ],
                vec![
                    TestResultAction {
                        label: "Remove `#[ink::storage_item]`",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink::storage_item]"),
                            end_pat: Some("#[ink::storage_item]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove item",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink::storage_item]"),
                            end_pat: Some("fn my_storage_item() {}"),
                        }],
                    },
                ],
            ];
            for (idx, item) in results.iter().enumerate() {
                let quickfixes = item.quickfixes.as_ref().unwrap();
                verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/tests.rs#L209-L236>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/tests.rs#L35-L98>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/tests.rs#L238-L255>.
    fn compound_diagnostic_works() {
        for code in valid_ink_impls!() {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &ink_impl, Version::V4, false);
            assert!(results.is_empty(), "impl: {code}");
        }
    }
}
