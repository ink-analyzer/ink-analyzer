//! ink! trait definition diagnostics.

use ink_analyzer_ir::ast::AstNode;
use ink_analyzer_ir::{
    ast, InkArgKind, InkAttributeKind, InkEntity, IsInkTrait, Message, TraitDefinition,
};

use super::{message, utils};
use crate::analysis::actions::entity as entity_actions;
use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils as analysis_utils;
use crate::{Action, ActionKind, Diagnostic, Severity};

const TRAIT_DEFINITION_SCOPE_NAME: &str = "trait definition";

/// Runs all ink! trait definition diagnostics.
///
/// The entry point for finding ink! trait definition semantic rules is the `trait_def` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/mod.rs#L42-L49>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L64-L84>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, trait_definition: &TraitDefinition) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, trait_definition);

    // Ensures that ink! trait definition is a `trait` item, see `utils::ensure_trait` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L116>.
    if let Some(diagnostic) = utils::ensure_trait(trait_definition, TRAIT_DEFINITION_SCOPE_NAME) {
        results.push(diagnostic);
    }

    if let Some(trait_item) = trait_definition.trait_item() {
        // Ensures that ink! trait definition `trait` item satisfies all common invariants of trait-based ink! entities,
        // see `utils::ensure_trait_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L108-L148>.
        utils::ensure_trait_invariants(results, trait_item, TRAIT_DEFINITION_SCOPE_NAME);

        // Ensures that ink! trait definition `trait` item's associated items satisfy all invariants,
        // see `ensure_trait_item_invariants` doc.
        ensure_trait_item_invariants(results, trait_item);
    }

    // Runs ink! message diagnostics, see `message::diagnostics` doc.
    for item in trait_definition.messages() {
        message::diagnostics(results, item);
    }

    // Ensures that at least one ink! message, see `ensure_contains_message` doc.
    if let Some(diagnostic) = ensure_contains_message(trait_definition) {
        results.push(diagnostic);
    }

    // Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // see `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, trait_definition);
}

/// Ensures that ink! trait definition is a `trait` item whose associated items satisfy all invariants.
///
/// See reference below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L150-L208>.
///
/// See `utils::ensure_trait_item_invariants` doc for common invariants for all trait-based ink! entities that are handled by that utility.
/// This utility also runs `message::diagnostics` on trait methods with a ink! message attribute.
fn ensure_trait_item_invariants(results: &mut Vec<Diagnostic>, trait_item: &ast::Trait) {
    utils::ensure_trait_item_invariants(
        results,
        trait_item,
        "trait definition",
        |results, fn_item| {
            // All trait methods should be ink! messages.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L210-L288>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L298-L322>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L290-L296>.
            if let Some(message_item) = ink_analyzer_ir::ink_attrs(fn_item.syntax())
                .find_map(ink_analyzer_ir::ink_attr_to_entity::<Message>)
            {
                // Runs ink! message diagnostics, see `message::diagnostics` doc.
                message::diagnostics(results, &message_item);
            } else {
                // Determines the insertion offset and affixes for the quickfix.
                let insert_offset =
                    analysis_utils::first_ink_attribute_insert_offset(fn_item.syntax());
                // Gets the declaration range for the item.
                let range =
                    analysis_utils::ast_item_declaration_range(&ast::Item::Fn(fn_item.clone()))
                        .unwrap_or(fn_item.syntax().text_range());
                results.push(Diagnostic {
                    message: "All ink! trait definition methods must be ink! messages.".to_owned(),
                    range,
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action {
                        label: "Add ink! message attribute.".to_owned(),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: [TextEdit::insert(
                            "#[ink(message)]".to_owned(),
                            insert_offset,
                        )]
                        .into_iter()
                        .chain(
                            ink_analyzer_ir::ink_attrs(fn_item.syntax()).filter_map(|attr| {
                                (!matches!(
                                    attr.kind(),
                                    InkAttributeKind::Arg(
                                        InkArgKind::Message
                                            | InkArgKind::Default
                                            | InkArgKind::Payable
                                            | InkArgKind::Selector
                                    )
                                ))
                                .then_some(TextEdit::delete(attr.syntax().text_range()))
                            }),
                        )
                        .collect(),
                    }]),
                });
            }

            // Wildcard selectors are not supported.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/trait_item.rs#L80-L101>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L304>.
            for attr in ink_analyzer_ir::ink_attrs(fn_item.syntax()) {
                for arg in attr.args() {
                    if let Some(value) = arg.value() {
                        if value.is_wildcard() {
                            // Edit range for quickfix.
                            let range = analysis_utils::ink_arg_and_delimiter_removal_range(
                                arg,
                                Some(&attr),
                            );
                            results.push(Diagnostic {
                                message:
                                "Wildcard selectors (i.e `selector=_`) on ink! trait definition methods are not supported. \
                                They're only supported on inherent ink! messages and constructors."
                                    .to_owned(),
                                range: arg.text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: "Remove wildcard selector.".to_owned(),
                                    kind: ActionKind::QuickFix,
                                    range,
                                    edits: vec![TextEdit::delete(range)],
                                }]),
                            });
                        }
                    }
                }
            }
        },
        |results, type_alias| {
            results.push(Diagnostic {
                message: "Associated types in ink! trait definitions are not yet supported."
                    .to_owned(),
                range: type_alias.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Remove associated type.".to_owned(),
                    kind: ActionKind::QuickFix,
                    range: type_alias.syntax().text_range(),
                    edits: vec![TextEdit::delete(type_alias.syntax().text_range())],
                }]),
            });
        },
    );
}

/// Ensures that at least one ink! message.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L73-L79>.
fn ensure_contains_message(trait_definition: &TraitDefinition) -> Option<Diagnostic> {
    // Gets the declaration range for the item.
    let range = trait_definition
        .trait_item()
        .and_then(|it| analysis_utils::ast_item_declaration_range(&ast::Item::Trait(it.clone())))
        .unwrap_or(trait_definition.syntax().text_range());
    utils::ensure_at_least_one_item(
        trait_definition.messages(),
        Diagnostic {
            message: "At least one ink! message must be defined for an ink! trait definition."
                .to_owned(),
            range,
            severity: Severity::Error,
            quickfixes: entity_actions::add_message_to_trait_definition(
                trait_definition,
                ActionKind::QuickFix,
                None,
            )
            .map(|action| vec![action]),
        },
    )
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/trait_item.rs#L85-L99>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L163-L164>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L290-L296>.
fn ensure_valid_quasi_direct_ink_descendants(
    results: &mut Vec<Diagnostic>,
    trait_definition: &TraitDefinition,
) {
    utils::ensure_valid_quasi_direct_ink_descendants(results, trait_definition, |attr| {
        matches!(
            attr.kind(),
            InkAttributeKind::Arg(
                InkArgKind::Message
                    | InkArgKind::Payable
                    | InkArgKind::Default
                    | InkArgKind::Selector
            )
        )
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use ink_analyzer_ir::syntax::{TextRange, TextSize};
    use quote::{format_ident, quote};
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_trait_definition(code: &str) -> TraitDefinition {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! trait definitions used for positive(`works`) tests for ink! trait definition verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L329-L334>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L360-L365>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L375-L380>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L391-L404>.
    macro_rules! valid_traits {
        () => {
            [
                // Simple.
                quote! {
                    #[ink(message)]
                    fn my_message(&self);

                    #[ink(message)]
                    fn my_message_mut(&mut self);
                },
                // Selectors.
                quote! {
                    #[ink(message, selector = 0xDEADBEEF)]
                    fn my_message(&self);

                    #[ink(message, selector = 0xC0FEFEED)]
                    fn my_message_mut(&mut self);
                },
                // Payable.
                quote! {
                    #[ink(message, payable)]
                    fn my_message(&self);
                    #[ink(message, payable)]
                    fn my_message_mut(&mut self);
                },
                // Compound.
                quote! {
                    #[ink(message)]
                    fn my_message_1(&self);

                    #[ink(message, payable)]
                    fn my_message_2(&self);

                    #[ink(message, payable, selector = 0xDEADBEEF)]
                    fn my_message_3(&self);

                    #[ink(message)]
                    fn my_message_mut_1(&mut self);

                    #[ink(message, payable)]
                    fn my_message_mut_2(&mut self);

                    #[ink(message, payable, selector = 0xC0DEBEEF)]
                    fn my_message_mut_3(&mut self);
                },
            ]
            .iter()
            .flat_map(|messages| {
                [
                    // Simple.
                    quote! {
                        #[ink::trait_definition]
                        pub trait MyTrait {
                            #messages
                        }
                    },
                    // Namespace.
                    quote! {
                        #[ink::trait_definition(namespace="my_namespace")]
                        pub trait MyTrait {
                            #messages
                        }
                    },
                    // Keep Attr.
                    quote! {
                        #[ink::trait_definition(keep_attr="foo,bar")]
                        pub trait MyTrait {
                            #messages
                        }
                    },
                    // Compound.
                    quote! {
                        #[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar")]
                        pub trait MyTrait {
                            #messages
                        }
                    },
                ]
            })
        };
    }

    #[test]
    fn valid_trait_properties_works() {
        for code in valid_traits!() {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_trait_invariants(
                &mut results,
                trait_definition.trait_item().unwrap(),
                TRAIT_DEFINITION_SCOPE_NAME,
            );
            assert!(results.is_empty(), "trait definition: {code}");
        }
    }

    #[test]
    fn invalid_trait_properties_fails() {
        for (code, expected_quickfixes) in [
            // Visibility.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L48-L58>.
            (
                quote! {
                    trait MyTrait {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-trait MyTrait"),
                        end_pat: Some("<-trait MyTrait"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(crate) trait MyTrait {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(crate)"),
                        end_pat: Some("pub(crate)"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(self) trait MyTrait {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(self)"),
                        end_pat: Some("pub(self)"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(super) trait MyTrait {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(super)"),
                        end_pat: Some("pub(super)"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(in my::path) trait MyTrait {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(in my::path)"),
                        end_pat: Some("pub(in my::path)"),
                    }],
                }],
            ),
            // Unsafe.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L32-L38>.
            (
                quote! {
                    pub unsafe trait MyTrait {}
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
            // Auto.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L40-L46>.
            (
                quote! {
                    pub auto trait MyTrait {}
                },
                vec![TestResultAction {
                    label: "Remove `auto`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-auto"),
                        end_pat: Some("auto "),
                    }],
                }],
            ),
            // Generic.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L60-L66>.
            (
                quote! {
                    pub trait MyTrait<T> {}
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
            // Supertrait.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L68-L74>.
            (
                quote! {
                    pub trait MyTrait: SuperTrait {}
                },
                vec![TestResultAction {
                    label: "Remove type",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-: SuperTrait"),
                        end_pat: Some(": SuperTrait"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::trait_definition]
                #code
            };
            let trait_definition = parse_first_trait_definition(&code);

            let mut results = Vec::new();
            utils::ensure_trait_invariants(
                &mut results,
                trait_definition.trait_item().unwrap(),
                TRAIT_DEFINITION_SCOPE_NAME,
            );

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "trait definition: {code}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "trait definition: {code}"
            );
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn valid_trait_items_works() {
        for code in valid_traits!() {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_trait_item_invariants(&mut results, trait_definition.trait_item().unwrap());
            assert!(results.is_empty(), "trait definition: {code}");
        }
    }

    #[test]
    fn invalid_trait_items_fails() {
        for (items, expected_quickfixes) in [
            // Const.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L76-L84>.
            (
                quote! {
                    const T: i32;
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-const"),
                        end_pat: Some("i32;"),
                    }],
                }],
            ),
            // Type.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L86-L94>.
            (
                quote! {
                    type Type;
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-type"),
                        end_pat: Some("Type;"),
                    }],
                }],
            ),
            // Macro.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L96-L104>.
            (
                quote! {
                    my_macro_call!();
                },
                vec![TestResultAction {
                    label: "Remove macro",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-my_macro_call"),
                        end_pat: Some("my_macro_call!();"),
                    }],
                }],
            ),
            // Non-flagged method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L106-L126>.
            (
                quote! {
                    fn non_flagged(&self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![TestResultTextRange {
                        text: "message",
                        start_pat: Some("<-fn"),
                        end_pat: Some("<-fn"),
                    }],
                }],
            ),
            (
                quote! {
                    fn non_flagged_mut(&mut self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![TestResultTextRange {
                        text: "message",
                        start_pat: Some("<-fn"),
                        end_pat: Some("<-fn"),
                    }],
                }],
            ),
            // Default implementation.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L128-L144>.
            (
                quote! {
                    #[ink(message)]
                    fn default_implemented(&self) {}
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-{}"),
                        end_pat: Some("{}"),
                    }],
                }],
            ),
            // Const method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L146-L162>.
            (
                quote! {
                    #[ink(message)]
                    const fn const_message(&self);
                },
                vec![TestResultAction {
                    label: "Remove `const`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-const"),
                        end_pat: Some("const "),
                    }],
                }],
            ),
            // Async method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L164-L180>.
            (
                quote! {
                    #[ink(message)]
                    async fn async_message(&self);
                },
                vec![TestResultAction {
                    label: "Remove `async`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-async"),
                        end_pat: Some("async "),
                    }],
                }],
            ),
            // Unsafe method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L182-L198>.
            (
                quote! {
                    #[ink(message)]
                    unsafe fn unsafe_message(&self);
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
            // Explicit ABI.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L200-L216>.
            (
                quote! {
                    #[ink(message)]
                    extern fn extern_message(&self);
                },
                vec![TestResultAction {
                    label: "Remove explicit ABI",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-extern"),
                        end_pat: Some("extern "),
                    }],
                }],
            ),
            // Variadic method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L218-L234>.
            (
                quote! {
                    #[ink(message)]
                    fn variadic_message(&self, ...);
                },
                vec![TestResultAction {
                    label: "un-variadic",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, ..."),
                        end_pat: Some("..."),
                    }],
                }],
            ),
            // Generic method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L236-L252>.
            (
                quote! {
                    #[ink(message)]
                    fn generic_message<T>(&self);
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
            // Unsupported ink! attribute.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L254-L270>.
            (
                quote! {
                    #[ink(constructor)]
                    fn my_constructor() -> Self;
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![
                        // Add ink! message attribute.
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("<-#[ink(constructor)]"),
                        },
                        // Remove ink! constructor attribute.
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("#[ink(constructor)]"),
                        },
                    ],
                }],
            ),
            (
                quote! {
                    #[ink(storage)]
                    fn unsupported_method(&self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![
                        // Add ink! message attribute.
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("<-#[ink(storage)]"),
                        },
                        // Remove ink! storage attribute.
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("#[ink(storage)]"),
                        },
                    ],
                }],
            ),
            (
                quote! {
                    #[ink(unknown)]
                    fn unknown_method(&self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![
                        // Add ink! message attribute.
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("<-#[ink(unknown)]"),
                            end_pat: Some("<-#[ink(unknown)]"),
                        },
                        // Remove unknown ink! attribute.
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(unknown)]"),
                            end_pat: Some("#[ink(unknown)]"),
                        },
                    ],
                }],
            ),
            // Invalid message.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L272-L295>.
            (
                quote! {
                    #[ink(message)]
                    fn no_self_ref_receiver();
                },
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&mut self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                ],
            ),
            (
                quote! {
                    #[ink(message)]
                    fn no_self_ref_receiver(self: &Self);
                },
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&mut self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                ],
            ),
            (
                quote! {
                    #[ink(message)]
                    fn no_self_ref_receiver(self);
                },
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&mut self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                ],
            ),
            // Wildcard selectors.
            (
                quote! {
                    #[ink(message, selector = _)]
                    fn has_wildcard_selector(&self);
                },
                vec![TestResultAction {
                    label: "Remove wildcard",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, selector = _"),
                        end_pat: Some("selector = _"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink(message)]
                    #[ink(selector = _)]
                    fn has_wildcard_selector(&self);
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(selector = _)]"),
                        end_pat: Some("#[ink(selector = _)]"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::trait_definition]
                pub trait MyTrait {
                    #items
                }
            };
            let trait_definition = parse_first_trait_definition(&code);

            let mut results = Vec::new();
            ensure_trait_item_invariants(&mut results, trait_definition.trait_item().unwrap());

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "trait definition: {items}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "trait definition: {items}"
            );
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn one_message_works() {
        let trait_definition = parse_first_trait_definition(quote_as_str! {
            #[ink::trait_definition]
            pub trait MyTrait {
                #[ink(message)]
                fn my_message(&self) {
                }
            }
        });

        let result = ensure_contains_message(&trait_definition);
        assert!(result.is_none());
    }

    #[test]
    fn multiple_messages_works() {
        // Tests snippets with btn 2 and 5 messages.
        for idx in 2..=5 {
            // Creates multiple messages.
            let messages = (1..=idx).map(|i| {
                let name = format_ident!("my_message{i}");
                quote! {
                    #[ink(message)]
                    fn #name(&self) {
                    }
                }
            });

            // Creates contract with multiple messages.
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #[ink::trait_definition]
                pub trait MyTrait {
                    #( #messages )*
                }
            });

            let result = ensure_contains_message(&trait_definition);
            assert!(result.is_none());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L106-L126>.
    fn missing_message_fails() {
        let code = quote_as_pretty_string! {
            #[ink::trait_definition]
            pub trait MyTrait {
            }
        };
        let trait_definition = parse_first_trait_definition(&code);

        let result = ensure_contains_message(&trait_definition);

        // Verifies diagnostics.
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
        // Verifies quickfixes.
        assert!(result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0]
            .label
            .contains("Add ink! message"));
        let offset = TextSize::from(parse_offset_at(&code, Some("{")).unwrap() as u32);
        assert_eq!(
            result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0].edits[0].range,
            TextRange::new(offset, offset)
        );
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for code in valid_traits!() {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &trait_definition);
            assert!(results.is_empty());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L254-L270>.
    fn invalid_quasi_direct_descendant_fails() {
        let code = quote_as_pretty_string! {
            #[ink::trait_definition]
            pub trait MyTrait {
                #[ink(constructor)]
                fn my_constructor() -> Self;

                #[ink(event)]
                fn unsupported_method(&self);
            }
        };
        let trait_definition = parse_first_trait_definition(&code);

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &trait_definition);
        // 1 diagnostic each for `constructor` and `event`.
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
            vec![
                TestResultAction {
                    label: "Remove `#[ink(constructor)]`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                    }],
                },
                TestResultAction {
                    label: "Remove item",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("fn my_constructor() -> Self;"),
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
                        end_pat: Some("fn unsupported_method(&self);"),
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
    fn compound_diagnostic_works() {
        for code in valid_traits!() {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &trait_definition);
            assert!(results.is_empty(), "trait definition: {code}");
        }
    }
}
