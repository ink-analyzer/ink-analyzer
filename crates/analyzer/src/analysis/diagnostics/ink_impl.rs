//! ink! impl diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasVisibility};
use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::{
    ast, FromSyntax, InkArgKind, InkAttributeKind, InkImpl, IsInkFn, IsInkImplItem,
};

use super::{constructor, message, utils};
use crate::analysis::snippets::{
    CONSTRUCTOR_PLAIN, CONSTRUCTOR_SNIPPET, MESSAGE_PLAIN, MESSAGE_SNIPPET,
};
use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils as analysis_utils;
use crate::{Action, Diagnostic, Severity};

const IMPL_SCOPE_NAME: &str = "impl";

/// Runs all ink! impl diagnostics.
///
/// The entry point for finding ink! impl semantic rules is the `item_impl` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221-L334>.
pub fn diagnostics(
    results: &mut Vec<Diagnostic>,
    ink_impl: &InkImpl,
    skip_callable_diagnostics: bool,
) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, ink_impl);

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
        ink_impl
            .constructors()
            .iter()
            .for_each(|item| constructor::diagnostics(results, item));

        // Runs ink! message diagnostics, see `message::diagnostics` doc.
        ink_impl
            .messages()
            .iter()
            .for_each(|item| message::diagnostics(results, item));
    }

    // Ensures that ink! messages and constructors are defined in the root of an `impl` item,
    // see `ensure_impl_parent_for_callables` doc.
    ensure_callables_in_root(results, ink_impl);

    // Ensures that ink! impl is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L88-L97>.
    if let Some(diagnostic) = utils::ensure_contract_parent(ink_impl, IMPL_SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // See `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, ink_impl);
}

/// Ensures that ink! impl is an `impl` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221>.
fn ensure_impl(ink_impl: &InkImpl) -> Option<Diagnostic> {
    ink_impl.impl_item().is_none().then_some(Diagnostic {
        message: "ink! impl must be an `impl` item.".to_string(),
        range: ink_impl.syntax().text_range(),
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
                message: "ink! impl must not be `default`.".to_string(),
                range: default_token.text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Remove `default` keyword.".to_string(),
                    range,
                    edits: vec![TextEdit::delete(range)],
                }]),
            });
        }

        if let Some(unsafe_token) = impl_item.unsafe_token() {
            // Edit range for quickfix.
            let range = analysis_utils::token_and_trivia_range(&unsafe_token);
            results.push(Diagnostic {
                message: "ink! impl must not be `unsafe`.".to_string(),
                range: unsafe_token.text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Remove `unsafe` keyword.".to_string(),
                    range,
                    edits: vec![TextEdit::delete(range)],
                }]),
            });
        }

        if let Some(diagnostic) = utils::ensure_no_generics(&impl_item, IMPL_SCOPE_NAME) {
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
                                    .to_string(),
                                range: generic_arg_list.syntax().text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: "Remove generic types.".to_string(),
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
                    .to_string(),
                range: arg.text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Remove ink! namespace argument.".to_string(),
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
                                label: format!("Remove `{}` visibility.", visibility.syntax()),
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
                        results.push(Diagnostic {
                            message: format!(
                                "ink! {name}s in inherent ink! impl blocks must have `pub` visibility."
                            ),
                            range: visibility
                                .as_ref()
                                .map_or(fn_item.syntax(), AstNode::syntax)
                                .text_range(),
                            severity: Severity::Error,
                            quickfixes: visibility
                                .as_ref()
                                .map(|vis| vis.syntax().text_range())
                                .or(fn_item
                                    .default_token()
                                    .or(fn_item.const_token())
                                    .or(fn_item.async_token())
                                    .or(fn_item.unsafe_token())
                                    .or(fn_item.abi().and_then(|abi| {
                                        ink_analyzer_ir::first_child_token(abi.syntax())
                                    }))
                                    .or(fn_item.fn_token())
                                    .map(|it| {
                                        TextRange::new(
                                            it.text_range().start(),
                                            it.text_range().start(),
                                        )
                                    }))
                                .map(|range| {
                                    vec![Action {
                                        label: "Change to `pub` visibility.".to_string(),
                                        range,
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
    (ink_impl.impl_attr().is_none()
        && ink_impl.constructors().is_empty()
        && ink_impl.messages().is_empty())
    .then_some(Diagnostic {
        message: "At least one ink! constructor or ink! message must be defined for an ink! impl without an `#[ink(impl)]` annotation."
            .to_string(),
        range: ink_impl.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: ink_impl
            .impl_item()
            .as_ref()
            .and_then(|impl_item| Some(impl_item).zip(impl_item.assoc_item_list()))
            .map(|(impl_item, assoc_item_list)| {
                // Set quickfix insertion offset at the beginning of the associated items list.
                let insert_offset = analysis_utils::assoc_item_insert_offset_start(&assoc_item_list);
                // Set quickfix insertion indent.
                let indent = analysis_utils::item_children_indenting(impl_item.syntax());

                vec![
                    Action {
                        label: "Add ink! constructor `fn`.".to_string(),
                        range: TextRange::new(insert_offset, insert_offset),
                        edits: vec![TextEdit::insert_with_snippet(
                            analysis_utils::apply_indenting(CONSTRUCTOR_PLAIN, &indent),
                            insert_offset,
                            Some(analysis_utils::apply_indenting(
                                CONSTRUCTOR_SNIPPET,
                                &indent,
                            )),
                        )],
                    },
                    Action {
                        label: "Add ink! message `fn`.".to_string(),
                        range: TextRange::new(insert_offset, insert_offset),
                        edits: vec![TextEdit::insert_with_snippet(
                            analysis_utils::apply_indenting(MESSAGE_PLAIN, &indent),
                            insert_offset,
                            Some(analysis_utils::apply_indenting(MESSAGE_SNIPPET, &indent)),
                        )],
                    },
                ]
            }),
    })
}

/// Ensures that item is defined in the root of this specific `impl` item.
fn ensure_parent_impl<T>(ink_impl: &InkImpl, item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: IsInkImplItem + FromSyntax,
{
    let is_parent = match item.impl_item() {
        Some(parent_impl_item) => parent_impl_item.syntax() == ink_impl.syntax(),
        None => false,
    };

    (!is_parent).then_some(Diagnostic {
        message: format!(
            "ink! {ink_scope_name}s must be defined in the root of an ink! contract's `impl` block."
        ),
        range: item.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: ink_impl
            .impl_item()
            .and_then(|it| it.assoc_item_list())
            .map(|assoc_item_list| {
                // Moves the item to the root of the `impl` block.
                vec![Action::move_item(
                    item.syntax(),
                    analysis_utils::assoc_item_insert_offset_end(&assoc_item_list),
                    "Move item to the root of the ink! contract's `impl` block.".to_string(),
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
    ink_impl
        .constructors()
        .iter()
        .filter_map(|item| ensure_parent_impl(ink_impl, item, "constructor"))
        .chain(
            ink_impl
                .messages()
                .iter()
                .filter_map(|item| ensure_parent_impl(ink_impl, item, "message")),
        )
        .for_each(|diagnostic| results.push(diagnostic));
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/impl_item.rs#L62-L106>.
fn ensure_valid_quasi_direct_ink_descendants(results: &mut Vec<Diagnostic>, ink_impl: &InkImpl) {
    utils::ensure_valid_quasi_direct_ink_descendants(results, ink_impl, |attr| {
        matches!(
            attr.kind(),
            InkAttributeKind::Arg(
                InkArgKind::Constructor
                    | InkArgKind::Message
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
    use crate::test_utils::verify_actions;
    use ink_analyzer_ir::syntax::TextSize;
    use ink_analyzer_ir::InkFile;
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_ink_impl(code: &str) -> InkImpl {
        InkFile::parse(code)
            .syntax()
            .descendants()
            .find_map(InkImpl::cast)
            .unwrap()
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
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor)]
                            fn new() -> Self {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(message)]
                            fn minimal_message(&self) {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor)]
                            fn new() -> Self {}

                            #[ink(message)]
                            fn minimal_message(&self) {}
                        }
                    },
                    // Traits + Args.
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor, payable, default, selector=1)]
                            fn new() -> Self {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(message, payable, default, selector=1)]
                            fn minimal_message(&self) {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor, payable, default, selector=1)]
                            fn new() -> Self {}

                            #[ink(message, payable, default, selector=1)]
                            fn minimal_message(&self) {}
                        }
                    },
                ]
                .iter()
                .flat_map(|code| {
                    // Namespace shouldn't be used for trait implementations.
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
            assert_eq!(&fix.edits[0].text, "");
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
                    label: "Remove `pub`",
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
                    label: "Remove `pub`",
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
                                start_pat: Some("<-}->"),
                                end_pat: Some("<-}->"),
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
                                start_pat: Some("<-}->"),
                                end_pat: Some("<-}->"),
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
                            pub fn my_constructor() -> i32 {
                            }

                            #[ink(message)]
                            pub fn my_message() {
                            }
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
                                start_pat: Some("<-}->"),
                                end_pat: Some("<-}->"),
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
                                start_pat: Some("<-}->"),
                                end_pat: Some("<-}->"),
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
    fn valid_quasi_direct_descendant_works() {
        for code in valid_ink_impls!() {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &ink_impl);
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

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &ink_impl);

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
            diagnostics(&mut results, &ink_impl, false);
            assert!(results.is_empty(), "impl: {code}");
        }
    }
}
