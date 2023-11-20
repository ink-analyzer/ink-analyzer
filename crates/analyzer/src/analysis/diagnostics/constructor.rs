//! ink! constructor diagnostics.

use ink_analyzer_ir::ast::AstNode;
use ink_analyzer_ir::{ast, Constructor, IsInkFn};

use super::utils;
use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils as analysis_utils;
use crate::{Action, ActionKind, Diagnostic, Severity};

const CONSTRUCTOR_SCOPE_NAME: &str = "constructor";

/// Runs all ink! constructor diagnostics.
///
/// The entry point for finding ink! constructor semantic rules is the constructor module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L155-L170>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, constructor: &Constructor) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, constructor);

    // Ensures that ink! constructor is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L155>.
    if let Some(diagnostic) = utils::ensure_fn(constructor, CONSTRUCTOR_SCOPE_NAME) {
        results.push(diagnostic);
    }

    if let Some(fn_item) = constructor.fn_item() {
        // Ensures that ink! constructor `fn` item satisfies all common invariants of externally callable ink! entities,
        // see `utils::ensure_callable_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L156>.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/callable.rs#L355-L440>.
        utils::ensure_callable_invariants(results, fn_item, CONSTRUCTOR_SCOPE_NAME);

        // Ensures that ink! constructor `fn` item has no self receiver, see `utils::ensure_no_self_receiver` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L158>.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L107-L128>.
        if let Some(diagnostic) = utils::ensure_no_self_receiver(fn_item, CONSTRUCTOR_SCOPE_NAME) {
            results.push(diagnostic);
        }

        // Ensures that ink! constructor `fn` item has a return type, see `ensure_return_type` doc.
        if let Some(diagnostic) = ensure_return_type(fn_item) {
            results.push(diagnostic);
        }
    }

    // Ensures that ink! constructor has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::ensure_no_ink_descendants(results, constructor, CONSTRUCTOR_SCOPE_NAME);
}

/// Ensures that ink! constructor has a return type.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L157>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L91-L105>.
fn ensure_return_type(fn_item: &ast::Fn) -> Option<Diagnostic> {
    // Determines if the function has a return type.
    let has_return_type = fn_item
        .ret_type()
        .map_or(false, |ret_type| ret_type.ty().is_some());

    // Gets the declaration range for the item.
    let range = analysis_utils::ast_item_declaration_range(&ast::Item::Fn(fn_item.clone()))
        .unwrap_or(fn_item.syntax().text_range());

    (!has_return_type).then_some(Diagnostic {
        message: "ink! constructor must have a return type.".to_string(),
        range,
        severity: Severity::Error,
        quickfixes: fn_item
            .param_list()
            .map(|param_list| param_list.syntax().text_range().end())
            .map(|insert_offset| {
                vec![Action {
                    label: "Add return type.".to_string(),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::insert_with_snippet(
                        " -> Self".to_string(),
                        insert_offset,
                        Some(" -> ${1:Self}".to_string()),
                    )],
                }]
            }),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use quote::quote;
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

    fn parse_first_constructor(code: &str) -> Constructor {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! constructors used for positive(`works`) tests for ink! constructor verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L370-L397>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L259-L282>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L344-L359>.
    macro_rules! valid_constructors {
        () => {
            [
                // simple + inherited visibility
                quote! {
                    fn my_constructor() -> Self {}
                },
                // simple + pub
                quote! {
                    pub fn my_constructor() -> Self {}
                },
                // Result return type + inherited visibility
                quote! {
                    fn my_constructor() -> Result<Self, ()> {}
                },
                // Result return type + pub
                quote! {
                    pub fn my_constructor() -> Result<Self, ()> {}
                },
                // simple + inherited visibility + single input
                quote! {
                    fn my_constructor(a: i32) -> Self {}
                },
                // simple + pub + single input
                quote! {
                    fn my_constructor(a: i32) -> Self {}
                },
                // simple + inherited visibility + many inputs
                quote! {
                    fn my_constructor(a: i32, b: u64, c: [u8; 32]) -> Self {}
                },
                // simple + inherited visibility + many inputs
                quote! {
                    fn my_constructor(a: i32, b: u64, c: [u8; 32]) -> Self {}
                },
            ]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #[ink(constructor)]
                        #code
                    },
                    // Payable.
                    quote! {
                        #[ink(constructor, payable)]
                        #code
                    },
                    // Selector.
                    quote! {
                        #[ink(constructor, selector=1)]
                        #code
                    },
                    quote! {
                        #[ink(constructor, selector=0x1)]
                        #code
                    },
                    quote! {
                        #[ink(constructor, selector=_)]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink(constructor, payable, default, selector=1)]
                        #code
                    },
                    quote! {
                        #[ink(constructor)]
                        #[ink(payable, default, selector=1)]
                        #code
                    },
                    quote! {
                        #[ink(constructor)]
                        #[ink(payable)]
                        #[ink(default)]
                        #[ink(selector=1)]
                        #code
                    },
                ]
            })
        };
    }

    #[test]
    fn valid_callable_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_callable_invariants(
                &mut results,
                constructor.fn_item().unwrap(),
                CONSTRUCTOR_SCOPE_NAME,
            );
            assert!(results.is_empty(), "constructor: {code}");
        }
    }

    #[test]
    fn invalid_callable_fails() {
        for (code, expected_quickfixes) in [
            // Bad visibility.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L557-L575>.
            (
                quote! {
                    pub(crate) fn my_constructor() -> Self {}
                },
                vec![
                    TestResultAction {
                        label: "`pub`",
                        edits: vec![TestResultTextRange {
                            text: "pub",
                            start_pat: Some("<-pub(crate)"),
                            end_pat: Some("pub(crate)"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-pub(crate)"),
                            end_pat: Some("pub(crate) "),
                        }],
                    },
                ],
            ),
            (
                quote! {
                    pub(self) fn my_constructor() -> Self {}
                },
                vec![
                    TestResultAction {
                        label: "`pub`",
                        edits: vec![TestResultTextRange {
                            text: "pub",
                            start_pat: Some("<-pub(self)"),
                            end_pat: Some("pub(self)"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-pub(self)"),
                            end_pat: Some("pub(self) "),
                        }],
                    },
                ],
            ),
            (
                quote! {
                    pub(super) fn my_constructor() -> Self {}
                },
                vec![
                    TestResultAction {
                        label: "`pub`",
                        edits: vec![TestResultTextRange {
                            text: "pub",
                            start_pat: Some("<-pub(super)"),
                            end_pat: Some("pub(super)"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-pub(super)"),
                            end_pat: Some("pub(super) "),
                        }],
                    },
                ],
            ),
            (
                quote! {
                    pub(in my::path) fn my_constructor() -> Self {}
                },
                vec![
                    TestResultAction {
                        label: "`pub`",
                        edits: vec![TestResultTextRange {
                            text: "pub",
                            start_pat: Some("<-pub(in my::path)"),
                            end_pat: Some("pub(in my::path)"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-pub(in my::path)"),
                            end_pat: Some("pub(in my::path) "),
                        }],
                    },
                ],
            ),
            // Generic params fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L452-L467>.
            (
                quote! {
                    fn my_constructor<T>() -> Self {}
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
            (
                quote! {
                    pub fn my_constructor<T>() -> Self {}
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
            (
                quote! {
                    fn my_constructor<T>() -> Self {}
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
            (
                quote! {
                    pub fn my_constructor<T>() -> Self {}
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
            // Const fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L469-L484>.
            (
                quote! {
                    const fn my_constructor() -> Self {}
                },
                vec![TestResultAction {
                    label: "Remove `const`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-const fn"),
                        end_pat: Some("const "),
                    }],
                }],
            ),
            // Async fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L486-L501>.
            (
                quote! {
                    async fn my_constructor() -> Self {}
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
            // Unsafe fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L503-L518>.
            (
                quote! {
                    unsafe fn my_constructor() -> Self {}
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
            // Explicit ABI fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L520-L538>.
            (
                quote! {
                    extern "C" fn my_constructor() -> Self {}
                },
                vec![TestResultAction {
                    label: "Remove explicit ABI",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-extern "C""#),
                        end_pat: Some(r#"extern "C" "#),
                    }],
                }],
            ),
            // Variadic fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L540-L555>.
            (
                quote! {
                    fn my_constructor(...) -> Self {}
                },
                vec![TestResultAction {
                    label: "un-variadic",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-..."),
                        end_pat: Some("..."),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(constructor)]
                #code
            };
            let constructor = parse_first_constructor(&code);

            let mut results = Vec::new();
            utils::ensure_callable_invariants(
                &mut results,
                constructor.fn_item().unwrap(),
                CONSTRUCTOR_SCOPE_NAME,
            );

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "constructor: {code}");
            assert_eq!(results[0].severity, Severity::Error, "constructor: {code}");
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn no_self_receiver_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #code
            });

            let result = utils::ensure_no_self_receiver(
                constructor.fn_item().unwrap(),
                CONSTRUCTOR_SCOPE_NAME,
            );
            assert!(result.is_none(), "constructor: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L424-L450>.
    fn self_receiver_fails() {
        for (code, start_pat, end_pat) in [
            (
                quote! {
                    fn my_constructor(self) -> Self {}
                },
                "<-self",
                "self",
            ),
            (
                quote! {
                    fn my_constructor(mut self) -> Self {}
                },
                "<-mut self",
                "mut self",
            ),
            (
                quote! {
                    fn my_constructor(&self) -> Self {}
                },
                "<-&self",
                "&self",
            ),
            (
                quote! {
                    fn my_constructor(&mut self) -> Self {}
                },
                "<-&mut self",
                "&mut self",
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(constructor)]
                #code
            };
            let constructor = parse_first_constructor(&code);

            let result = utils::ensure_no_self_receiver(
                constructor.fn_item().unwrap(),
                CONSTRUCTOR_SCOPE_NAME,
            );

            // Verifies diagnostics.
            assert!(result.is_some(), "constructor: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "constructor: {code}"
            );
            // Verifies quickfixes.
            let expected_quickfixes = vec![
                // Removes self receiver.
                TestResultAction {
                    label: "self receiver",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(start_pat),
                        end_pat: Some(end_pat),
                    }],
                },
            ];
            let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn return_type_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #code
            });

            let result = ensure_return_type(constructor.fn_item().unwrap());
            assert!(result.is_none(), "constructor: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L407-L422>.
    fn missing_return_type_fails() {
        for (code, pat) in [
            (
                quote! {
                    fn my_constructor() {}
                },
                "()",
            ),
            (
                quote! {
                    fn my_constructor(a: i32) {}
                },
                "(a: i32)",
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(constructor)]
                #code
            };
            let constructor = parse_first_constructor(&code);

            let result = ensure_return_type(constructor.fn_item().unwrap());

            // Verifies diagnostics.
            assert!(result.is_some(), "constructor: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "constructor: {code}"
            );
            // Verifies quickfixes.
            let expected_quickfixes = vec![TestResultAction {
                label: "Add return",
                edits: vec![TestResultTextRange {
                    text: "->",
                    start_pat: Some(pat),
                    end_pat: Some(pat),
                }],
            }];
            let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_no_ink_descendants(&mut results, &constructor, CONSTRUCTOR_SCOPE_NAME);
            assert!(results.is_empty(), "constructor: {code}");
        }
    }

    #[test]
    fn ink_descendants_fails() {
        let code = quote_as_pretty_string! {
            #[ink(constructor)]
            pub fn my_constructor() -> Self {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        };
        let constructor = parse_first_constructor(&code);

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &constructor, CONSTRUCTOR_SCOPE_NAME);

        // 2 diagnostics for `event` and `topic`.
        assert_eq!(results.len(), 2);
        // both diagnostics should be errors.
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
                        end_pat: Some("}"),
                    }],
                },
            ],
            vec![TestResultAction {
                label: "Remove `#[ink(topic)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[ink(topic)]"),
                    end_pat: Some("#[ink(topic)]"),
                }],
            }],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L370-L397>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L259-L282>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L344-L359>.
    fn compound_diagnostic_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &constructor);
            assert!(results.is_empty(), "constructor: {code}");
        }
    }
}
