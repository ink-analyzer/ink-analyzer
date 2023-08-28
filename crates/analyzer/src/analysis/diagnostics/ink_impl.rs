//! ink! impl diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasVisibility};
use ink_analyzer_ir::{
    ast, FromSyntax, InkArgKind, InkAttributeKind, InkImpl, IsInkFn, IsInkImplItem,
};

use super::{constructor, message, utils};
use crate::{Diagnostic, Severity};

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
            results.push(Diagnostic {
                message: "ink! impl must not be `default`.".to_string(),
                range: default_token.text_range(),
                severity: Severity::Error,
            });
        }

        if let Some(unsafe_token) = impl_item.unsafe_token() {
            results.push(Diagnostic {
                message: "ink! impl must not be `unsafe`.".to_string(),
                range: unsafe_token.text_range(),
                severity: Severity::Error,
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
                            })
                        })
                        .collect(),
                );
            }
        }

        if let Some((_, arg)) = impl_item.trait_().zip(ink_impl.namespace_arg()) {
            results.push(Diagnostic {
                message: "ink! namespace argument is not allowed on trait ink! impl blocks."
                    .to_string(),
                range: arg.text_range(),
                severity: Severity::Error,
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
                        results.push(Diagnostic {
                            message: format!("ink! {name}s in trait ink! impl blocks must have inherited visibility."),
                            range: visibility.syntax().text_range(),
                            severity: Severity::Error,
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
                            message: format!("ink! {name}s in inherent ink! impl blocks must have `pub` visibility."),
                            range: visibility.as_ref().map_or(fn_item.syntax(), AstNode::syntax).text_range(),
                            severity: Severity::Error,
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
    use ink_analyzer_ir::InkFile;
    use quote::quote;
    use test_utils::quote_as_str;

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
                struct MyContract;
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
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
                #code
            });

            let result = ensure_impl(&ink_impl);
            assert!(result.is_some(), "impl: {code}");
            assert_eq!(result.unwrap().severity, Severity::Error, "impl: {code}");
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
        for code in [
            // Default.
            quote! {
                default impl MyContract {}
            },
            // Unsafe.
            quote! {
                unsafe impl MyContract {}
            },
            // Generic.
            quote! {
                impl MyContract<T> {}
            },
            // Trait implementations with namespace.
            quote! {
                #[ink(namespace="my_namespace")]
                impl MyTrait for MyContract {}
            },
            // Trait implementations pub visibility for callables.
            quote! {
                impl MyTrait for MyContract {
                    #[ink(constructor)]
                    pub fn new() -> Self {}
                }
            },
            quote! {
                impl MyTrait for MyContract {
                    #[ink(message)]
                    pub fn minimal_message(&self) {}
                }
            },
            // Inherent implementations inherited visibility for callables.
            quote! {
                impl MyContract {
                    #[ink(constructor)]
                    fn new() -> Self {}
                }
            },
            quote! {
                impl MyContract {
                    #[ink(message)]
                    fn minimal_message(&self) {}
                }
            },
        ] {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
                #code
            });

            let mut results = Vec::new();
            ensure_impl_invariants(&mut results, &ink_impl);
            assert_eq!(results.len(), 1, "impl: {code}");
            assert_eq!(results[0].severity, Severity::Error, "impl: {code}");
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
    fn missing_annotation_and_no_callables_fails() {
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
        for code in [
            quote! {
                fn callable_container() {
                    #[ink(constructor)]
                    pub fn my_constructor() -> i32 {
                    }

                    #[ink(message)]
                    pub fn my_message() {
                    }
                }
            },
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
        ] {
            let ink_impl = parse_first_ink_impl(quote_as_str! {
                #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
                impl MyContract {
                    #code
                }
            });

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
        let ink_impl = parse_first_ink_impl(quote_as_str! {
            #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
            impl MyContract {
                #[ink(storage)]
                struct MyContract {
                }

                #[ink(event)]
                struct MyEvent {
                }

                #[ink::trait_definition]
                trait MyTrait {
                }

                #[ink::chain_extension]
                trait MyChainExtension {
                }

                #[ink::storage_item]
                struct MyStorageItem {
                }
            }
        });

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
