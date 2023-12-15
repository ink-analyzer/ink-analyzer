//! ink! extension diagnostics.

use ink_analyzer_ir::syntax::{AstNode, SyntaxNode};
use ink_analyzer_ir::{ast, Extension, InkEntity, IsInkFn};
use itertools::Itertools;

use super::utils;
use crate::Diagnostic;

const EXTENSION_SCOPE_NAME: &str = "extension";

/// Runs all ink! extension diagnostics.
///
/// The entry point for finding ink! extension semantic rules is the `chain_extension` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L467-L500>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L859-L860>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, extension: &Extension) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, extension);

    // Ensures that ink! extension is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L473>.
    if let Some(diagnostic) = utils::ensure_fn(extension, EXTENSION_SCOPE_NAME) {
        results.push(diagnostic);
    }

    if let Some(fn_item) = extension.fn_item() {
        // Ensures that ink! extension `fn` item satisfies all common invariants of function-based ink! entities,
        // see `utils::ensure_fn_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L395-L465>.
        utils::ensure_fn_invariants(results, fn_item, EXTENSION_SCOPE_NAME);

        // Ensures that ink! extension `fn` item has no self receiver, see `utils::ensure_no_self_receiver` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L488-L493>.
        if let Some(diagnostic) = utils::ensure_no_self_receiver(fn_item, EXTENSION_SCOPE_NAME) {
            results.push(diagnostic);
        }
    }

    // Ensures that ink! extension input and output types implement SCALE codec traits,
    // see `ensure_custom_types_impl_scale_codec_traits` doc.
    ensure_custom_types_impl_scale_codec_traits(results, extension);

    // Ensures that ink! extension has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::ensure_no_ink_descendants(results, extension, EXTENSION_SCOPE_NAME);
}

// Ensures that the ink! extension input and output types implement SCALE codec traits.
//
// Ref: <https://docs.substrate.io/reference/scale-codec/>.
//
// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L859-L860>.
fn ensure_custom_types_impl_scale_codec_traits(
    results: &mut Vec<Diagnostic>,
    extension: &Extension,
) {
    // Extracts all custom types from input and output types.
    let custom_types = extension.fn_item().and_then(|fn_item| {
        // Extracts custom types from input types.
        let param_types = fn_item.param_list().map(|param_list| {
            param_list
                .params()
                .filter_map(|param| param.ty().as_ref().and_then(extract_custom_types))
                .flatten()
                .collect::<Vec<_>>()
        });
        // Extracts custom types from output type.
        let ret_types = fn_item
            .ret_type()
            .as_ref()
            .and_then(ast::RetType::ty)
            .as_ref()
            .and_then(extract_custom_types);

        // Merges all extracted custom types.
        match (param_types, ret_types) {
            (Some(params), Some(rets)) => {
                let mut path_types = Vec::with_capacity(params.len() + rets.len());
                path_types.extend(params);
                path_types.extend(rets);
                Some(path_types)
            }
            (param_options, ret_options) => param_options.or(ret_options),
        }
    });

    // Add diagnostics for resolvable/local custom types that don't implement SCALE codec traits.
    if let Some(types) = custom_types {
        results.extend(
            types
                .iter()
                // Deduplicate paths.
                .unique_by(|path| path.to_string())
                // Returns highest-level resolvable/local custom types
                .filter_map(|path| extract_resolvable_custom_types(path, extension.syntax()))
                .flatten()
                // Deduplicate custom types.
                .unique_by(|adt| adt.syntax().text_range())
                .filter_map(|adt| {
                    utils::ensure_impl_scale_codec_traits(&adt, "extension input and output types")
                }),
        );
    }
}

// Returns the highest-level custom types for the type (if any).
fn extract_custom_types(ty: &ast::Type) -> Option<Vec<ast::Path>> {
    macro_rules! extract_type {
        ($type: ident) => {
            $type.ty().as_ref().and_then(extract_custom_types)
        };
    }

    match ty {
        ast::Type::PathType(it) => it.path().map(|path| vec![path]),
        ast::Type::ArrayType(it) => extract_type!(it),
        ast::Type::ParenType(it) => extract_type!(it),
        ast::Type::PtrType(it) => extract_type!(it),
        ast::Type::RefType(it) => extract_type!(it),
        ast::Type::SliceType(it) => extract_type!(it),
        ast::Type::TupleType(it) => {
            let types = it
                .fields()
                .filter_map(|ty| extract_custom_types(&ty))
                .flatten()
                .collect::<Vec<_>>();
            (!types.is_empty()).then_some(types)
        }
        _ => None,
    }
}

// Returns the highest-level resolvable custom types for the path (if any).
fn extract_resolvable_custom_types(
    path: &ast::Path,
    ref_node: &SyntaxNode,
) -> Option<Vec<ast::Adt>> {
    // Resolves custom type from path.
    ink_analyzer_ir::resolve_item::<ast::Adt>(path, ref_node)
        .map(|path| vec![path])
        // Otherwise resolves custom types from generic parameters.
        .or(path
            .segment()
            .as_ref()
            .and_then(ast::PathSegment::generic_arg_list)
            .map(|arg_list| {
                arg_list
                    .generic_args()
                    .filter_map(|arg| match arg {
                        ast::GenericArg::TypeArg(it) => {
                            it.ty().as_ref().and_then(extract_custom_types)
                        }
                        _ => None,
                    })
                    .flatten()
                    // Deduplicate paths.
                    .unique_by(|path| path.to_string())
                    // Returns highest-level resolvable/local custom types
                    .flat_map(|path| extract_resolvable_custom_types(&path, ref_node))
                    .flatten()
                    .collect::<Vec<_>>()
            }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::Severity;
    use quote::quote;
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

    fn parse_first_extension(code: &str) -> Extension {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! extensions used for positive(`works`) tests for ink! extension verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L878-L887>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L926-L939>.
    macro_rules! valid_extensions {
        () => {
            [
                // no input + no output
                quote! {
                    fn my_extension();
                },
                // single input only
                quote! {
                    fn my_extension(a: i32);
                },
                // single output only
                quote! {
                    fn my_extension() -> bool;
                },
                // single input + single output
                quote! {
                    fn my_extension(a: i32) -> bool;
                },
                // single input + tuple output
                quote! {
                    fn my_extension(a: i32) -> (i32, u64, bool);
                },
                // many inputs + output
                quote! {
                    fn my_extension(a: i32, b: u64, c: [u8; 32]) -> bool;
                },
                // many inputs + tuple output
                quote! {
                    fn my_extension(a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool);
                },
            ]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #[ink(extension=1)]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink(extension=1, handle_status=false)]
                        #code
                    },
                ]
            })
        };
    }

    #[test]
    fn valid_fn_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_fn_invariants(
                &mut results,
                extension.fn_item().unwrap(),
                EXTENSION_SCOPE_NAME,
            );
            assert!(results.is_empty(), "extension: {code}");
        }
    }

    #[test]
    fn invalid_fn_fails() {
        for (code, expected_quickfixes) in [
            // Generic params fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L720-L729>.
            (
                quote! {
                    fn my_extension<T>();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L665-L674>.
            (
                quote! {
                    const fn my_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L676-L685>.
            (
                quote! {
                    async fn my_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L687-L696>.
            (
                quote! {
                    unsafe fn my_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            (
                quote! {
                    extern "C" fn my_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            (
                quote! {
                    fn my_extension(...);
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
                #[ink(extension=1)]
                #code
            };
            let extension = parse_first_extension(&code);

            let mut results = Vec::new();
            utils::ensure_fn_invariants(
                &mut results,
                extension.fn_item().unwrap(),
                EXTENSION_SCOPE_NAME,
            );

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "extension: {code}");
            assert_eq!(results[0].severity, Severity::Error, "extension: {code}");
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
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let result =
                utils::ensure_no_self_receiver(extension.fn_item().unwrap(), EXTENSION_SCOPE_NAME);
            assert!(result.is_none(), "extension: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/ir/src/ir/chain_extension.rs#L153-L155>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/ir/src/ir/chain_extension.rs#L812-L859>.
    fn self_receiver_fails() {
        for (code, start_pat, end_pat) in [
            (
                quote! {
                    fn my_extension(self);
                },
                "<-self",
                "self",
            ),
            (
                quote! {
                    fn my_extension(mut self);
                },
                "<-mut self",
                "mut self",
            ),
            (
                quote! {
                    fn my_extension(&self);
                },
                "<-&self",
                "&self",
            ),
            (
                quote! {
                    fn my_extension(&mut self);
                },
                "<-&mut self",
                "&mut self",
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(extension=1)]
                #code
            };
            let extension = parse_first_extension(&code);

            let result =
                utils::ensure_no_self_receiver(extension.fn_item().unwrap(), EXTENSION_SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some(), "extension: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "extension: {code}"
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
    fn no_custom_types_or_impl_scale_codec_traits_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_custom_types_impl_scale_codec_traits(&mut results, &extension);
            assert!(results.is_empty(), "extension: {code}");
        }
    }

    #[test]
    fn custom_types_missing_impl_scale_codec_traits_fails() {
        let expected_quickfixes = vec![TestResultAction {
            label: "Derive `scale::Encode`, `scale::Decode`, `scale_info::TypeInfo`",
            edits: vec![TestResultTextRange {
                text: "scale::Encode, scale::Decode, scale_info::TypeInfo",
                start_pat: Some("<-struct MyType;"),
                end_pat: Some("<-struct MyType;"),
            }],
        }];
        for fn_def in [
            // Simple custom type.
            quote! {
                fn my_extension(a: MyType);
            },
            quote! {
                fn my_extension() -> MyType;
            },
            quote! {
                fn my_extension(a: MyType) -> MyType;
            },
            // Tuple member.
            quote! {
                fn my_extension(a: (i32, MyType));
            },
            quote! {
                fn my_extension() -> (bool, MyType);
            },
            // Array type.
            quote! {
                fn my_extension(a: [MyType]);
            },
            quote! {
                fn my_extension() -> [MyType];
            },
            // Slice type.
            quote! {
                fn my_extension(a: &[MyType]);
            },
            quote! {
                fn my_extension() -> &[MyType];
            },
            // Option type.
            quote! {
                fn my_extension(a: Option<MyType>);
            },
            quote! {
                fn my_extension() -> Option<MyType>;
            },
            quote! {
                fn my_extension(a: &Option<MyType>);
            },
            quote! {
                fn my_extension() -> &Option<MyType>;
            },
            quote! {
                fn my_extension(a: Option<&MyType>);
            },
            quote! {
                fn my_extension() -> Option<&MyType>;
            },
            // Vec type.
            quote! {
                fn my_extension(a: Vec<MyType>);
            },
            quote! {
                fn my_extension() -> Vec<MyType>;
            },
            quote! {
                fn my_extension(a: &Vec<MyType>);
            },
            quote! {
                fn my_extension() -> &Vec<MyType>;
            },
            quote! {
                fn my_extension(a: Vec<&MyType>);
            },
            quote! {
                fn my_extension() -> Vec<&MyType>;
            },
        ] {
            let code = quote_as_pretty_string! {
                #[ink::chain_extension]
                pub trait MyChainExtension {
                    type ErrorCode = MyErrorCode;

                    #[ink(extension=1)]
                    #fn_def
                }

                // Custom type with missing SCALE codec traits implementations.
                struct MyType;
            };
            let extension = parse_first_extension(&code);

            let mut results = Vec::new();
            ensure_custom_types_impl_scale_codec_traits(&mut results, &extension);

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "chain extension: {code}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "chain extension: {code}"
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
    fn no_ink_descendants_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_no_ink_descendants(&mut results, &extension, EXTENSION_SCOPE_NAME);
            assert!(results.is_empty(), "extension: {code}");
        }
    }

    #[test]
    fn ink_descendants_fails() {
        let code = quote_as_pretty_string! {
            #[ink(extension=1)]
            fn my_extension() {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        };
        let extension = parse_first_extension(&code);

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &extension, EXTENSION_SCOPE_NAME);

        // 2 diagnostics for `event` and `topic`.
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
    fn compound_diagnostic_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &extension);
            assert!(results.is_empty(), "extension: {code}");
        }
    }
}
