//! ink! extension/function diagnostics.

use ink_analyzer_ir::syntax::{AstNode, SyntaxNode};
use ink_analyzer_ir::{ast, InkArgKind, InkAttributeKind, IsChainExtensionFn};
use itertools::Itertools;

use crate::analysis::diagnostics::common;
use crate::{Diagnostic, Version};

const SCOPE_NAME_EXTENSION: &str = "extension";
const SCOPE_NAME_FUNCTION: &str = "function";

const ATTR_KIND_EXTENSION: InkAttributeKind = InkAttributeKind::Arg(InkArgKind::Extension);
const ATTR_KIND_FUNCTION: InkAttributeKind = InkAttributeKind::Arg(InkArgKind::Function);

/// Runs all ink! extension/function diagnostics.
///
/// The entry point for finding ink! extension semantic rules is the `chain_extension` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L467-L500>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L859-L860>.
pub fn diagnostics<T>(results: &mut Vec<Diagnostic>, extension_fn: &T, version: Version)
where
    T: IsChainExtensionFn,
{
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, extension_fn, version);

    let scope_name = versioned_scope_name(version);

    // Ensures that ink! extension/function is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L473>.
    if let Some(diagnostic) = common::ensure_fn(extension_fn, scope_name) {
        results.push(diagnostic);
    }

    if let Some(fn_item) = extension_fn.fn_item() {
        // Ensures that ink! extension/function `fn` item satisfies all common invariants of function-based ink! entities,
        // see `utils::ensure_fn_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L395-L465>.
        common::ensure_fn_invariants(results, fn_item, scope_name);

        // Ensures that ink! extension `fn` item has no self receiver, see `utils::ensure_no_self_receiver` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L488-L493>.
        if let Some(diagnostic) = common::ensure_no_self_receiver(fn_item, scope_name) {
            results.push(diagnostic);
        }
    }

    // Ensures that ink! extension/function input and output types implement SCALE codec traits,
    // see `ensure_custom_types_impl_scale_codec_traits` doc.
    ensure_custom_types_impl_scale_codec_traits(results, extension_fn, version);

    // Ensures that ink! extension/function has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    common::ensure_valid_quasi_direct_ink_descendants_by_kind(
        results,
        extension_fn,
        versioned_attr_kind(version),
        version,
        scope_name,
    );
}

// Ensures that the ink! extension/function input and output types implement SCALE codec traits.
//
// Ref: <https://docs.substrate.io/reference/scale-codec/>.
//
// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L859-L860>.
fn ensure_custom_types_impl_scale_codec_traits<T>(
    results: &mut Vec<Diagnostic>,
    extension_fn: &T,
    version: Version,
) where
    T: IsChainExtensionFn,
{
    // Extracts all custom types from input and output types.
    let custom_types = extension_fn.fn_item().and_then(|fn_item| {
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
                .filter_map(|path| extract_resolvable_custom_types(path, extension_fn.syntax()))
                .flatten()
                // Deduplicate custom types.
                .unique_by(|adt| adt.syntax().text_range())
                .filter_map(|adt| {
                    common::ensure_impl_scale_codec_traits(
                        &adt,
                        "extension input and output types",
                        version,
                    )
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
        .or_else(|| {
            path.segment()
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
                })
        })
}

fn versioned_scope_name(version: Version) -> &'static str {
    if version == Version::V5 {
        SCOPE_NAME_FUNCTION
    } else {
        SCOPE_NAME_EXTENSION
    }
}

fn versioned_attr_kind(version: Version) -> InkAttributeKind {
    if version == Version::V5 {
        ATTR_KIND_FUNCTION
    } else {
        ATTR_KIND_EXTENSION
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::Severity;
    use ink_analyzer_ir::{Extension, Function, IsInkFn};
    use quote::quote;
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

    // List of valid minimal ink! extensions used for positive(`works`) tests for ink! extension verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L878-L887>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L926-L939>.
    macro_rules! valid_extensions {
        () => {
            valid_extensions!(v4)
        };
        (v4) => {
            valid_extensions!(extension)
        };
        (v5) => {
            valid_extensions!(function)
        };
        ($id_arg_kind: expr) => {
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
                        #[ink($id_arg_kind=1)]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink($id_arg_kind=1, handle_status=false)]
                        #code
                    },
                ]
            })
        };
    }

    #[test]
    fn valid_fn_works() {
        for (version, extensions) in versioned_fixtures!(valid_extensions) {
            for code in extensions {
                let mut results = Vec::new();
                if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_fn_invariants(
                        &mut results,
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_FUNCTION,
                    );
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_fn_invariants(
                        &mut results,
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_EXTENSION,
                    );
                }
                assert!(
                    results.is_empty(),
                    "extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn invalid_fn_fails() {
        for (version, id_arg_kind) in [
            (Version::V4, quote! { extension }),
            (Version::V5, quote! { function }),
        ] {
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
                    #[ink(#id_arg_kind=1)]
                    #code
                };

                let mut results = Vec::new();
                if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(&code);
                    common::ensure_fn_invariants(
                        &mut results,
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_FUNCTION,
                    );
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(&code);
                    common::ensure_fn_invariants(
                        &mut results,
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_EXTENSION,
                    );
                }

                // Verifies diagnostics.
                assert_eq!(
                    results.len(),
                    1,
                    "extension: {code}, version: {:?}",
                    version
                );
                assert_eq!(
                    results[0].severity,
                    Severity::Error,
                    "extension: {code}, version: {:?}",
                    version
                );
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
    fn no_self_receiver_works() {
        for (version, extensions) in versioned_fixtures!(valid_extensions) {
            for code in extensions {
                let result = if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_no_self_receiver(
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_FUNCTION,
                    )
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_no_self_receiver(
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_EXTENSION,
                    )
                };
                assert!(
                    result.is_none(),
                    "extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/ir/src/ir/chain_extension.rs#L153-L155>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/ir/src/ir/chain_extension.rs#L812-L859>.
    fn self_receiver_fails() {
        for (version, id_arg_kind) in [
            (Version::V4, quote! { extension }),
            (Version::V5, quote! { function }),
        ] {
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
                    #[ink(#id_arg_kind=1)]
                    #code
                };

                let result = if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(&code);
                    common::ensure_no_self_receiver(
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_FUNCTION,
                    )
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(&code);
                    common::ensure_no_self_receiver(
                        extension.fn_item().unwrap(),
                        SCOPE_NAME_EXTENSION,
                    )
                };

                // Verifies diagnostics.
                assert!(
                    result.is_some(),
                    "extension: {code}, version: {:?}",
                    version
                );
                assert_eq!(
                    result.as_ref().unwrap().severity,
                    Severity::Error,
                    "extension: {code}, version: {:?}",
                    version
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
    }

    #[test]
    fn no_custom_types_or_impl_scale_codec_traits_works() {
        for (version, extensions) in versioned_fixtures!(valid_extensions) {
            for code in extensions {
                let mut results = Vec::new();
                if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_custom_types_impl_scale_codec_traits(&mut results, &extension, version);
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_custom_types_impl_scale_codec_traits(&mut results, &extension, version);
                }
                assert!(
                    results.is_empty(),
                    "extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn custom_types_missing_impl_scale_codec_traits_fails() {
        for (version, id_arg_kind, expected_quickfixes) in [
            (
                Version::V4,
                quote! { extension },
                vec![TestResultAction {
                    label: "Derive `scale::Encode`, `scale::Decode`, `scale_info::TypeInfo`",
                    edits: vec![TestResultTextRange {
                        text: "#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]",
                        start_pat: Some("<-struct MyType;"),
                        end_pat: Some("<-struct MyType;"),
                    }],
                }],
            ),
            (
                Version::V5,
                quote! { function },
                vec![TestResultAction {
                    label: "Derive `scale::Encode`, `scale::Decode`, `scale_info::TypeInfo`",
                    edits: vec![TestResultTextRange {
                        text: "#[ink::scale_derive(Encode, Decode, TypeInfo)]",
                        start_pat: Some("<-struct MyType;"),
                        end_pat: Some("<-struct MyType;"),
                    }],
                }],
            ),
        ] {
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
                // Result type.
                quote! {
                    fn my_extension(a: Result<MyType, ()>);
                },
                quote! {
                    fn my_extension() -> Result<MyType, ()>;
                },
                quote! {
                    fn my_extension(a: Result<(), MyType>);
                },
                quote! {
                    fn my_extension() -> Result<(), MyType>;
                },
                quote! {
                    fn my_extension(a: &Result<MyType, ()>);
                },
                quote! {
                    fn my_extension() -> &Result<MyType, ()>;
                },
                quote! {
                    fn my_extension(a: Result<&MyType, ()>);
                },
                quote! {
                    fn my_extension() -> Result<&MyType, ()>;
                },
            ] {
                let code = quote_as_pretty_string! {
                    #[ink::chain_extension]
                    pub trait MyChainExtension {
                        type ErrorCode = MyErrorCode;

                        #[ink(#id_arg_kind=1)]
                        #fn_def
                    }

                    // Custom type with missing SCALE codec traits implementations.
                    struct MyType;
                };
                let mut results = Vec::new();

                if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(&code);
                    ensure_custom_types_impl_scale_codec_traits(&mut results, &extension, version);
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(&code);
                    ensure_custom_types_impl_scale_codec_traits(&mut results, &extension, version);
                }

                // Verifies diagnostics.
                assert_eq!(
                    results.len(),
                    1,
                    "chain extension: {code}, version: {:?}",
                    version
                );
                assert_eq!(
                    results[0].severity,
                    Severity::Error,
                    "chain extension: {code}, version: {:?}",
                    version
                );
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
    fn no_ink_descendants_works() {
        for (version, extensions) in versioned_fixtures!(valid_extensions) {
            for code in extensions {
                let mut results = Vec::new();
                if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                        &mut results,
                        &extension,
                        versioned_attr_kind(version),
                        version,
                        versioned_scope_name(version),
                    );
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                        &mut results,
                        &extension,
                        versioned_attr_kind(version),
                        version,
                        versioned_scope_name(version),
                    );
                }
                assert!(
                    results.is_empty(),
                    "extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn ink_descendants_fails() {
        for (version, id_arg_kind) in [
            (Version::V4, quote! { extension }),
            (Version::V5, quote! { function }),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(#id_arg_kind=1)]
                fn my_extension() {
                    #[ink(event)]
                    struct MyEvent {
                        #[ink(topic)]
                        value: bool,
                    }
                }
            };
            let mut results = Vec::new();

            if version == Version::V5 {
                let extension: Function = parse_first_ink_entity_of_type(&code);
                common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                    &mut results,
                    &extension,
                    versioned_attr_kind(version),
                    version,
                    versioned_scope_name(version),
                );
            } else {
                let extension: Extension = parse_first_ink_entity_of_type(&code);
                common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                    &mut results,
                    &extension,
                    versioned_attr_kind(version),
                    version,
                    versioned_scope_name(version),
                );
            }

            // 2 diagnostics for `event` and `topic`.
            assert_eq!(
                results.len(),
                2,
                "extension: {code}, version: {:?}",
                version
            );
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                2,
                "extension: {code}, version: {:?}",
                version
            );
            // Verifies quickfixes.
            let expected_quickfixes = [
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
    }

    #[test]
    fn compound_diagnostic_works() {
        for (version, extensions) in versioned_fixtures!(valid_extensions) {
            for code in extensions {
                let mut results = Vec::new();
                if version == Version::V5 {
                    let extension: Function = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    diagnostics(&mut results, &extension, version);
                } else {
                    let extension: Extension = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    diagnostics(&mut results, &extension, version);
                }
                assert!(
                    results.is_empty(),
                    "extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }
}
