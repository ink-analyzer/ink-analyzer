//! ink! chain extension diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasName};
use ink_analyzer_ir::{
    ast, ChainExtension, Extension, FromInkAttribute, FromSyntax, InkArgKind, InkAttributeKind,
    IsInkTrait,
};
use std::collections::HashSet;

use super::{extension, utils};
use crate::{Diagnostic, Severity};

const CHAIN_EXTENSION_SCOPE_NAME: &str = "chain extension";

/// Runs all ink! chain extension diagnostics.
///
/// The entry point for finding ink! chain extension semantic rules is the `chain_extension` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L201-L211>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L188-L197>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, chain_extension: &ChainExtension) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, chain_extension);

    // Ensures that ink! chain extension is a `trait` item, see `utils::ensure_trait` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L222>.
    if let Some(diagnostic) = utils::ensure_trait(chain_extension, CHAIN_EXTENSION_SCOPE_NAME) {
        results.push(diagnostic);
    }

    if let Some(trait_item) = chain_extension.trait_item() {
        // Ensures that ink! chain extension `trait` item satisfies all common invariants of trait-based ink! entities,
        // see `utils::ensure_trait_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L213-L254>.
        utils::ensure_trait_invariants(results, trait_item, CHAIN_EXTENSION_SCOPE_NAME);

        // Ensures that ink! chain extension `trait` item's associated items satisfy all invariants,
        // see `ensure_trait_item_invariants` doc.
        ensure_trait_item_invariants(results, trait_item);
    }

    // Runs ink! extension diagnostics, see `extension::diagnostics` doc.
    chain_extension
        .extensions()
        .iter()
        .for_each(|item| extension::diagnostics(results, item));

    // Ensures that exactly one `ErrorCode` associated type is defined, see `ensure_error_code_quantity` doc.
    ensure_error_code_type_quantity(results, chain_extension);

    // Ensures that no ink! extension ids are overlapping, see `ensure_no_overlapping_ids` doc.
    ensure_no_overlapping_ids(results, chain_extension);

    // Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // see `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, chain_extension);
}

/// Ensures that ink! chain extension is a `trait` item whose associated items satisfy all invariants.
///
/// See reference below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L213-L254>.
///
/// See `utils::ensure_trait_item_invariants` doc for common invariants for all trait-based ink! entities that are handled by that utility.
/// This utility also runs `extension::diagnostics` on trait methods with a ink! extension attribute.
fn ensure_trait_item_invariants(results: &mut Vec<Diagnostic>, trait_item: &ast::Trait) {
    utils::ensure_trait_item_invariants(
        results,
        trait_item,
        "chain extension",
        |results, fn_item| {
            // All trait methods should be ink! extensions.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L447-L464>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L467-L501>.
            match ink_analyzer_ir::ink_attrs(fn_item.syntax()).find_map(Extension::cast) {
                // Runs ink! extension diagnostics, see `extension::diagnostics` doc.
                Some(extension_item) => extension::diagnostics(results, &extension_item),
                // Add diagnostic if method isn't an ink! extension.
                None => results.push(Diagnostic {
                    message: "All ink! chain extension methods must be ink! extensions."
                        .to_string(),
                    range: fn_item.syntax().text_range(),
                    severity: Severity::Error,
                }),
            }
        },
        |results, type_alias| {
            // Associated type invariants.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L256-L307>.
            let (is_named_error_code, name_marker) = match type_alias.name() {
                Some(name) => (name.to_string() == "ErrorCode", Some(name)),
                None => (false, None),
            };

            if !is_named_error_code {
                results.push(Diagnostic {
                    message:
                        "The associated type of a ink! chain extension must be named `ErrorCode`."
                            .to_string(),
                    range: match name_marker {
                        Some(name) => name.syntax().text_range(),
                        None => trait_item.syntax().text_range(),
                    },
                    severity: Severity::Error,
                });
            }

            if let Some(diagnostic) =
                utils::ensure_no_generics(type_alias, "chain extension `ErrorCode` type")
            {
                results.push(diagnostic);
            }

            if let Some(diagnostic) = utils::ensure_no_trait_bounds(
                type_alias,
                "Trait bounds on ink! chain extension `ErrorCode` types are not supported.",
            ) {
                results.push(diagnostic);
            }

            if type_alias.ty().is_none() {
                results.push(Diagnostic {
                    message: "ink! chain extension `ErrorCode` types must have a default type."
                        .to_string(),
                    range: type_alias.syntax().text_range(),
                    severity: Severity::Error,
                });
            }
        },
    );
}

/// Ensures that exactly one `ErrorCode` associated type is defined.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L292-L305>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L383-L391>.
fn ensure_error_code_type_quantity(
    results: &mut Vec<Diagnostic>,
    chain_extension: &ChainExtension,
) {
    if let Some(trait_item) = chain_extension.trait_item() {
        if let Some(assoc_item_list) = trait_item.assoc_item_list() {
            let error_codes: Vec<ast::TypeAlias> = assoc_item_list
                .assoc_items()
                .filter_map(|assoc_item| match assoc_item {
                    ast::AssocItem::TypeAlias(type_alias) => {
                        let name = type_alias.name()?;
                        (name.to_string() == "ErrorCode").then_some(type_alias)
                    }
                    _ => None,
                })
                .collect();

            if error_codes.is_empty() {
                results.push(Diagnostic {
                    message: "Missing `ErrorCode` associated type for ink! chain extension."
                        .to_string(),
                    range: chain_extension.syntax().text_range(),
                    severity: Severity::Error,
                });
            } else if error_codes.len() > 1 {
                error_codes[1..].iter().for_each(|item| {
                    results.push(Diagnostic {
                        message: "Duplicate `ErrorCode` associated type for ink! chain extension."
                            .to_string(),
                        range: item.syntax().text_range(),
                        severity: Severity::Error,
                    });
                });
            };
        }
    }
}

/// Ensures that no ink! extension ids are overlapping.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L292-L306>.
fn ensure_no_overlapping_ids(results: &mut Vec<Diagnostic>, chain_extension: &ChainExtension) {
    let mut seen_extensions: HashSet<u32> = HashSet::new();
    chain_extension.extensions().iter().for_each(|extension| {
        if let Some(extension_id) = extension.id() {
            let is_seen = seen_extensions.get(&extension_id).is_some();
            seen_extensions.insert(extension_id);

            is_seen.then(|| results.push(Diagnostic {
                message: "Extension ids must be unique across all ink! extensions in an ink! chain extension.".to_string(),
                range: extension.ink_attr().syntax().text_range(),
                severity: Severity::Error,
            }));
        }
    });
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L476-L487>.
fn ensure_valid_quasi_direct_ink_descendants(
    results: &mut Vec<Diagnostic>,
    chain_extension: &ChainExtension,
) {
    utils::ensure_valid_quasi_direct_ink_descendants(results, chain_extension, |attr| {
        matches!(
            attr.kind(),
            InkAttributeKind::Arg(InkArgKind::Extension | InkArgKind::HandleStatus)
        )
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{InkFile, InkMacroKind, IsInkEntity, IsInkTrait};
    use quote::quote;
    use test_utils::quote_as_str;

    fn parse_first_chain_extension(code: &str) -> ChainExtension {
        ChainExtension::cast(
            InkFile::parse(code)
                .tree()
                .ink_attrs_in_scope()
                .find(|attr| *attr.kind() == InkAttributeKind::Macro(InkMacroKind::ChainExtension))
                .unwrap(),
        )
        .unwrap()
    }

    // List of valid minimal ink! chain extensions used for positive(`works`) tests for ink! chain extension verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L875-L888>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L923-L940>.
    macro_rules! valid_chain_extensions {
        () => {
            [
                // No methods.
                quote! {
                    // Chain extension with no methods is valid.
                },
                // Simple.
                quote! {
                    #[ink(extension=1)]
                    fn my_extension();

                    #[ink(extension=2)]
                    fn my_extension2();
                },
                // Input + output variations.
                quote! {
                    #[ink(extension=1)]
                    fn my_extension();

                    #[ink(extension=2)]
                    fn my_extension2(a: i32);

                    #[ink(extension=3)]
                    fn my_extension3() -> bool;

                    #[ink(extension=4)]
                    fn my_extension4(a: i32) -> bool;

                    #[ink(extension=5)]
                    fn my_extension5(a: i32) -> (i32, u64, bool);

                    #[ink(extension=6)]
                    fn my_extension6(a: i32, b: u64, c: [u8; 32]) -> bool;

                    #[ink(extension=7)]
                    fn my_extension7(a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool);
                },
                // Handle status.
                quote! {
                    #[ink(extension=1, handle_status=true)]
                    fn my_extension();

                    #[ink(extension=2, handle_status=false)]
                    fn my_extension2(a: i32);

                    #[ink(extension=3, handle_status=true)]
                    fn my_extension3() -> bool;

                    #[ink(extension=4, handle_status=false)]
                    fn my_extension4(a: i32) -> bool;

                    #[ink(extension=5, handle_status=true)]
                    fn my_extension5(a: i32) -> (i32, u64, bool);

                    #[ink(extension=6, handle_status=false)]
                    fn my_extension6(a: i32, b: u64, c: [u8; 32]) -> bool;

                    #[ink(extension=7, handle_status=true)]
                    fn my_extension7(a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool);
                },
            ]
            .iter()
            .flat_map(|extensions| {
                [
                    // Simple.
                    quote! {
                        #[ink::chain_extension]
                        pub trait MyChainExtension {
                            type ErrorCode = ();

                            #extensions
                        }
                    },
                ]
            })
        };
    }

    #[test]
    fn valid_trait_properties_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_trait_invariants(
                &mut results,
                chain_extension.trait_item().unwrap(),
                "chain extension",
            );
            assert!(results.is_empty(), "chain extension: {code}");
        }
    }

    #[test]
    fn invalid_trait_properties_fails() {
        for code in [
            // Visibility.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L539-L549>.
            quote! {
                trait MyChainExtension {}
            },
            quote! {
                crate trait MyChainExtension {}
            },
            quote! {
                pub(crate) trait MyChainExtension {}
            },
            quote! {
                pub(self) trait MyChainExtension {}
            },
            quote! {
                pub(super) trait MyChainExtension {}
            },
            quote! {
                pub(in my::path) trait MyChainExtension {}
            },
            // Unsafe.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L523-L529>.
            quote! {
                pub unsafe trait MyChainExtension {}
            },
            // Auto.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L531-L537>.
            quote! {
                pub auto trait MyChainExtension {}
            },
            // Generic.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L551-L557>.
            quote! {
                pub trait MyChainExtension<T> {}
            },
            // Supertrait.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L559-L565>.
            quote! {
                pub trait MyChainExtension: SuperChainExtension {}
            },
        ] {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #[ink::chain_extension]
                #code
            });

            let mut results = Vec::new();
            utils::ensure_trait_invariants(
                &mut results,
                chain_extension.trait_item().unwrap(),
                "chain extension",
            );
            assert_eq!(results.len(), 1, "chain extension: {code}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "chain extension: {code}"
            );
        }
    }

    #[test]
    fn valid_trait_items_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_trait_item_invariants(&mut results, chain_extension.trait_item().unwrap());
            assert!(results.is_empty(), "chain extension: {code}");
        }
    }

    #[test]
    fn invalid_trait_items_fails() {
        for items in [
            // Const.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L567-L575>.
            quote! {
                const T: i32;
            },
            // Associate type name.
            // NOTE: default type set to `()` to test only the name.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L577-L585>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L587-L620>.
            quote! {
                type Type = ();
            },
            quote! {
                type IncorrectName  = ();
            },
            // Associate type invariants.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L587-L620>.
            quote! {
                type ErrorCode<T> = (); // generics.
            },
            quote! {
                type ErrorCode: Copy = (); // trait bounds.
            },
            quote! {
                type ErrorCode; // no default type.
            },
            // Macro.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L622-L630>.
            quote! {
                my_macro_call!();
            },
            // Non-flagged method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L632-L652>.
            quote! {
                fn non_flagged();
            },
            // Default implementation.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L654-L663>.
            quote! {
                #[ink(extension=1)]
                fn default_implemented() {}
            },
            // Const method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L665-L674>.
            quote! {
                #[ink(extension=1)]
                const fn const_extension();
            },
            // Async method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L676-L685>.
            quote! {
                #[ink(extension=1)]
                async fn async_extension();
            },
            // Unsafe method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L687-L696>.
            quote! {
                #[ink(extension=1)]
                unsafe fn unsafe_extension();
            },
            // Explicit ABI.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            quote! {
                #[ink(extension=1)]
                    extern fn extern_extension();
            },
            // Variadic method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L709-L718>.
            quote! {
                #[ink(extension=1)]
                fn variadic_extension(...);
            },
            // Generic method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L720-L729>.
            quote! {
                #[ink(extension=1)]
                fn generic_message<T>();
            },
            // Unsupported ink! attribute.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L731-L749>.
            quote! {
                #[ink(constructor)]
                fn my_constructor() -> Self;
            },
            quote! {
                #[ink(message)]
                fn my_message();
            },
            quote! {
                #[ink(unknown)]
                fn unknown_ink_attribute();
            },
            // Invalid message.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L810-L857>.
            quote! {
                #[ink(extension=1)]
                fn has_self_receiver(self);
            },
            quote! {
                #[ink(extension=1)]
                fn has_self_receiver(mut self);
            },
            quote! {
                #[ink(extension=1)]
                fn has_self_receiver(&self);
            },
            quote! {
                #[ink(extension=1)]
                fn has_self_receiver(&mut self);
            },
        ] {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #[ink::chain_extension]
                pub trait MyChainExtension {
                    #items
                }
            });

            let mut results = Vec::new();
            ensure_trait_item_invariants(&mut results, chain_extension.trait_item().unwrap());
            assert_eq!(results.len(), 1, "chain extension: {items}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "chain extension: {items}"
            );
        }
    }

    #[test]
    fn one_error_code_type_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_error_code_type_quantity(&mut results, &chain_extension);
            assert!(results.is_empty(), "chain extension: {code}");
        }
    }

    #[test]
    fn multiple_error_code_types_fails() {
        // Tests snippets with btn 2 and 5 error code types.
        for idx in 2..=5 {
            // Creates multiple error code types.
            let error_code_types = (1..=idx).map(|_| {
                quote! {
                    type ErrorCode = ();
                }
            });

            // Creates contract with multiple error code types.
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #[ink::chain_extension]
                pub trait MyChainExtension {
                    #( #error_code_types )*
                }
            });

            let mut results = Vec::new();
            ensure_error_code_type_quantity(&mut results, &chain_extension);
            // There should be `idx-1` extraneous error code types.
            assert_eq!(results.len(), idx - 1);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                idx - 1
            );
        }
    }

    #[test]
    fn missing_error_code_type_fails() {
        let chain_extension = parse_first_chain_extension(quote_as_str! {
            #[ink::chain_extension]
            pub trait MyChainExtension {
            }
        });

        let mut results = Vec::new();
        ensure_error_code_type_quantity(&mut results, &chain_extension);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
    }

    #[test]
    fn non_overlapping_ids_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_no_overlapping_ids(&mut results, &chain_extension);
            assert!(results.is_empty(), "chain extension: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L859-L870>.
    fn overlapping_ids_fails() {
        for code in [
            // Overlapping decimal.
            quote! {
                #[ink(extension=1)]
                fn my_extension();

                #[ink(extension=1)]
                fn my_extension2();
            },
            // Overlapping hexadecimal.
            quote! {
                #[ink(extension=0x1)]
                fn my_extension();

                #[ink(extension=0x1)]
                fn my_extension2();
            },
            // Overlapping detected across decimal and hex representations.
            quote! {
                #[ink(extension=1)]
                fn my_extension();

                #[ink(extension=0x1)]
                fn my_extension2();
            },
        ] {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #[ink::chain_extension]
                pub trait MyChainExtension {
                    #code
                }
            });

            let mut results = Vec::new();
            ensure_no_overlapping_ids(&mut results, &chain_extension);
            // 1 error the overlapping extension id.
            assert_eq!(results.len(), 1, "chain extension: {code}");
            // All diagnostics should be errors.
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "chain extension: {code}"
            );
        }
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &chain_extension);
            assert!(results.is_empty(), "chain extension: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L731-L749>.
    fn invalid_quasi_direct_descendant_fails() {
        let chain_extension = parse_first_chain_extension(quote_as_str! {
            #[ink::chain_extension]
            pub trait MyChainExtension {
                #[ink(constructor)]
                fn my_constructor() -> Self;

                #[ink(message)]
                fn my_message(&self);

                #[ink(unknown)]
                fn unknown_ink_attribute(&self);
            }
        });

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &chain_extension);
        // 1 diagnostics for `constructor` `message` and `unknown`.
        assert_eq!(results.len(), 3);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            3
        );
    }

    #[test]
    fn compound_diagnostic_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &chain_extension);
            assert!(results.is_empty(), "chain extension: {code}");
        }
    }
}
