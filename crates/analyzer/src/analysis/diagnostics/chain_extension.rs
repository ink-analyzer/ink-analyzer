//! ink! chain extension diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasName};
use ink_analyzer_ir::meta::MetaValue;
use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::{
    ast, ChainExtension, Extension, FromInkAttribute, FromSyntax, InkArg, InkArgKind,
    InkAttributeKind, IsInkTrait,
};
use std::collections::HashSet;

use super::{extension, utils};
use crate::analysis::snippets::{ERROR_CODE_PLAIN, ERROR_CODE_SNIPPET};
use crate::analysis::text_edit::TextEdit;
use crate::analysis::utils as analysis_utils;
use crate::{Action, ActionKind, Diagnostic, Severity};

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
    }

    // Ensures that ink! chain extension `trait` item's associated items satisfy all invariants,
    // see `ensure_trait_item_invariants` doc.
    ensure_trait_item_invariants(results, chain_extension);

    // Runs ink! extension diagnostics, see `extension::diagnostics` doc.
    for item in chain_extension.extensions() {
        extension::diagnostics(results, item);
    }

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
fn ensure_trait_item_invariants(results: &mut Vec<Diagnostic>, chain_extension: &ChainExtension) {
    // Tracks already used and suggested ids for quickfixes.
    let mut unavailable_ids = init_unavailable_ids(chain_extension);
    if let Some(trait_item) = chain_extension.trait_item() {
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
                    None => {
                        // Determines quickfix insertion offset and affixes.
                        let insert_offset =
                            analysis_utils::first_ink_attribute_insert_offset(fn_item.syntax());
                        // Computes a unique id for the chain extension method.
                        let suggested_id =
                            analysis_utils::suggest_unique_id(Some(1), &mut unavailable_ids);
                        // Gets the declaration range for the item.
                        let range = analysis_utils::ast_item_declaration_range(&ast::Item::Fn(
                            fn_item.clone(),
                        ))
                        .unwrap_or(fn_item.syntax().text_range());
                        results.push(Diagnostic {
                            message: "All ink! chain extension methods must be ink! extensions."
                                .to_string(),
                            range,
                            severity: Severity::Error,
                            quickfixes: Some(vec![Action {
                                label: "Add ink! extension attribute.".to_string(),
                                kind: ActionKind::QuickFix,
                                range,
                                edits: [TextEdit::insert_with_snippet(
                                    format!("#[ink(extension = {suggested_id})]"),
                                    insert_offset,
                                    Some(format!("#[ink(extension = ${{1:{suggested_id}}})]")),
                                )]
                                .into_iter()
                                .chain(ink_analyzer_ir::ink_attrs(fn_item.syntax()).filter_map(
                                    |attr| {
                                        (!matches!(
                                            attr.kind(),
                                            InkAttributeKind::Arg(InkArgKind::Extension)
                                                | InkAttributeKind::Arg(InkArgKind::HandleStatus)
                                        ))
                                        .then_some(TextEdit::delete(attr.syntax().text_range()))
                                    },
                                ))
                                .collect(),
                            }]),
                        })
                    }
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
                        message: "The associated type of a ink! chain extension must be named `ErrorCode`."
                            .to_string(),
                        range: name_marker
                            .as_ref()
                            .map(|it| it.syntax().text_range())
                            // Defaults to the declaration range for the chain extension.
                            .unwrap_or(
                                chain_extension_declaration_range(chain_extension)
                            ),
                        severity: Severity::Error,
                        quickfixes: name_marker.as_ref().map(|name| {
                            vec![Action {
                                label: "Rename associated type to `ErrorCode`.".to_string(),
                                kind: ActionKind::QuickFix,
                                range: name.syntax().text_range(),
                                edits: vec![TextEdit::replace(
                                    "ErrorCode".to_string(),
                                    name.syntax().text_range(),
                                )],
                            }]
                        }),
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
                    // Get the insert offset and affixes for the quickfix.
                    let insert_offset = type_alias
                        .eq_token()
                        .map(|it| it.text_range().end())
                        .or(type_alias
                            .semicolon_token()
                            .map(|it| it.text_range().start()))
                        .unwrap_or(type_alias.syntax().text_range().start());
                    let insert_prefix = if type_alias.eq_token().is_none() {
                        " = "
                    } else {
                        ""
                    };
                    let insert_suffix = if type_alias.semicolon_token().is_none() {
                        ";"
                    } else {
                        ""
                    };
                    results.push(Diagnostic {
                        message: "ink! chain extension `ErrorCode` types must have a default type."
                            .to_string(),
                        range: type_alias.syntax().text_range(),
                        severity: Severity::Error,
                        quickfixes: Some(vec![Action {
                            label: "Add `ErrorCode` default type.".to_string(),
                            kind: ActionKind::QuickFix,
                            range: type_alias.syntax().text_range(),
                            edits: vec![TextEdit::insert_with_snippet(
                                format!("{insert_prefix}(){insert_suffix}"),
                                insert_offset,
                                Some(format!("{insert_prefix}${{1:()}}{insert_suffix}")),
                            )],
                        }]),
                    });
                }
            },
        );
    }
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
                // Set quickfix insertion offset at the beginning of the associated items list.
                let insert_offset =
                    analysis_utils::assoc_item_insert_offset_start(&assoc_item_list);
                // Set quickfix insertion indent.
                let indent = analysis_utils::item_children_indenting(trait_item.syntax());
                // Gets the declaration range for the item.
                let range = chain_extension_declaration_range(chain_extension);
                // Creates diagnostic and quickfix for missing `ErrorCode` type.
                results.push(Diagnostic {
                    message: "Missing `ErrorCode` associated type for ink! chain extension."
                        .to_string(),
                    range,
                    severity: Severity::Error,
                    quickfixes: Some(vec![Action {
                        label: "Add `ErrorCode` type for ink! chain extension.".to_string(),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::insert_with_snippet(
                            analysis_utils::apply_indenting(ERROR_CODE_PLAIN, &indent),
                            insert_offset,
                            Some(analysis_utils::apply_indenting(ERROR_CODE_SNIPPET, &indent)),
                        )],
                    }]),
                });
            } else if error_codes.len() > 1 {
                for item in &error_codes[1..] {
                    results.push(Diagnostic {
                        message: "Duplicate `ErrorCode` associated type for ink! chain extension."
                            .to_string(),
                        range: item.syntax().text_range(),
                        severity: Severity::Error,
                        quickfixes: Some(vec![Action {
                            label: "Remove duplicate `ErrorCode` type for ink! chain extension."
                                .to_string(),
                            kind: ActionKind::QuickFix,
                            range: item.syntax().text_range(),
                            edits: vec![TextEdit::delete(item.syntax().text_range())],
                        }]),
                    });
                }
            };
        }
    }
}

/// Returns text range of the contract `mod` "declaration" (i.e tokens between meta - attributes/rustdoc - and the start of the item list).
fn chain_extension_declaration_range(chain_extension: &ChainExtension) -> TextRange {
    chain_extension
        .trait_item()
        .and_then(|it| analysis_utils::ast_item_declaration_range(&ast::Item::Trait(it.clone())))
        .unwrap_or(chain_extension.syntax().text_range())
}

/// Ensures that no ink! extension ids are overlapping.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L292-L306>.
fn ensure_no_overlapping_ids(results: &mut Vec<Diagnostic>, chain_extension: &ChainExtension) {
    let mut seen_ids: HashSet<u32> = HashSet::new();
    let mut unavailable_ids = init_unavailable_ids(chain_extension);
    for (idx, extension) in chain_extension.extensions().iter().enumerate() {
        if let Some(id) = extension.id() {
            if seen_ids.get(&id).is_some() {
                // Determines text range for the argument value.
                let value_range_option = extension
                    .ink_attr()
                    .args()
                    .iter()
                    .find(|it| *it.kind() == InkArgKind::Extension)
                    .and_then(InkArg::value)
                    .map(MetaValue::text_range);
                results.push(Diagnostic {
                    message: "Extension ids must be unique across all ink! extensions in an ink! chain extension."
                        .to_string(),
                    range: value_range_option.unwrap_or(extension.ink_attr().syntax().text_range()),
                    severity: Severity::Error,
                    quickfixes: value_range_option.map(|range| {
                        let suggested_id = analysis_utils::suggest_unique_id(
                            Some(idx as u32 + 1),
                            &mut unavailable_ids,
                        );
                        vec![Action {
                            label: "Replace with a unique extension id.".to_string(),
                            kind: ActionKind::QuickFix,
                            range,
                            edits: vec![TextEdit::replace_with_snippet(
                                format!("{suggested_id}"),
                                range,
                                Some(format!("${{1:{suggested_id}}}")),
                            )],
                        }]
                    }),
                })
            }

            seen_ids.insert(id);
        }
    }
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

/// Initializes unavailable extension ids.
fn init_unavailable_ids(chain_extension: &ChainExtension) -> HashSet<u32> {
    chain_extension
        .extensions()
        .iter()
        .filter_map(Extension::id)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::verify_actions;
    use ink_analyzer_ir::syntax::TextSize;
    use ink_analyzer_ir::{InkFile, InkMacroKind, IsInkEntity, IsInkTrait};
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

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
        for (code, expected_quickfixes) in [
            // Visibility.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L539-L549>.
            (
                quote! {
                    trait MyChainExtension {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-trait"),
                        end_pat: Some("<-trait"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(crate) trait MyChainExtension {}
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
                    pub(self) trait MyChainExtension {}
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
                    pub(super) trait MyChainExtension {}
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
                    pub(in my::path) trait MyChainExtension {}
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L523-L529>.
            (
                quote! {
                    pub unsafe trait MyChainExtension {}
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L531-L537>.
            (
                quote! {
                    pub auto trait MyChainExtension {}
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L551-L557>.
            (
                quote! {
                    pub trait MyChainExtension<T> {}
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L559-L565>.
            (
                quote! {
                    pub trait MyChainExtension: SuperChainExtension {}
                },
                vec![TestResultAction {
                    label: "Remove type",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-: SuperChainExtension"),
                        end_pat: Some(": SuperChainExtension"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::chain_extension]
                #code
            };
            let chain_extension = parse_first_chain_extension(&code);

            let mut results = Vec::new();
            utils::ensure_trait_invariants(
                &mut results,
                chain_extension.trait_item().unwrap(),
                "chain extension",
            );

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
    fn valid_trait_items_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_trait_item_invariants(&mut results, &chain_extension);
            assert!(results.is_empty(), "chain extension: {code}");
        }
    }

    #[test]
    fn invalid_trait_items_fails() {
        for (items, expected_quickfixes) in [
            // Const.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L567-L575>.
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
            // Associated type name.
            // NOTE: default type set to `()` to test only the name.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L577-L585>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L587-L620>.
            (
                quote! {
                    type Type = ();
                },
                vec![TestResultAction {
                    label: "`ErrorCode`",
                    edits: vec![TestResultTextRange {
                        text: "ErrorCode",
                        start_pat: Some("<-Type"),
                        end_pat: Some("Type"),
                    }],
                }],
            ),
            (
                quote! {
                    type IncorrectName  = ();
                },
                vec![TestResultAction {
                    label: "`ErrorCode`",
                    edits: vec![TestResultTextRange {
                        text: "ErrorCode",
                        start_pat: Some("<-IncorrectName"),
                        end_pat: Some("IncorrectName"),
                    }],
                }],
            ),
            // Associated type invariants.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L587-L620>.
            (
                quote! {
                    type ErrorCode<T> = (); // generics.
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
                    type ErrorCode: Copy = (); // trait bounds.
                },
                vec![TestResultAction {
                    label: "Remove type",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-: Copy"),
                        end_pat: Some(": Copy"),
                    }],
                }],
            ),
            (
                quote! {
                    type ErrorCode; // no default type.
                },
                vec![TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "=",
                        start_pat: Some("ErrorCode"),
                        end_pat: Some("ErrorCode"),
                    }],
                }],
            ),
            // Macro.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L622-L630>.
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L632-L652>.
            (
                quote! {
                    fn non_flagged();
                },
                vec![TestResultAction {
                    label: "Add ink! extension",
                    edits: vec![TestResultTextRange {
                        text: "extension",
                        start_pat: Some("<-fn"),
                        end_pat: Some("<-fn"),
                    }],
                }],
            ),
            // Default implementation.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L654-L663>.
            (
                quote! {
                    #[ink(extension=1)]
                    fn default_implemented() {}
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L665-L674>.
            (
                quote! {
                    #[ink(extension=1)]
                    const fn const_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L676-L685>.
            (
                quote! {
                    #[ink(extension=1)]
                    async fn async_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L687-L696>.
            (
                quote! {
                    #[ink(extension=1)]
                    unsafe fn unsafe_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            (
                quote! {
                    #[ink(extension=1)]
                    extern fn extern_extension();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L709-L718>.
            (
                quote! {
                    #[ink(extension=1)]
                    fn variadic_extension(...);
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
            // Generic method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L720-L729>.
            (
                quote! {
                    #[ink(extension=1)]
                    fn generic_message<T>();
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
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L731-L749>.
            (
                quote! {
                    #[ink(constructor)]
                    fn my_constructor() -> Self;
                },
                vec![TestResultAction {
                    label: "Add ink! extension",
                    edits: vec![
                        // Add ink! extension attribute.
                        TestResultTextRange {
                            text: "extension",
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
                    #[ink(message)]
                    fn my_message();
                },
                vec![TestResultAction {
                    label: "Add ink! extension",
                    edits: vec![
                        // Add ink! extension attribute.
                        TestResultTextRange {
                            text: "extension",
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("<-#[ink(message)]"),
                        },
                        // Remove ink! message attribute.
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                        },
                    ],
                }],
            ),
            (
                quote! {
                    #[ink(unknown)]
                    fn unknown_method();
                },
                vec![TestResultAction {
                    label: "Add ink! extension",
                    edits: vec![
                        // Add ink! extension attribute.
                        TestResultTextRange {
                            text: "extension",
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
            // self receiver.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L810-L857>.
            (
                quote! {
                    #[ink(extension=1)]
                    fn has_self_receiver(self);
                },
                vec![TestResultAction {
                    label: "Remove self",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-self)"),
                        end_pat: Some("(self"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink(extension=1)]
                    fn has_self_receiver(mut self);
                },
                vec![TestResultAction {
                    label: "Remove self",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-mut self"),
                        end_pat: Some("mut self"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink(extension=1)]
                    fn has_self_receiver(&self);
                },
                vec![TestResultAction {
                    label: "Remove self",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-&self"),
                        end_pat: Some("&self"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink(extension=1)]
                    fn has_self_receiver(&mut self);
                },
                vec![TestResultAction {
                    label: "Remove self",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-&mut self"),
                        end_pat: Some("&mut self"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::chain_extension]
                pub trait MyChainExtension {
                    #items
                }
            };
            let chain_extension = parse_first_chain_extension(&code);

            let mut results = Vec::new();
            ensure_trait_item_invariants(&mut results, &chain_extension);

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "chain extension: {items}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "chain extension: {items}"
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
            // All quickfixes should be for removal.
            for item in results {
                let fix = &item.quickfixes.as_ref().unwrap()[0];
                assert!(fix.label.contains("Remove duplicate `ErrorCode`"));
                assert_eq!(&fix.edits[0].text, "");
            }
        }
    }

    #[test]
    fn missing_error_code_type_fails() {
        let code = quote_as_pretty_string! {
            #[ink::chain_extension]
            pub trait MyChainExtension {
            }
        };
        let chain_extension = parse_first_chain_extension(&code);

        let mut results = Vec::new();
        ensure_error_code_type_quantity(&mut results, &chain_extension);

        // Verifies diagnostics.
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
        // Verifies quickfixes.
        assert!(results[0].quickfixes.as_ref().unwrap()[0]
            .label
            .contains("Add `ErrorCode`"));
        let offset = TextSize::from(parse_offset_at(&code, Some("{")).unwrap() as u32);
        assert_eq!(
            results[0].quickfixes.as_ref().unwrap()[0].edits[0].range,
            TextRange::new(offset, offset)
        );
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
            // Verifies quickfixes.
            let quick_fix_label = &results[0].quickfixes.as_ref().unwrap()[0].label;
            assert!(
                quick_fix_label.contains("Replace")
                    && quick_fix_label.contains("unique extension id")
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
        let code = quote_as_pretty_string! {
            #[ink::chain_extension]
            pub trait MyChainExtension {
                #[ink(constructor)]
                fn my_constructor() -> Self;

                #[ink(message)]
                fn my_message(&self);

                #[ink(unknown)]
                fn unknown_method(&self);
            }
        };
        let chain_extension = parse_first_chain_extension(&code);

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &chain_extension);

        // 1 diagnostic each for `constructor`, `message` and `unknown`.
        assert_eq!(results.len(), 3);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            3
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
                    label: "Remove `#[ink(message)]`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                    }],
                },
                TestResultAction {
                    label: "Remove item",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("fn my_message(&self);"),
                    }],
                },
            ],
            vec![
                TestResultAction {
                    label: "Remove `#[ink(unknown)]`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(unknown)]"),
                        end_pat: Some("#[ink(unknown)]"),
                    }],
                },
                TestResultAction {
                    label: "Remove item",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(unknown)]"),
                        end_pat: Some("fn unknown_method(&self);"),
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
