//! ink! chain extension diagnostics.

mod error_code;
mod extension_fn;

use std::collections::HashSet;

use ink_analyzer_ir::ast::{AstNode, HasName};
use ink_analyzer_ir::meta::MetaValue;
use ink_analyzer_ir::{
    ast, ChainExtension, Extension, Function, InkArg, InkArgKind, InkAttributeKind, InkEntity,
    InkFile, InkMacroKind, IsChainExtensionFn, IsInkTrait, IsIntId,
};

use super::common;
use crate::analysis::{actions::entity as entity_actions, text_edit::TextEdit, utils};
use crate::{Action, ActionKind, Diagnostic, Severity, TextRange, Version};

const SCOPE_NAME: &str = "chain extension";

/// Runs all ink! chain extension diagnostics.
///
/// The entry point for finding ink! chain extension semantic rules is the `chain_extension` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L201-L211>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L188-L197>.
pub fn diagnostics(
    results: &mut Vec<Diagnostic>,
    chain_extension: &ChainExtension,
    version: Version,
) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, chain_extension, version);

    if version.is_v5() {
        // For ink! v5, ensures that ink! chain extension has an ink! extension attribute argument,
        // see `ensure_extension_arg` doc.
        if let Some(diagnostic) = ensure_extension_arg(chain_extension) {
            results.push(diagnostic);
        }
    }

    // Ensures that ink! chain extension is a `trait` item, see `utils::ensure_trait` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L222>.
    if let Some(diagnostic) = common::ensure_trait(chain_extension, SCOPE_NAME) {
        results.push(diagnostic);
    }

    if let Some(trait_item) = chain_extension.trait_item() {
        // Ensures that ink! chain extension `trait` item satisfies all common invariants of trait-based ink! entities,
        // see `utils::ensure_trait_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L213-L254>.
        common::ensure_trait_invariants(results, trait_item, SCOPE_NAME);
    }

    // Ensures that ink! chain extension `trait` item's associated items satisfy all invariants,
    // see `ensure_trait_item_invariants` doc.
    ensure_trait_item_invariants(results, chain_extension, version);

    // Runs ink! extension diagnostics, see `extension_fn::diagnostics` doc.
    for item in chain_extension.extensions() {
        extension_fn::diagnostics(results, item, version);
    }

    // Ensures that exactly one `ErrorCode` associated type is defined, see `ensure_error_code_quantity` doc.
    ensure_error_code_type_quantity(results, chain_extension);

    // Ensures that no ink! extension ids are overlapping, see `ensure_no_overlapping_ids` doc.
    ensure_no_overlapping_ids(results, chain_extension, version);

    // Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // see `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, chain_extension, version);

    // Runs ink! chain extension `ErrorCode` type diagnostics, see `error_code::diagnostics` doc.
    error_code::diagnostics(results, chain_extension, version);
}

/// Ensures that ink! chain extension has an ink! extension attribute argument,
/// see `ensure_extension_arg` doc.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html#macro-attributes>
///
/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/ir/src/ir/chain_extension.rs#L117-L121>
///
/// NOTE: Should only be used for ink! v5.
fn ensure_extension_arg(chain_extension: &ChainExtension) -> Option<Diagnostic> {
    chain_extension.extension_arg().is_none().then(|| {
        // Text range for the diagnostic.
        let range = chain_extension
            .ink_attr()
            .map(|attr| attr.syntax().text_range())
            .unwrap_or_else(|| utils::ink_trait_declaration_range(chain_extension));
        Diagnostic {
            message: "An ink! chain extension must have a ink! extension argument.".to_owned(),
            range,
            severity: Severity::Error,
            quickfixes: chain_extension
                .ink_attr()
                .and_then(|attr| utils::ink_arg_insert_offset_and_affixes(attr, None))
                .map(|(insert_offset, insert_prefix, insert_suffix)| {
                    // Range for the quickfix.
                    let range = TextRange::new(insert_offset, insert_offset);
                    // Suggested id for the extension.
                    let suggested_id = chain_extension
                        .syntax()
                        .ancestors()
                        .last()
                        .and_then(InkFile::cast)
                        .and_then(|file| {
                            let unavailable_ids = file
                                .chain_extensions()
                                .iter()
                                .filter_map(ChainExtension::id)
                                .collect();
                            utils::suggest_unique_id(None, &unavailable_ids)
                        })
                        .unwrap_or(1);

                    vec![Action {
                        label: "Add ink! extension argument.".to_owned(),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::insert_with_snippet(
                            format!(
                                "{}extension = {suggested_id}{}",
                                insert_prefix.unwrap_or_default(),
                                insert_suffix.unwrap_or_default()
                            ),
                            insert_offset,
                            Some(format!(
                                "{}extension = ${{1:{suggested_id}}}{}",
                                insert_prefix.unwrap_or_default(),
                                insert_suffix.unwrap_or_default()
                            )),
                        )],
                    }]
                }),
        }
    })
}

/// Ensures that ink! chain extension is a `trait` item whose associated items satisfy all invariants.
///
/// See reference below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L213-L254>.
///
/// See `utils::ensure_trait_item_invariants` doc for common invariants for all trait-based ink! entities that are handled by that utility.
/// This utility also runs `extension_fn::diagnostics` on trait functions with an ink! extension attribute.
fn ensure_trait_item_invariants(
    results: &mut Vec<Diagnostic>,
    chain_extension: &ChainExtension,
    version: Version,
) {
    let assoc_type_validator = |results: &mut Vec<Diagnostic>, type_alias: &ast::TypeAlias| {
        // Associated type invariants.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L256-L307>.
        let (is_named_error_code, name_marker) = match type_alias.name() {
            Some(name) => (name.to_string() == "ErrorCode", Some(name)),
            None => (false, None),
        };

        if !is_named_error_code {
            results.push(Diagnostic {
                message: "The associated type of a ink! chain extension must be named `ErrorCode`."
                    .to_owned(),
                range: name_marker
                    .as_ref()
                    // Defaults to the declaration range for the chain extension.
                    .map(|it| it.syntax().text_range())
                    .unwrap_or_else(|| utils::ink_trait_declaration_range(chain_extension)),
                severity: Severity::Error,
                quickfixes: name_marker.as_ref().map(|name| {
                    vec![Action {
                        label: "Rename associated type to `ErrorCode`.".to_owned(),
                        kind: ActionKind::QuickFix,
                        range: name.syntax().text_range(),
                        edits: vec![TextEdit::replace(
                            "ErrorCode".to_owned(),
                            name.syntax().text_range(),
                        )],
                    }]
                }),
            });
        }

        if let Some(diagnostic) =
            common::ensure_no_generics(type_alias, "chain extension `ErrorCode` type")
        {
            results.push(diagnostic);
        }

        if let Some(diagnostic) = common::ensure_no_trait_bounds(
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
                .or_else(|| {
                    type_alias
                        .semicolon_token()
                        .map(|it| it.text_range().start())
                })
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
                    .to_owned(),
                range: type_alias.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: "Add `ErrorCode` default type.".to_owned(),
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
    };
    macro_rules! trait_item_validator {
        ($trait_item: ident, $entity: ty, $id_arg_kind: ident, $id_type: ty) => {
            // Tracks already used and suggested ids for quickfixes.
            let mut unavailable_ids: HashSet<$id_type> = init_unavailable_ids(chain_extension, version);
            common::ensure_trait_item_invariants(
                results,
                $trait_item,
                "chain extension",
                |results, fn_item| {
                    // All trait functions should be ink! extensions.
                    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L447-L464>.
                    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L467-L501>.
                    let extension_fn = ink_analyzer_ir::ink_attrs(fn_item.syntax())
                        .find_map(ink_analyzer_ir::ink_attr_to_entity::<$entity>);
                    if let Some(extension_fn) = extension_fn {
                        // Runs ink! function/extension diagnostics, see `extension_fn::diagnostics` doc.
                        extension_fn::diagnostics(results, &extension_fn, version);
                    } else {
                        // Add diagnostic if function isn't an ink! function/extension.
                        assoc_fn_validator(results, fn_item, &mut unavailable_ids, InkArgKind::$id_arg_kind)
                    }
                },
                assoc_type_validator,
            );
        };
    }

    if let Some(trait_item) = chain_extension.trait_item() {
        if version.is_legacy() {
            trait_item_validator!(trait_item, Extension, Extension, u32);
        } else {
            trait_item_validator!(trait_item, Function, Function, u16);
        }
    }

    fn assoc_fn_validator<T>(
        results: &mut Vec<Diagnostic>,
        fn_item: &ast::Fn,
        unavailable_ids: &mut HashSet<T>,
        id_arg_kind: InkArgKind,
    ) where
        T: IsIntId,
    {
        // Determines quickfix insertion offset and affixes.
        let insert_offset = utils::first_ink_attribute_insert_offset(fn_item.syntax());
        // Computes a unique id for the chain extension function.
        let suggested_id = utils::suggest_unique_id_mut(None, unavailable_ids).unwrap_or(1.into());
        // Gets the declaration range for the item.
        let range = utils::ast_item_declaration_range(&ast::Item::Fn(fn_item.clone()))
            .unwrap_or(fn_item.syntax().text_range());

        // Suppress missing `function` arg warnings if the deprecated `extension` arg is present
        // to reduce noise, because there will be a deprecation warning (and quickfix) added by
        // `utils::validate_entity_attributes`.
        let is_extension_arg =
            || ink_analyzer_ir::ink_arg_by_kind(fn_item.syntax(), InkArgKind::Extension).is_some();
        if id_arg_kind == InkArgKind::Function && is_extension_arg() {
            return;
        }

        results.push(Diagnostic {
            message: format!("All ink! chain extension functions must be ink! {id_arg_kind}s."),
            range,
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: format!("Add ink! {id_arg_kind} attribute."),
                kind: ActionKind::QuickFix,
                range,
                edits: [TextEdit::insert_with_snippet(
                    format!("#[ink({id_arg_kind} = {suggested_id})]"),
                    insert_offset,
                    Some(format!("#[ink({id_arg_kind} = ${{1:{suggested_id}}})]")),
                )]
                .into_iter()
                .chain(
                    ink_analyzer_ir::ink_attrs(fn_item.syntax()).filter_map(|attr| {
                        (*attr.kind() != InkAttributeKind::Arg(id_arg_kind)
                            && *attr.kind() != InkAttributeKind::Arg(InkArgKind::HandleStatus))
                        .then(|| TextEdit::delete(attr.syntax().text_range()))
                    }),
                )
                .collect(),
            }]),
        });
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
                // Creates diagnostic and quickfix for missing `ErrorCode` type.
                results.push(Diagnostic {
                    message: "Missing `ErrorCode` associated type for ink! chain extension."
                        .to_owned(),
                    range: utils::ink_trait_declaration_range(chain_extension),
                    severity: Severity::Error,
                    quickfixes: entity_actions::add_error_code(
                        chain_extension,
                        ActionKind::QuickFix,
                        None,
                    )
                    .map(|action| vec![action]),
                });
            } else if error_codes.len() > 1 {
                for item in &error_codes[1..] {
                    results.push(Diagnostic {
                        message: "Duplicate `ErrorCode` associated type for ink! chain extension."
                            .to_owned(),
                        range: item.syntax().text_range(),
                        severity: Severity::Error,
                        quickfixes: Some(vec![Action {
                            label: "Remove duplicate `ErrorCode` type for ink! chain extension."
                                .to_owned(),
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

/// Ensures that no ink! extension ids are overlapping.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L292-L306>.
fn ensure_no_overlapping_ids(
    results: &mut Vec<Diagnostic>,
    chain_extension: &ChainExtension,
    version: Version,
) {
    if version.is_legacy() {
        let mut unavailable_ids = init_unavailable_ids(chain_extension, version);
        ensure_no_overlapping_ids_inner::<_, u32>(
            results,
            chain_extension.extensions(),
            &mut unavailable_ids,
            version,
        );
    } else {
        let mut unavailable_ids = init_unavailable_ids(chain_extension, version);
        ensure_no_overlapping_ids_inner::<_, u16>(
            results,
            chain_extension.functions(),
            &mut unavailable_ids,
            version,
        );
    }

    fn ensure_no_overlapping_ids_inner<E, I>(
        results: &mut Vec<Diagnostic>,
        extension_fns: &[E],
        unavailable_ids: &mut HashSet<I>,
        version: Version,
    ) where
        E: IsChainExtensionFn,
        I: IsIntId,
    {
        let mut seen_ids: HashSet<I> = HashSet::new();
        for extension_fn in extension_fns {
            if let Some(id) = extension_fn.id() {
                if seen_ids.contains(&id) {
                    // Determines text range for the argument value.
                    let value_range_option = extension_fn
                        .id_arg()
                        .as_ref()
                        .and_then(InkArg::value)
                        .map(MetaValue::text_range);
                    results.push(Diagnostic {
                        message: format!(
                            "{} ids must be unique across all associated functions \
                            in an ink! chain extension.",
                            if version.is_legacy() {
                                "Extension"
                            } else {
                                "Function"
                            }
                        ),
                        range: value_range_option
                            .or_else(|| {
                                extension_fn
                                    .ink_attr()
                                    .map(|attr| attr.syntax().text_range())
                            })
                            .unwrap_or(extension_fn.syntax().text_range()),
                        severity: Severity::Error,
                        quickfixes: value_range_option
                            .zip(utils::suggest_unique_id_mut(None, unavailable_ids))
                            .map(|(range, suggested_id)| {
                                vec![Action {
                                    label: format!(
                                        "Replace with a unique {} id.",
                                        if version.is_legacy() {
                                            "extension"
                                        } else {
                                            "function"
                                        }
                                    ),
                                    kind: ActionKind::QuickFix,
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

                seen_ids.insert(id);
            }
        }
    }
}

/// Ensures that only valid quasi-direct ink! attribute descendants
/// (i.e. ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L476-L487>.
fn ensure_valid_quasi_direct_ink_descendants(
    results: &mut Vec<Diagnostic>,
    chain_extension: &ChainExtension,
    version: Version,
) {
    common::ensure_valid_quasi_direct_ink_descendants_by_kind(
        results,
        chain_extension,
        InkAttributeKind::Macro(InkMacroKind::ChainExtension),
        version,
        SCOPE_NAME,
    );
}

/// Initializes unavailable extension ids.
fn init_unavailable_ids<T>(chain_extension: &ChainExtension, version: Version) -> HashSet<T>
where
    T: IsIntId,
{
    fn init_unavailable_ids_inner<E, I>(assoc_fns: &[E]) -> HashSet<I>
    where
        E: IsChainExtensionFn,
        I: IsIntId,
    {
        assoc_fns
            .iter()
            .filter_map(IsChainExtensionFn::id)
            .collect()
    }

    if version.is_legacy() {
        init_unavailable_ids_inner(chain_extension.extensions())
    } else {
        init_unavailable_ids_inner(chain_extension.functions())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use ink_analyzer_ir::{
        syntax::{TextRange, TextSize},
        MinorVersion,
    };
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_chain_extension(code: &str) -> ChainExtension {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! chain extensions used for positive(`works`) tests for ink! chain extension verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L875-L888>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L923-L940>.
    macro_rules! valid_chain_extensions {
        () => {
            valid_chain_extensions!(v4)
        };
        (v4) => {
            valid_chain_extensions!(extension, derive(scale::Encode, scale::Decode, scale_info::TypeInfo))
        };
        (v5) => {
            valid_chain_extensions!(function, ink::scale_derive(Encode, Decode, TypeInfo), extension=1)
        };
        ($id_arg_kind: expr, $scale_derive_attr: meta $(, $macro_args: meta)?) => {
            [
                // No functions.
                quote! {
                    // Chain extension with no functions is valid.
                },
                // Simple.
                quote! {
                    #[ink($id_arg_kind=1)]
                    fn my_extension();

                    #[ink($id_arg_kind=2)]
                    fn my_extension2();
                },
                // Input + output variations.
                quote! {
                    #[ink($id_arg_kind=1)]
                    fn my_extension();

                    #[ink($id_arg_kind=2)]
                    fn my_extension2(a: i32);

                    #[ink($id_arg_kind=3)]
                    fn my_extension3() -> bool;

                    #[ink($id_arg_kind=4)]
                    fn my_extension4(a: i32) -> bool;

                    #[ink($id_arg_kind=5)]
                    fn my_extension5(a: i32) -> (i32, u64, bool);

                    #[ink($id_arg_kind=6)]
                    fn my_extension6(a: i32, b: u64, c: [u8; 32]) -> bool;

                    #[ink($id_arg_kind=7)]
                    fn my_extension7(a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool);
                },
                // Handle status.
                quote! {
                    #[ink($id_arg_kind=1, handle_status=true)]
                    fn my_extension();

                    #[ink($id_arg_kind=2, handle_status=false)]
                    fn my_extension2(a: i32);

                    #[ink($id_arg_kind=3, handle_status=true)]
                    fn my_extension3() -> bool;

                    #[ink($id_arg_kind=4, handle_status=false)]
                    fn my_extension4(a: i32) -> bool;

                    #[ink($id_arg_kind=5, handle_status=true)]
                    fn my_extension5(a: i32) -> (i32, u64, bool);

                    #[ink($id_arg_kind=6, handle_status=false)]
                    fn my_extension6(a: i32, b: u64, c: [u8; 32]) -> bool;

                    #[ink($id_arg_kind=7, handle_status=true)]
                    fn my_extension7(a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool);
                },
            ]
            .iter()
            .flat_map(|extensions| {
                [
                    // Simple.
                    quote! {
                        #[ink::chain_extension$(($macro_args))?]
                        pub trait MyChainExtension {
                            type ErrorCode = MyErrorCode;

                            #extensions
                        }

                        #[$scale_derive_attr]
                        pub enum MyErrorCode {
                            InvalidKey,
                            CannotWriteToKey,
                            CannotReadFromKey,
                        }

                        impl ink::env::chain_extension::FromStatusCode for MyErrorCode {
                            fn from_status_code(status_code: u32) -> Result<(), Self> {
                                match status_code {
                                    0 => Ok(()),
                                    1 => Err(Self::InvalidKey),
                                    2 => Err(Self::CannotWriteToKey),
                                    3 => Err(Self::CannotReadFromKey),
                                    _ => panic!("encountered unknown status code"),
                                }
                            }
                        }
                    },
                ]
            })
        };
    }

    #[test]
    fn v5_extension_arg_works() {
        for code in valid_chain_extensions!(v5) {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let result = ensure_extension_arg(&chain_extension);
            assert!(result.is_none(), "chain extension: {code}");
        }
    }

    #[test]
    fn v5_missing_extension_arg_fails() {
        let code = quote_as_pretty_string! {
            #[ink::chain_extension]
            pub trait MyChainExtension {}
        };
        let chain_extension = parse_first_chain_extension(&code);

        let result = ensure_extension_arg(&chain_extension);

        // Verifies diagnostics.
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
        // Verifies quickfixes.
        let expected_quickfixes = [TestResultAction {
            label: "Add ink! extension",
            edits: vec![TestResultTextRange {
                text: "(extension = 1)",
                start_pat: Some("#[ink::chain_extension"),
                end_pat: Some("#[ink::chain_extension"),
            }],
        }];
        let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
        verify_actions(&code, quickfixes, &expected_quickfixes);
    }

    #[test]
    fn valid_trait_properties_works() {
        for code in valid_chain_extensions!() {
            let chain_extension = parse_first_chain_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            common::ensure_trait_invariants(
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
            common::ensure_trait_invariants(
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
        for (version, chain_extensions) in versioned_fixtures!(valid_chain_extensions) {
            for code in chain_extensions {
                let chain_extension = parse_first_chain_extension(quote_as_str! {
                    #code
                });

                let mut results = Vec::new();
                ensure_trait_item_invariants(&mut results, &chain_extension, version);
                assert!(
                    results.is_empty(),
                    "chain extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn invalid_trait_items_fails() {
        for (version, id_arg_name, id_arg_kind, macro_args) in [
            (
                Version::Legacy,
                "extension",
                quote! { extension },
                quote! {},
            ),
            (
                Version::V5(MinorVersion::Base),
                "function",
                quote! { function },
                quote! { (extension=1) },
            ),
        ] {
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
                // Non-flagged function.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L632-L652>.
                (
                    quote! {
                        fn non_flagged();
                    },
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: id_arg_name,
                            start_pat: Some("<-fn"),
                            end_pat: Some("<-fn"),
                        }],
                    }],
                ),
                // Default implementation.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L654-L663>.
                (
                    quote! {
                        #[ink(#id_arg_kind=1)]
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
                // Const function.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L665-L674>.
                (
                    quote! {
                        #[ink(#id_arg_kind=1)]
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
                // Async function.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L676-L685>.
                (
                    quote! {
                        #[ink(#id_arg_kind=1)]
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
                // Unsafe function.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L687-L696>.
                (
                    quote! {
                        #[ink(#id_arg_kind=1)]
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
                        #[ink(#id_arg_kind=1)]
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
                // Variadic function.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L709-L718>.
                (
                    quote! {
                        #[ink(#id_arg_kind=1)]
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
                // Generic function.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L720-L729>.
                (
                    quote! {
                        #[ink(#id_arg_kind=1)]
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
                        label: "Add",
                        edits: vec![
                            // Add ink! extension attribute.
                            TestResultTextRange {
                                text: id_arg_name,
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
                        label: "Add",
                        edits: vec![
                            // Add ink! extension/function attribute.
                            TestResultTextRange {
                                text: id_arg_name,
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
                        fn unknown_fn();
                    },
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![
                            // Add ink! extension/function attribute.
                            TestResultTextRange {
                                text: id_arg_name,
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
                        #[ink(#id_arg_kind=1)]
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
                        #[ink(#id_arg_kind=1)]
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
                        #[ink(#id_arg_kind=1)]
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
                        #[ink(#id_arg_kind=1)]
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
                    #[ink::chain_extension #macro_args]
                    pub trait MyChainExtension {
                        #items
                    }
                };
                let chain_extension = parse_first_chain_extension(&code);

                let mut results = Vec::new();
                ensure_trait_item_invariants(&mut results, &chain_extension, version);

                // Verifies diagnostics.
                assert_eq!(results.len(), 1, "chain extension: {items}");
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
        for (version, chain_extensions) in versioned_fixtures!(valid_chain_extensions) {
            for code in chain_extensions {
                let chain_extension = parse_first_chain_extension(quote_as_str! {
                    #code
                });

                let mut results = Vec::new();
                ensure_no_overlapping_ids(&mut results, &chain_extension, version);
                assert!(
                    results.is_empty(),
                    "chain extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L859-L870>.
    fn overlapping_ids_fails() {
        for (version, id_arg_kind, macro_args) in [
            (Version::Legacy, quote! { extension }, quote! {}),
            (
                Version::V5(MinorVersion::Base),
                quote! { function },
                quote! { (extension=1) },
            ),
        ] {
            for code in [
                // Overlapping decimal.
                quote! {
                    #[ink(#id_arg_kind=1)]
                    fn my_extension();

                    #[ink(#id_arg_kind=1)]
                    fn my_extension2();
                },
                // Overlapping hexadecimal.
                quote! {
                    #[ink(#id_arg_kind=0x1)]
                    fn my_extension();

                    #[ink(#id_arg_kind=0x1)]
                    fn my_extension2();
                },
                // Overlapping detected across decimal and hex representations.
                quote! {
                    #[ink(#id_arg_kind=1)]
                    fn my_extension();

                    #[ink(#id_arg_kind=0x1)]
                    fn my_extension2();
                },
            ] {
                let chain_extension = parse_first_chain_extension(quote_as_str! {
                    #[ink::chain_extension #macro_args]
                    pub trait MyChainExtension {
                        #code
                    }
                });

                let mut results = Vec::new();
                ensure_no_overlapping_ids(&mut results, &chain_extension, version);
                // 1 error the overlapping extension id.
                assert_eq!(
                    results.len(),
                    1,
                    "chain extension: {code}, version: {:?}",
                    version
                );
                // All diagnostics should be errors.
                assert_eq!(
                    results[0].severity,
                    Severity::Error,
                    "chain extension: {code}, version: {:?}",
                    version
                );
                // Verifies quickfixes.
                let quick_fix_label = &results[0].quickfixes.as_ref().unwrap()[0].label;
                assert!(
                    quick_fix_label.contains("Replace") && quick_fix_label.contains("unique"),
                    "chain extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for (version, chain_extensions) in versioned_fixtures!(valid_chain_extensions) {
            for code in chain_extensions {
                let chain_extension = parse_first_chain_extension(quote_as_str! {
                    #code
                });

                let mut results = Vec::new();
                ensure_valid_quasi_direct_ink_descendants(&mut results, &chain_extension, version);
                assert!(
                    results.is_empty(),
                    "chain extension: {code}, version: {:?}",
                    version
                );
            }
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
            }
        };
        let chain_extension = parse_first_chain_extension(&code);

        for version in [Version::Legacy, Version::V5(MinorVersion::Base)] {
            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &chain_extension, version);

            // 1 diagnostic each for `constructor` and `message`.
            assert_eq!(
                results.len(),
                2,
                "chain extension: {code}, version: {:?}",
                version
            );
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                2,
                "chain extension: {code}, version: {:?}",
                version
            );
            // Verifies quickfixes.
            let expected_quickfixes = [
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
            ];
            for (idx, item) in results.iter().enumerate() {
                let quickfixes = item.quickfixes.as_ref().unwrap();
                verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
            }
        }
    }

    #[test]
    fn compound_diagnostic_works() {
        for (version, chain_extensions) in versioned_fixtures!(valid_chain_extensions) {
            for code in chain_extensions {
                let chain_extension = parse_first_chain_extension(quote_as_str! {
                    #code
                });

                let mut results = Vec::new();
                diagnostics(&mut results, &chain_extension, version);
                assert!(
                    results.is_empty(),
                    "chain extension: {code}, version: {:?}",
                    version
                );
            }
        }
    }
}
