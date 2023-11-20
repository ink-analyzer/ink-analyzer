//! Utilities for ink! analysis.

use ink_analyzer_ir::ast::{HasAttrs, HasDocComments, HasModuleItem, HasName};
use ink_analyzer_ir::syntax::{
    AstNode, AstToken, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, TextSize,
};
use ink_analyzer_ir::{
    ast, Contract, InkArg, InkArgKind, InkArgValueKind, InkArgValueStringKind, InkAttribute,
    InkAttributeKind, InkEntity, InkImpl, InkMacroKind, IsInkStruct, IsInkTrait, Storage,
};
use itertools::Itertools;
use std::collections::HashSet;

use crate::utils;

/// Returns valid sibling ink! argument kinds for the given ink! attribute kind.
///
/// (i.e argument kinds that don't conflict with the given ink! attribute kind,
/// e.g for the `contract` attribute macro kind, this would be `env` and `keep_attr`
/// while for the `storage` attribute argument kind, this would be `default`, `payable` and `selector`).
pub fn valid_sibling_ink_args(attr_kind: InkAttributeKind) -> Vec<InkArgKind> {
    match attr_kind {
        // Returns valid sibling args (if any) for ink! attribute macros.
        InkAttributeKind::Macro(macro_kind) => {
            match macro_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L188-L197>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>.
                InkMacroKind::ChainExtension => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L111-L199>.
                InkMacroKind::Contract => vec![InkArgKind::Env, InkArgKind::KeepAttr],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/config.rs#L36-L59>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
                InkMacroKind::StorageItem => vec![InkArgKind::Derive],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27-L30>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
                InkMacroKind::Test => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/config.rs#L60-L85>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
                InkMacroKind::TraitDefinition => vec![InkArgKind::KeepAttr, InkArgKind::Namespace],
                // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L49-L85>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L41-L45>.
                InkMacroKind::E2ETest => vec![
                    InkArgKind::AdditionalContracts,
                    InkArgKind::Environment,
                    InkArgKind::KeepAttr,
                ],
                _ => Vec::new(),
            }
        }
        // Returns valid sibling args (if any) for ink! attribute arguments.
        // IR crate already makes sure `arg_kind` is the best match regardless of source code order,
        // See [`ink_analyzer_ir::ink_arg_kind_sort_order`] doc.
        InkAttributeKind::Arg(arg_kind) => {
            match arg_kind {
                // Unambiguous `arg_kind`.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L83-L93>.
                InkArgKind::Storage => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L88-L98>.
                InkArgKind::Event => vec![InkArgKind::Anonymous],
                InkArgKind::Anonymous => vec![InkArgKind::Event],
                InkArgKind::Topic => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L301-L315>.
                InkArgKind::Impl => vec![InkArgKind::Namespace],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L136-L148>.
                // Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/constructor.rs#L136-L149>.
                InkArgKind::Constructor => vec![
                    InkArgKind::Default,
                    // NOTE: While ink! docs "claim" that "ink! constructors are always implicitly payable and thus cannot be flagged as such",
                    // Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L316-L317>,
                    // the `ink_ir` crate currently accepts `payable` annotations for ink! constructors,
                    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L143>,
                    // so we follow the implementation (not the documentation) and thus allow `payable` annotations for ink! constructors.
                    InkArgKind::Payable,
                    InkArgKind::Selector,
                ],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L182-L194>.
                // Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/message.rs#L182-L195>.
                InkArgKind::Message => vec![
                    InkArgKind::Default,
                    InkArgKind::Payable,
                    InkArgKind::Selector,
                ],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
                InkArgKind::Env => vec![InkArgKind::KeepAttr],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L476-L487>.
                InkArgKind::Extension => vec![InkArgKind::HandleStatus],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/config.rs#L36-L59>.
                InkArgKind::Derive => Vec::new(),

                // Ambiguous `arg_kind`.
                // `keep_attr` is ambiguous because it can be used with both `contract` and `trait_definition` macros.
                // See `contract`, `trait_definition` and `env` patterns above for references.
                InkArgKind::KeepAttr => vec![InkArgKind::Env, InkArgKind::Namespace],
                // Similar to `keep_attr` above, `namespace` can be used with
                // `trait_definition` macro and `impl` argument.
                // But additionally, it can also be a standalone argument on an `impl` block as long as it's not a trait `impl` block.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L316-L321>.
                // See `trait_definition` and `impl` patterns above for more references.
                InkArgKind::Namespace => vec![InkArgKind::KeepAttr, InkArgKind::Impl],
                // See `extension` pattern above for references.
                InkArgKind::HandleStatus => vec![InkArgKind::Extension],
                // See `constructor` and `message` patterns above for references.
                InkArgKind::Payable => vec![
                    InkArgKind::Constructor,
                    InkArgKind::Default,
                    InkArgKind::Message,
                    InkArgKind::Selector,
                ],
                InkArgKind::Default => vec![
                    InkArgKind::Constructor,
                    InkArgKind::Message,
                    InkArgKind::Payable,
                    InkArgKind::Selector,
                ],
                InkArgKind::Selector => vec![
                    InkArgKind::Constructor,
                    InkArgKind::Default,
                    InkArgKind::Message,
                    InkArgKind::Payable,
                ],
                _ => Vec::new(),
            }
        }
    }
}

/// Returns valid quasi-direct descendant ink! argument kinds for the given ink! attribute kind.
///
/// (i.e argument kinds that are allowed in the scope of the given ink! attribute kind,
/// e.g for the `chain_extension` attribute macro kind, this would be `extension` and `handle_status`
/// while for the `event` attribute argument kind, this would be `topic`).
pub fn valid_quasi_direct_descendant_ink_args(attr_kind: InkAttributeKind) -> Vec<InkArgKind> {
    match attr_kind {
        // Returns valid quasi-direct descendant args (if any) for ink! attribute macros.
        InkAttributeKind::Macro(macro_kind) => {
            match macro_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L476-L487>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>.
                InkMacroKind::ChainExtension => {
                    vec![InkArgKind::Extension, InkArgKind::HandleStatus]
                }
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L58-L116>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L111-L199>.
                InkMacroKind::Contract => vec![
                    InkArgKind::Anonymous,
                    InkArgKind::Constructor,
                    InkArgKind::Default,
                    InkArgKind::Event,
                    InkArgKind::Impl,
                    InkArgKind::Message,
                    InkArgKind::Namespace,
                    InkArgKind::Payable,
                    InkArgKind::Selector,
                    InkArgKind::Storage,
                ],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/trait_item.rs#L85-L99>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L163-L164>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L290-L296>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
                InkMacroKind::TraitDefinition => vec![
                    InkArgKind::Default,
                    InkArgKind::Message,
                    InkArgKind::Payable,
                    InkArgKind::Selector,
                ],
                // ink! storage items, ink! tests and ink! e2e tests can't have ink! descendants.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/ir.rs#L37-L48>.
                _ => Vec::new(),
            }
        }
        // Returns valid quasi-direct descendant args (if any) for ink! attribute arguments.
        // IR crate already makes sure `arg_kind` is the best match regardless of source code order,
        // See [`ink_analyzer_ir::ink_arg_kind_sort_order`] doc.
        InkAttributeKind::Arg(arg_kind) => {
            match arg_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L132-L139>.
                InkArgKind::Event | InkArgKind::Anonymous => vec![InkArgKind::Topic],
                InkArgKind::Topic => Vec::new(),
                // `env` is used with the `contract` macro while `keep_attr` is ambiguous because
                // it can be used with both `contract` and `trait_definition` macro.
                // See `contract`, `trait_definition` patterns above for references.
                InkArgKind::Env | InkArgKind::KeepAttr => vec![
                    InkArgKind::Anonymous,
                    InkArgKind::Constructor,
                    InkArgKind::Default,
                    InkArgKind::Event,
                    InkArgKind::Impl,
                    InkArgKind::Message,
                    InkArgKind::Namespace,
                    InkArgKind::Payable,
                    InkArgKind::Selector,
                    InkArgKind::Storage,
                ],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L118-L216>.
                // `impl` can be used on `impl` blocks.
                // `namespace` be used with `trait_definition` macro and `impl` argument.
                // But additionally, `namespace` can also be a standalone argument on an `impl` block as long as it's not a trait `impl` block.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L316-L321>.
                // See `trait_definition` patterns above for more `namespace` references.
                InkArgKind::Impl | InkArgKind::Namespace => vec![
                    InkArgKind::Constructor,
                    InkArgKind::Default,
                    InkArgKind::Message,
                    InkArgKind::Payable,
                    InkArgKind::Selector,
                ],
                // All other ink! attribute arguments can't have ink! descendants.
                _ => Vec::new(),
            }
        }
    }
}

/// Returns valid quasi-direct descendant ink! macro kinds for the given ink! attribute kind.
///
/// (i.e macro kinds that are allowed in the scope of the given ink! attribute kind,
/// e.g for the `contract` attribute macro kind, this would be `chain_extension`, `storage_item`, `test` and `trait_definition`.
pub fn valid_quasi_direct_descendant_ink_macros(attr_kind: InkAttributeKind) -> Vec<InkMacroKind> {
    match attr_kind {
        // Returns valid quasi-direct descendant macros (if any) for ink! attribute macros.
        InkAttributeKind::Macro(macro_kind) => {
            match macro_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L111-L199>.
                InkMacroKind::Contract => vec![
                    InkMacroKind::ChainExtension,
                    InkMacroKind::StorageItem,
                    InkMacroKind::Test,
                    InkMacroKind::TraitDefinition,
                    InkMacroKind::E2ETest,
                ],
                // All other ink! attribute macros can't have ink! macro descendants.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/ir.rs#L37-L48>.
                _ => Vec::new(),
            }
        }
        // ink! attribute arguments can't have ink! macro descendants.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs>.
        InkAttributeKind::Arg(_) => Vec::new(),
    }
}

/// Returns valid ink! argument kinds for the given syntax kind.
///
/// (i.e argument kinds that can be applied to the given syntax kind,
/// e.g for the `impl` syntax kind, this would be `impl` and `namespace`.
pub fn valid_ink_args_by_syntax_kind(syntax_kind: SyntaxKind) -> Vec<InkArgKind> {
    match syntax_kind {
        // `env` and `keep_attr` can only be applied to a `mod` as siblings of an `ink::contract` macro.
        SyntaxKind::MODULE | SyntaxKind::MOD_KW => Vec::new(),
        // `keep_attr` and `namespace` can only be applied to a `trait` as siblings of an `ink::trait_definition` macro.
        SyntaxKind::TRAIT | SyntaxKind::TRAIT_KW => Vec::new(),
        // `derive` can only be applied to an ADT (`enum`, `struct` or `union`) as a sibling of an `ink::storage_item` macro.
        SyntaxKind::STRUCT | SyntaxKind::STRUCT_KW => vec![
            InkArgKind::Anonymous,
            InkArgKind::Event,
            InkArgKind::Storage,
        ],
        SyntaxKind::ENUM | SyntaxKind::ENUM_KW | SyntaxKind::UNION | SyntaxKind::UNION_KW => {
            Vec::new()
        }
        SyntaxKind::RECORD_FIELD => vec![InkArgKind::Topic],
        SyntaxKind::FN | SyntaxKind::FN_KW => vec![
            InkArgKind::Constructor,
            InkArgKind::Default,
            InkArgKind::Extension,
            InkArgKind::HandleStatus,
            InkArgKind::Message,
            InkArgKind::Payable,
            InkArgKind::Selector,
        ],
        SyntaxKind::IMPL | SyntaxKind::IMPL_KW => vec![InkArgKind::Impl, InkArgKind::Namespace],
        _ => Vec::new(),
    }
}

/// Returns valid ink! macro kinds for the given syntax kind.
///
/// (i.e macro kinds that can be applied to the given syntax kind,
/// e.g for the `module` syntax kind, this would be `contract`.
pub fn valid_ink_macros_by_syntax_kind(syntax_kind: SyntaxKind) -> Vec<InkMacroKind> {
    match syntax_kind {
        SyntaxKind::MODULE | SyntaxKind::MOD_KW => vec![InkMacroKind::Contract],
        SyntaxKind::TRAIT | SyntaxKind::TRAIT_KW => {
            vec![InkMacroKind::ChainExtension, InkMacroKind::TraitDefinition]
        }
        SyntaxKind::ENUM
        | SyntaxKind::ENUM_KW
        | SyntaxKind::STRUCT
        | SyntaxKind::STRUCT_KW
        | SyntaxKind::UNION
        | SyntaxKind::UNION_KW => vec![InkMacroKind::StorageItem],
        SyntaxKind::FN | SyntaxKind::FN_KW => vec![InkMacroKind::Test, InkMacroKind::E2ETest],
        _ => Vec::new(),
    }
}

/// Returns the primary ink! attribute candidate for the syntax node (if any),
/// a boolean flag indicating whether its the first ink! attribute.
///
/// (i.e returns either the first valid ink! attribute macro or the highest ranked ink! attribute argument,
/// see implementation of [`Ord`] for [`InkAttributeKind`] for details).
pub fn primary_ink_attribute_candidate(
    mut attrs: impl Iterator<Item = InkAttribute>,
) -> Option<(InkAttribute, bool)> {
    attrs.next().and_then(|first_attr| {
        let first_attr_range = first_attr.syntax().text_range();
        [first_attr]
            .into_iter()
            .chain(attrs)
            .filter(|attr| {
                // Ignore unknown attributes.
                !matches!(
                    attr.kind(),
                    InkAttributeKind::Macro(InkMacroKind::Unknown)
                        | InkAttributeKind::Arg(InkArgKind::Unknown)
                )
            })
            .sorted()
            .next()
            .map(|primary_candidate| {
                // Returns the best ranked valid ink! attribute
                // and a flag indicating whether or not its the first ink! attribute.
                let is_first = first_attr_range == primary_candidate.syntax().text_range();
                (primary_candidate, is_first)
            })
    })
}

/// Suggest primary attribute kinds in case the current one is either incomplete
/// (e.g `anonymous` without `event` or `derive` without `storage_item` attribute macro)
/// or ambiguous (e.g `selector` with neither `constructor` nor `message` or
/// `keep_attr` with neither `contract` nor `trait_definition` attribute macros).
pub fn primary_ink_attribute_kind_suggestions(
    attr_kind: InkAttributeKind,
) -> Vec<InkAttributeKind> {
    match attr_kind {
        InkAttributeKind::Arg(arg_kind) => {
            // Only ink! attribute arguments when set as the primary attribute have
            // the potential to be either incomplete or ambiguous.
            // See respective match pattern in the [`utils::valid_sibling_ink_args`] function for the rationale and references.
            match arg_kind {
                InkArgKind::Anonymous => vec![InkAttributeKind::Arg(InkArgKind::Event)],
                InkArgKind::KeepAttr => vec![
                    InkAttributeKind::Macro(InkMacroKind::Contract),
                    InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                ],
                InkArgKind::HandleStatus => vec![InkAttributeKind::Arg(InkArgKind::Extension)],
                InkArgKind::Namespace => vec![
                    InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                    InkAttributeKind::Arg(InkArgKind::Impl),
                ],
                InkArgKind::Payable | InkArgKind::Default | InkArgKind::Selector => vec![
                    InkAttributeKind::Arg(InkArgKind::Constructor),
                    InkAttributeKind::Arg(InkArgKind::Message),
                ],
                // Default
                _ => Vec::new(),
            }
        }
        // ink! attribute macros are always complete and unambiguous on their own.
        InkAttributeKind::Macro(_) => Vec::new(),
    }
}

/// Filters out duplicate ink! arguments from suggestions
/// (i.e ink! arguments that are already applied to the attribute's parent node).
pub fn remove_duplicate_ink_arg_suggestions(
    suggestions: &mut Vec<InkArgKind>,
    attr_parent: &SyntaxNode,
) {
    let already_annotated_ink_args: Vec<InkArgKind> = ink_analyzer_ir::ink_attrs(attr_parent)
        .flat_map(|ink_attr| ink_attr.args().to_owned())
        .map(|ink_arg| *ink_arg.kind())
        .collect();
    // Filters out duplicates.
    suggestions.retain(|arg_kind| !already_annotated_ink_args.contains(arg_kind));
}

/// Filters out duplicate ink! macros from suggestions
/// (i.e ink! macros that are already applied to the attribute's parent node).
pub fn remove_duplicate_ink_macro_suggestions(
    suggestions: &mut Vec<InkMacroKind>,
    attr_parent: &SyntaxNode,
) {
    let already_annotated_ink_macros: Vec<InkMacroKind> = ink_analyzer_ir::ink_attrs(attr_parent)
        .filter_map(|ink_attr| match ink_attr.kind() {
            InkAttributeKind::Macro(macro_kind) => Some(*macro_kind),
            InkAttributeKind::Arg(_) => None,
        })
        .collect();
    // Filters out duplicates.
    suggestions.retain(|arg_kind| !already_annotated_ink_macros.contains(arg_kind));
}

/// Filters out conflicting ink! arguments from suggestions
/// (i.e ink! arguments that aren't valid siblings of the best candidate for primary ink! attribute kind of the parent node).
pub fn remove_conflicting_ink_arg_suggestions(
    suggestions: &mut Vec<InkArgKind>,
    attr_parent: &SyntaxNode,
) {
    // Gets the primary ink! attribute candidate (if any).
    if let Some((primary_ink_attr, ..)) =
        primary_ink_attribute_candidate(ink_analyzer_ir::ink_attrs(attr_parent))
    {
        let valid_siblings = valid_sibling_ink_args(*primary_ink_attr.kind());
        // Filters out invalid siblings.
        suggestions.retain(|arg_kind| valid_siblings.contains(arg_kind));
    }
}

/// Filters out invalid ink! arguments from suggestions based on parent item's invariants.
///
/// (e.g ink! namespace arguments can't be applied to trait `impl` blocks).
pub fn remove_invalid_ink_arg_suggestions_for_parent_item(
    suggestions: &mut Vec<InkArgKind>,
    attr_parent: &SyntaxNode,
) {
    // Removes namespace suggestions for trait `impl` blocks.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/mod.rs#L316-L320>.
    if ast::Impl::can_cast(attr_parent.kind()) {
        let impl_item =
            ast::Impl::cast(attr_parent.clone()).expect("Should be able to cast to `impl` item.");
        if impl_item.trait_().is_some() {
            suggestions.retain(|arg_kind| *arg_kind != InkArgKind::Namespace);
        }
    }
}

/// Filters out invalid ink! arguments from suggestions based on parent ink! scope.
pub fn remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
    suggestions: &mut Vec<InkArgKind>,
    attr_parent: &SyntaxNode,
) {
    let parent_ink_scope_valid_ink_args: Vec<InkArgKind> =
        ink_analyzer_ir::ink_attrs_closest_ancestors(attr_parent)
            .flat_map(|attr| valid_quasi_direct_descendant_ink_args(*attr.kind()))
            .collect();

    // Filters out invalid arguments for the parent ink! scope (if any).
    if !parent_ink_scope_valid_ink_args.is_empty() {
        suggestions.retain(|arg_kind| {
            parent_ink_scope_valid_ink_args.is_empty()
                || parent_ink_scope_valid_ink_args.contains(arg_kind)
        });
    }
}

/// Filters out duplicate, conflicting and invalidly scoped ink! arguments.
/// (See [`remove_duplicate_ink_arg_suggestions`], [`remove_conflicting_ink_arg_suggestions`] and
/// [`remove_invalid_ink_arg_suggestions_for_parent_ink_scope`] docs).
pub fn remove_duplicate_conflicting_and_invalid_scope_ink_arg_suggestions(
    suggestions: &mut Vec<InkArgKind>,
    ink_attr: &InkAttribute,
) {
    if let Some(attr_parent) = ink_attr.syntax().parent() {
        // Filters out duplicate ink! attribute argument suggestions.
        remove_duplicate_ink_arg_suggestions(suggestions, &attr_parent);

        // Filters out conflicting ink! attribute argument actions.
        remove_conflicting_ink_arg_suggestions(suggestions, &attr_parent);

        // Filters out invalid (based on parent ink! scope) ink! attribute argument actions,
        // Doesn't apply to ink! attribute macros as their arguments are not influenced by the parent scope.
        if let InkAttributeKind::Arg(_) = ink_attr.kind() {
            remove_invalid_ink_arg_suggestions_for_parent_ink_scope(suggestions, &attr_parent);
        }
    }
}

/// Filters out invalid ink! macros from suggestions based on parent ink! scope.
pub fn remove_invalid_ink_macro_suggestions_for_parent_ink_scope(
    suggestions: &mut Vec<InkMacroKind>,
    attr_parent: &SyntaxNode,
) {
    let mut ink_ancestors = ink_analyzer_ir::ink_attrs_closest_ancestors(attr_parent);
    // Filters out invalid ink! macros for the parent ink! scope (if any).
    if let Some(first_ancestor) = ink_ancestors.next() {
        let parent_ink_scope_valid_ink_macros: Vec<InkMacroKind> = [first_ancestor]
            .into_iter()
            .chain(ink_ancestors)
            .flat_map(|attr| valid_quasi_direct_descendant_ink_macros(*attr.kind()))
            .collect();

        suggestions.retain(|macro_kind| {
            !parent_ink_scope_valid_ink_macros.is_empty()
                && parent_ink_scope_valid_ink_macros.contains(macro_kind)
        });
    }
}

/// Filters out invalid ink! macros from suggestions based on parent `cfg` scope.
pub fn remove_invalid_ink_macro_suggestions_for_parent_cfg_scope(
    suggestions: &mut Vec<InkMacroKind>,
    attr_parent: &SyntaxNode,
) {
    // Only suggest ink! test and ink! e2e test inside a "cfg test" scopes
    // (also check for an extra "e2e-tests" feature condition for ink! e2e test attributes).
    if suggestions
        .iter()
        .any(|macro_kind| matches!(macro_kind, InkMacroKind::Test | InkMacroKind::E2ETest))
    {
        let has_cfg_test_ancestors = attr_parent
            .ancestors()
            .filter(|ancestor| ancestor != attr_parent)
            .any(|node| ink_analyzer_ir::attrs(&node).any(|attr| is_cfg_test_attr(&attr)));
        let has_cfg_e2e_test_ancestors = has_cfg_test_ancestors
            && attr_parent
                .ancestors()
                .filter(|ancestor| ancestor != attr_parent)
                .any(|node| ink_analyzer_ir::attrs(&node).any(|attr| is_cfg_e2e_tests_attr(&attr)));

        suggestions.retain(|macro_kind| {
            !matches!(macro_kind, InkMacroKind::Test | InkMacroKind::E2ETest)
                || (*macro_kind == InkMacroKind::Test && has_cfg_test_ancestors)
                || (*macro_kind == InkMacroKind::E2ETest && has_cfg_e2e_test_ancestors)
        });
    }
}

/// Returns true if the attribute is a conditional compilation flag for test builds.
pub fn is_cfg_test_attr(attr: &ast::Attr) -> bool {
    attr.path().map_or(false, |path| path.to_string() == "cfg")
        && attr.token_tree().map_or(false, |token_tree| {
            let mut meta = token_tree.syntax().to_string();
            meta.retain(|it| !it.is_whitespace());
            meta.contains("(test") || meta.contains(",test")
        })
}

/// Returns true if the attribute is a conditional compilation flag for test builds
/// with an additional `e2e-tests` feature condition.
pub fn is_cfg_e2e_tests_attr(attr: &ast::Attr) -> bool {
    is_cfg_test_attr(attr)
        && attr.token_tree().map_or(false, |token_tree| {
            let mut meta = token_tree.syntax().to_string();
            meta.retain(|it| !it.is_whitespace());
            meta.contains(r#"feature="e2e-tests""#)
        })
}

/// Returns the insert text and snippet (if appropriate) for ink! attribute argument including
/// the `=` symbol after the ink! attribute argument name if necessary.
///
/// (i.e for `selector`, we return `"selector="` while for `payable`, we simply return `"payable"`)
pub fn ink_arg_insert_text(
    arg_kind: InkArgKind,
    insert_offset_option: Option<TextSize>,
    parent_attr_option: Option<&SyntaxNode>,
) -> (String, Option<String>) {
    // Determines whether or not to insert the `=` symbol after the ink! attribute argument name.
    let insert_equal_token = match InkArgValueKind::from(arg_kind) {
        // No `=` symbol is inserted after ink! attribute arguments that should not have a value.
        InkArgValueKind::None => false,
        // Adds an `=` symbol after the ink! attribute argument name if an `=` symbol is not
        // the next closest non-trivia token after the insert offset.
        _ => parent_attr_option
            .zip(insert_offset_option)
            .and_then(|(parent_node, insert_offset)| {
                parent_node
                    .token_at_offset(insert_offset)
                    .right_biased()
                    .and_then(|token| {
                        // Finds the next non-trivia token.
                        let is_next_non_trivia_token = |subject: &SyntaxToken| {
                            subject.text_range().start() >= insert_offset
                                && !subject.kind().is_trivia()
                        };
                        let next_non_trivia_token = if is_next_non_trivia_token(&token) {
                            Some(token)
                        } else {
                            ink_analyzer_ir::closest_item_which(
                                &token,
                                SyntaxToken::next_token,
                                is_next_non_trivia_token,
                                is_next_non_trivia_token,
                            )
                        };
                        next_non_trivia_token.map(|next_token| match next_token.kind() {
                            SyntaxKind::EQ => false,
                            // Adds an `=` symbol only if the next closest non-trivia token is not an `=` symbol.
                            _ => true,
                        })
                    })
            })
            // Defaults to inserting the `=` symbol (e.g. if either parent attribute is `None` or the next closest non-trivia token can't be determined).
            .unwrap_or(true),
    };
    let mut text = format!("{arg_kind}{}", if insert_equal_token { " = " } else { "" });
    // Creates (if appropriate) a snippet with tab stops and/or placeholders where applicable.
    let snippet = insert_equal_token.then_some(format!(
        "{text}{}",
        match InkArgValueKind::from(arg_kind) {
            InkArgValueKind::U32 | InkArgValueKind::U32OrWildcard => "${1:1}",
            InkArgValueKind::String(str_kind) => match str_kind {
                InkArgValueStringKind::Identifier => r#""${1:my_namespace}""#,
                _ => r#""$1""#,
            },
            InkArgValueKind::Bool => "${1:true}",
            InkArgValueKind::Path(_) => "${1:crate::}",
            // Should not be able to get here.
            InkArgValueKind::None => "",
        }
    ));
    // Add default values to insert text (if appropriate).
    text = format!(
        "{text}{}",
        if insert_equal_token {
            match InkArgValueKind::from(arg_kind) {
                InkArgValueKind::U32 | InkArgValueKind::U32OrWildcard => "1",
                InkArgValueKind::String(str_kind) => match str_kind {
                    InkArgValueStringKind::Identifier => r#""my_namespace""#,
                    _ => r#""""#,
                },
                InkArgValueKind::Bool => "true",
                InkArgValueKind::Path(_) => "crate::",
                // Should not be able to get here.
                InkArgValueKind::None => "",
            }
        } else {
            ""
        }
    );

    (text, snippet)
}

/// Returns the insert offset for an ink! attribute.
pub fn ink_attribute_insert_offset(node: &SyntaxNode) -> TextSize {
    ink_analyzer_ir::ink_attrs(node)
        // Finds the last ink! attribute.
        .last()
        .as_ref()
        .map(InkAttribute::syntax)
        // Finds the last attribute.
        .or(node
            .children()
            .filter_map(ast::Attr::cast)
            .last()
            .as_ref()
            .map(ast::Attr::syntax))
        // First the last token for last ink! attribute or generic attribute (if any).
        .and_then(SyntaxNode::last_token)
        // Otherwise finds the first trivia/rustdoc token for item (if any).
        .or(node
            .first_token()
            .filter(|first_token| first_token.kind().is_trivia()))
        // Finds the first non-(attribute/rustdoc/trivia) token for the item.
        .and_then(|it| ink_analyzer_ir::closest_non_trivia_token(&it, SyntaxToken::next_token))
        .as_ref()
        // Defaults to the start of the node.
        .map_or(node.text_range(), SyntaxToken::text_range)
        .start()
}

/// Returns the insert offset and affixes (i.e whitespace and delimiters e.g `(`, `,` and `)`) for an ink! attribute argument .
///
/// **NOTE**: For attributes that have values (e.g `selector = 1`), the equal symbol (`=`)
/// and the value are considered part of the attribute argument (not suffixes),
/// so they're not handled by this function. See [`ink_arg_insert_text`] doc instead.
pub fn ink_arg_insert_offset_and_affixes(
    ink_attr: &InkAttribute,
    arg_kind_option: Option<InkArgKind>,
) -> Option<(TextSize, Option<&str>, Option<&str>)> {
    // Determines if its a "primary" attribute argument
    // as those get inserted at the beginning of the argument list while everything else gets inserted at the end.
    let is_primary = arg_kind_option
        .as_ref()
        .map_or(false, InkArgKind::is_entity_type);

    // Only computes insert context for closed attributes because
    // unclosed attributes are too tricky for useful contextual edits.
    ink_attr.ast().r_brack_token().map(|r_bracket| {
        ink_attr.ast().token_tree().as_ref().map_or(
            (r_bracket.text_range().start(), Some("("), Some(")")),
            |token_tree| {
                (
                    // Computes the insert offset.
                    if is_primary {
                        // "Primary" attribute argument get inserted at the beginning of the argument list.
                        token_tree
                            .l_paren_token()
                            // Inserts just after left parenthesis if it exists, otherwise defaults to the end of the attribute.
                            .map_or(token_tree.syntax().text_range().end(), |r_paren| {
                                r_paren.text_range().end()
                            })
                    } else {
                        // Other attribute arguments get inserted at the end of the argument list.
                        token_tree
                            .r_paren_token()
                            // Inserts just before right parenthesis if it exists, otherwise defaults to the end of the attribute.
                            .map_or(token_tree.syntax().text_range().end(), |r_paren| {
                                r_paren.text_range().start()
                            })
                    },
                    // Determines the prefix to insert before the ink! attribute argument text.
                    match token_tree.l_paren_token() {
                        Some(_) => {
                            if is_primary {
                                // No prefix for "primary" attribute arguments that already have a left parenthesis before them.
                                None
                            } else {
                                // Determines prefix for "non-primary" attribute arguments that already have a left parenthesis before them.
                                token_tree
                                    .r_paren_token()
                                    .and_then(|r_paren| {
                                        r_paren.prev_token().and_then(|penultimate_token| {
                                            match penultimate_token.kind() {
                                                SyntaxKind::COMMA | SyntaxKind::L_PAREN => None,
                                                // Adds a comma if the token before the right parenthesis is
                                                // neither a comma nor the left parenthesis.
                                                _ => Some(", "),
                                            }
                                        })
                                    })
                                    .or(token_tree.syntax().last_token().and_then(|last_token| {
                                        match last_token.kind() {
                                            SyntaxKind::COMMA
                                            | SyntaxKind::L_PAREN
                                            | SyntaxKind::R_PAREN => None,
                                            // Adds a comma if there is no right parenthesis and the last token is
                                            // neither a comma nor the left parenthesis
                                            // (the right parenthesis in the pattern above will likely never match anything,
                                            // but parsers are weird :-) so we leave it for robustness? and clarity).
                                            _ => Some(", "),
                                        }
                                    }))
                            }
                        }
                        // Adds a left parenthesis if none already exists.
                        None => Some("("),
                    },
                    // Determines the suffix to insert after the ink! attribute argument text.
                    match token_tree.r_paren_token() {
                        Some(_) => {
                            if is_primary {
                                // Determines suffix for "primary" attribute arguments that already have a right parenthesis after them.
                                token_tree
                                    .l_paren_token()
                                    .and_then(|l_paren| {
                                        l_paren.next_token().and_then(|first_token| {
                                            match first_token.kind() {
                                                SyntaxKind::COMMA | SyntaxKind::R_PAREN => None,
                                                // Adds a comma if the token after the left parenthesis is
                                                // neither a comma nor the right parenthesis.
                                                _ => Some(", "),
                                            }
                                        })
                                    })
                                    .or(token_tree.syntax().first_token().and_then(|first_token| {
                                        match first_token.kind() {
                                            SyntaxKind::COMMA
                                            | SyntaxKind::L_PAREN
                                            | SyntaxKind::R_PAREN => None,
                                            // Adds a comma if there is no left parenthesis and the first token is
                                            // neither a comma nor the right parenthesis
                                            // (the left parenthesis in the pattern above will likely never match anything,
                                            // but parsers are weird :-) so we leave it for robustness? and clarity).
                                            _ => Some(", "),
                                        }
                                    }))
                            } else {
                                // No suffix for "non-primary" attribute arguments that already have a right parenthesis after them.
                                None
                            }
                        }
                        // Adds a right parenthesis if none already exists.
                        None => Some(")"),
                    },
                )
            },
        )
    })
}

/// Returns the insert offset for the first ink! attribute.
pub fn first_ink_attribute_insert_offset(node: &SyntaxNode) -> TextSize {
    ink_analyzer_ir::ink_attrs(node)
        // Finds the first ink! attribute.
        .next()
        .map_or(ink_attribute_insert_offset(node), |it| {
            it.syntax().text_range().start()
        })
}

/// Returns the insert offset and affixes (e.g whitespace to preserve formatting) for the first ink! attribute argument.
pub fn first_ink_arg_insert_offset_and_affixes(
    ink_attr: &InkAttribute,
) -> Option<(TextSize, Option<&str>, Option<&str>)> {
    ink_attr
        .args()
        .first()
        .map(|arg| {
            // Insert before the first argument (if present).
            (arg.text_range().start(), None, Some(", "))
        })
        .or(ink_attr
            .ast()
            .token_tree()
            .as_ref()
            .and_then(|token_tree| Some(token_tree).zip(token_tree.l_paren_token()))
            .map(|(token_tree, l_paren)| {
                // Otherwise, insert after left parenthesis (if present).
                (
                    l_paren.text_range().end(),
                    None,
                    match token_tree.r_paren_token() {
                        Some(_) => None,
                        None => Some(")"),
                    },
                )
            }))
        .or(ink_attr
            .ast()
            .token_tree()
            .as_ref()
            .and_then(|token_tree| Some(token_tree).zip(token_tree.r_paren_token()))
            .map(|(token_tree, r_paren)| {
                // Otherwise, insert before right parenthesis (if present).
                (
                    r_paren.text_range().start(),
                    match token_tree.l_paren_token() {
                        Some(_) => None,
                        None => Some("("),
                    },
                    None,
                )
            }))
        .or(ink_attr.ast().r_brack_token().map(|r_bracket| {
            // Otherwise, insert before right bracket (if present).
            (r_bracket.text_range().start(), Some("("), Some(")"))
        }))
}

/// Returns the indenting (preceding whitespace) of the syntax node.
pub fn item_indenting(node: &SyntaxNode) -> Option<String> {
    node.prev_sibling_or_token().and_then(|prev_elem| {
        (prev_elem.kind() == SyntaxKind::WHITESPACE)
            .then_some(end_indenting(prev_elem.to_string().as_str()))
    })
}

/// Returns the indenting at the end of string of whitespace.
///
/// NOTE: This function doesn't verify that the input is actually whitespace.
pub fn end_indenting(whitespace: &str) -> String {
    whitespace
        .chars()
        .rev()
        .take_while(|char| *char != '\n' && char.is_whitespace())
        .collect()
}

/// Returns appropriate indenting (preceding whitespace) for the syntax node's children.
pub fn item_children_indenting(node: &SyntaxNode) -> String {
    item_indenting(node)
        .and_then(|ident| (!ident.is_empty()).then_some(format!("{ident}{ident}")))
        .unwrap_or("    ".to_string())
}

/// Returns the deepest syntax element that fully covers text range (if any).
pub fn focused_element<T: InkEntity>(item: &T, range: TextRange) -> Option<SyntaxElement> {
    if range.is_empty() {
        // Uses item at offset utility if the range start and end are equal.
        item.item_at_offset(range.start())
            .focused_token()
            .cloned()
            .map(SyntaxElement::Token)
    } else {
        item.syntax()
            .text_range()
            // Ensure the text range is in the bounds of the source code.
            .contains_range(range)
            .then(|| {
                // Retrieves deepest element that fully covers the text range.
                item.syntax().covering_element(range)
            })
    }
}

/// Returns the covering attribute for the text range (if any).
pub fn covering_attribute<T: InkEntity>(item: &T, range: TextRange) -> Option<ast::Attr> {
    if range.is_empty() {
        // Uses item at offset utility if the range start and end are equal.
        // This way we keep some of the guarantees about parent AST items for unclosed attributes that
        // the item at offset utility enforces.
        item.item_at_offset(range.start()).parent_attr()
    } else {
        // Retrieves deepest element that fully covers the text range.
        focused_element(item, range).and_then(|covering_element| {
            if ast::Attr::can_cast(covering_element.kind()) {
                // Casts covering element to `ast::Attr` node if it's an attribute.
                covering_element.into_node().and_then(ast::Attr::cast)
            } else {
                // Finds the parent attribute (if any) of the covering element.
                ink_analyzer_ir::closest_ancestor_ast_type::<SyntaxElement, ast::Attr>(
                    &covering_element,
                )
            }
        })
    }
}

/// Returns the covering ink! attribute for the text range (if any).
pub fn covering_ink_attribute<T: InkEntity>(item: &T, range: TextRange) -> Option<InkAttribute> {
    covering_attribute(item, range).and_then(InkAttribute::cast)
}

/// Returns the parent AST item for the text range (if any).
pub fn parent_ast_item<T: InkEntity>(item: &T, range: TextRange) -> Option<ast::Item> {
    if range.is_empty() {
        // Uses item at offset utility if the range start and end are equal.
        // This way we keep some of the guarantees about parent AST items for unclosed attributes that
        // the item at offset utility enforces.
        item.item_at_offset(range.start()).parent_ast_item()
    } else {
        // Retrieves deepest element that fully covers the text range.
        focused_element(item, range).and_then(|covering_element| {
            if ast::Item::can_cast(covering_element.kind()) {
                // Casts covering element to `ast::Item` node if it's an AST item.
                covering_element.into_node().and_then(ast::Item::cast)
            } else {
                // Finds the parent AST item (if any) of the covering element.
                ink_analyzer_ir::parent_ast_item(&covering_element)
            }
        })
    }
}

/// Returns text range of the AST item "declaration" (i.e tokens between meta - attributes/rustdoc - and the start of the item list).
pub fn ast_item_declaration_range(item: &ast::Item) -> Option<TextRange> {
    match item {
        ast::Item::Module(module) => module
            .item_list()
            .map(|it| {
                it.l_curly_token()
                    .as_ref()
                    .map_or(it.syntax().text_range(), SyntaxToken::text_range)
            })
            .or(module
                .semicolon_token()
                .as_ref()
                .map(SyntaxToken::text_range)),
        ast::Item::Trait(trait_item) => trait_item.assoc_item_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map_or(it.syntax().text_range(), SyntaxToken::text_range)
        }),
        ast::Item::Impl(impl_item) => impl_item.assoc_item_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map_or(it.syntax().text_range(), SyntaxToken::text_range)
        }),
        ast::Item::Fn(fn_item) => fn_item
            .body()
            .map(|it| {
                it.stmt_list().map_or(it.syntax().text_range(), |it| {
                    it.l_curly_token()
                        .as_ref()
                        .map_or(it.syntax().text_range(), SyntaxToken::text_range)
                })
            })
            .or(fn_item
                .semicolon_token()
                .as_ref()
                .map(SyntaxToken::text_range)),
        ast::Item::Enum(enum_item) => enum_item.variant_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map_or(it.syntax().text_range(), SyntaxToken::text_range)
        }),
        ast::Item::Struct(struct_item) => struct_item
            .field_list()
            .map(|it| {
                match &it {
                    ast::FieldList::RecordFieldList(it) => {
                        it.l_curly_token().as_ref().map(SyntaxToken::text_range)
                    }
                    ast::FieldList::TupleFieldList(it) => {
                        struct_item
                            .semicolon_token()
                            .as_ref()
                            .map(SyntaxToken::text_range)
                            // should be end.
                            .or(it.r_paren_token().as_ref().map(SyntaxToken::text_range))
                            // should be end.
                            .or(Some(it.syntax().text_range()))
                    }
                }
                .unwrap_or(it.syntax().text_range())
            })
            .or(struct_item
                .semicolon_token()
                .as_ref()
                .map(SyntaxToken::text_range)),
        ast::Item::Union(union_item) => union_item.record_field_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map_or(it.syntax().text_range(), SyntaxToken::text_range)
        }),
        ast::Item::TypeAlias(type_alias) => type_alias
            .semicolon_token()
            .as_ref()
            .map(SyntaxToken::text_range),
        _ => None,
    }
    .map(TextRange::end)
    .map(|end| {
        let last_comment = item.doc_comments().last().map(|it| it.syntax().clone());
        let last_attr_token = item.attrs().last().and_then(|it| it.syntax().last_token());
        let start = last_comment
            .as_ref()
            .zip(last_attr_token.as_ref())
            .map(|(comment, attr)| {
                if comment.text_range().end() >= attr.text_range().end() {
                    comment.clone()
                } else {
                    attr.clone()
                }
            })
            .or(last_comment)
            .or(last_attr_token)
            // Finds the first non-(attribute/rustdoc/trivia) token for the item.
            .and_then(|it| ink_analyzer_ir::closest_non_trivia_token(&it, SyntaxToken::next_token))
            .as_ref()
            // Defaults to the start of the item.
            .map_or(item.syntax().text_range(), SyntaxToken::text_range)
            .start();

        // Returns the text range for the item's "declaration".
        TextRange::new(start, end)
    })
}

/// Returns the terminal syntax token of the AST item "declaration" (i.e the right curly bracket `}` or semi-colon `;`).
pub fn ast_item_terminal_token(item: &ast::Item) -> Option<SyntaxToken> {
    match item {
        ast::Item::Module(module) => module
            .item_list()
            .and_then(|it| it.r_curly_token())
            .or(module.semicolon_token()),
        ast::Item::Trait(trait_item) => trait_item
            .assoc_item_list()
            .and_then(|it| it.r_curly_token()),
        ast::Item::Impl(impl_item) => impl_item
            .assoc_item_list()
            .and_then(|it| it.r_curly_token()),
        ast::Item::Fn(fn_item) => fn_item
            .body()
            .and_then(|it| it.stmt_list().and_then(|it| it.r_curly_token()))
            .or(fn_item.semicolon_token()),
        ast::Item::Enum(enum_item) => enum_item.variant_list().and_then(|it| it.r_curly_token()),
        ast::Item::Struct(struct_item) => struct_item
            .field_list()
            .and_then(|it| {
                match &it {
                    ast::FieldList::RecordFieldList(it) => it.r_curly_token(),
                    ast::FieldList::TupleFieldList(it) => {
                        struct_item
                            .semicolon_token()
                            // should be end.
                            .or(it.r_paren_token())
                    }
                }
            })
            .or(struct_item.semicolon_token()),
        ast::Item::Union(union_item) => union_item
            .record_field_list()
            .and_then(|it| it.r_curly_token()),
        ast::Item::TypeAlias(type_alias) => type_alias.semicolon_token(),
        _ => None,
    }
}

/// Returns text range of the syntax token and it's immediate (next) trivia (whitespace and comments).
pub fn token_and_trivia_range(token: &SyntaxToken) -> TextRange {
    TextRange::new(
        token.text_range().start(),
        // Either the start of the next non-trivia token or the end of the target token.
        ink_analyzer_ir::closest_non_trivia_token(token, SyntaxToken::next_token)
            .map_or(token.text_range().end(), |it| it.text_range().start()),
    )
}

/// Returns text range of the syntax node and it's immediate (next) trivia (whitespace and comments).
pub fn node_and_trivia_range(node: &SyntaxNode) -> TextRange {
    TextRange::new(
        node.text_range().start(),
        // Either the start of the next non-trivia token or the end of the target node.
        node.last_token()
            .as_ref()
            .map_or(node.text_range(), token_and_trivia_range)
            .end(),
    )
}

/// Returns text range of the syntax token and it's immediate (next or previous) delimiter (e.g comma - ",").
pub fn token_and_delimiter_range(token: &SyntaxToken, delimiter: SyntaxKind) -> TextRange {
    // Gets the next delimiter (if any).
    let next_delimiter = ink_analyzer_ir::closest_item_which(
        token,
        SyntaxToken::next_token,
        |subject| subject.kind() == delimiter,
        |subject| !subject.kind().is_trivia(),
    );
    TextRange::new(
        // Either the start of the previous delimiter token (if any and there's no next delimiter)
        // or the start of the target token.
        next_delimiter
            .is_none()
            .then(|| {
                // Returns the previous delimiter token (if any).
                ink_analyzer_ir::closest_item_which(
                    token,
                    SyntaxToken::prev_token,
                    |subject| subject.kind() == delimiter,
                    |subject| !subject.kind().is_trivia(),
                )
            })
            .flatten()
            .as_ref()
            .unwrap_or(token)
            .text_range()
            .start(),
        // Either the end of the next delimiter token (if any) or the end of the target token.
        next_delimiter
            .as_ref()
            .map_or(token.text_range(), SyntaxToken::text_range)
            .end(),
    )
}

/// Returns text range of the syntax node and it's immediate (next or previous) delimiter (e.g comma - ",").
pub fn node_and_delimiter_range(node: &SyntaxNode, delimiter: SyntaxKind) -> TextRange {
    // Gets the end position.
    let end = node
        .last_token()
        .as_ref()
        .map_or(node.text_range(), |token| {
            token_and_delimiter_range(token, delimiter)
        })
        .end();
    TextRange::new(
        // Either the start of the previous delimiter token (if any and there's no next delimiter)
        // or the start of the target node.
        (end == node.text_range().end())
            .then(|| {
                // Returns a text range including previous delimiter token (if any).
                // Previous is implied because we know there's no next delimiter
                // because `end == node.text_range().end()`.
                node.first_token()
                    .map(|token| token_and_delimiter_range(&token, delimiter))
            })
            .flatten()
            .unwrap_or(node.text_range())
            .start(),
        // Either the end of the next delimiter token or the end of the target node.
        end,
    )
}

/// Returns text range of the ink! attribute argument and it's immediate (next) delimiter (i.e comma - ",").
pub fn ink_arg_and_delimiter_removal_range(
    arg: &InkArg,
    parent_attr_option: Option<&InkAttribute>,
) -> TextRange {
    // Gets the last token of the ink! attribute argument (if any).
    let last_token_option = arg
        .meta()
        // Argument value.
        .value()
        .option()
        .and_then(|result| {
            match result {
                Ok(value) => value.elements(),
                Err(elements) => elements,
            }
            .last()
        })
        .and_then(|elem| match elem {
            SyntaxElement::Node(node) => node.last_token(),
            SyntaxElement::Token(token) => Some(token.clone()),
        })
        // Equal token ("=") if no value is present.
        .or(arg.meta().eq().map(|eq| eq.syntax().clone()))
        // Last token of argument name if no value nor equal symbol is present.
        .or(arg.meta().name().option().and_then(|result| match result {
            Ok(name) => Some(name.syntax().clone()),
            Err(elements) => elements.last().and_then(|elem| match elem {
                SyntaxElement::Node(node) => node.last_token(),
                SyntaxElement::Token(token) => Some(token.clone()),
            }),
        }));

    // Determines the parent attribute for the argument.
    if let Some(attr) = parent_attr_option.cloned().or(last_token_option
        .as_ref()
        .and_then(|token| {
            ink_analyzer_ir::closest_ancestor_ast_type::<SyntaxToken, ast::Attr>(token)
        })
        .and_then(InkAttribute::cast))
    {
        if attr.args().len() == 1 {
            match attr.kind() {
                // Returns the text range for the attribute meta (if possible) if the attribute has only a single argument.
                InkAttributeKind::Macro(_) => {
                    if let Some(token_tree) =
                        attr.ast().meta().as_ref().and_then(ast::Meta::token_tree)
                    {
                        return token_tree.syntax().text_range();
                    }
                }
                // Returns the text range for the whole attribute if the ink! attribute argument represents the entire attribute.
                InkAttributeKind::Arg(_) => {
                    return attr.syntax().text_range();
                }
            }
        }
    }

    // Gets the end position.
    let end = last_token_option
        .as_ref()
        .map_or(arg.text_range(), |token| {
            token_and_delimiter_range(token, SyntaxKind::COMMA)
        })
        .end();

    // Returns the text range of attribute argument + delimiter (if any) .
    TextRange::new(
        (end == arg.text_range().end())
            .then(|| {
                // Gets the first token of the ink! attribute argument (if any).
                arg.meta()
                    // Argument name.
                    .name()
                    .option()
                    .and_then(|result| match result {
                        Ok(name) => Some(name.syntax().clone()),
                        Err(elements) => elements.last().and_then(|elem| match elem {
                            SyntaxElement::Node(node) => node.first_token(),
                            SyntaxElement::Token(token) => Some(token.clone()),
                        }),
                    })
                    // Equal token ("=") if no name is present.
                    .or(arg.meta().eq().map(|eq| eq.syntax().clone()))
                    // First token argument value if no name nor equal symbol is present.
                    .or(arg
                        .meta()
                        .value()
                        .option()
                        .and_then(|result| {
                            match result {
                                Ok(value) => value.elements(),
                                Err(elements) => elements,
                            }
                            .first()
                        })
                        .and_then(|elem| match elem {
                            SyntaxElement::Node(node) => node.first_token(),
                            SyntaxElement::Token(token) => Some(token.clone()),
                        }))
            })
            .flatten()
            .as_ref()
            // Returns a text range including previous delimiter token (if any).
            // Previous is implied because we know there's no next delimiter
            // because `end == arg.text_range().end()`.
            .map_or(arg.text_range(), |token| {
                token_and_delimiter_range(token, SyntaxKind::COMMA)
            })
            .start(),
        // Either the end of the next delimiter token or the end of the ink! attribute argument.
        end,
    )
}

/// Returns the offset for the beginning of an item list (e.g body of an AST item - i.e `mod` e.t.c).
pub fn item_insert_offset_start(item_list: &ast::ItemList) -> TextSize {
    item_list
        .items()
        // Determines position after the last `use` item in the item list.
        .filter(|it| matches!(it, ast::Item::Use(_)))
        .last()
        .map(|it| it.syntax().text_range().end())
        // Determines position after the left curly bracket.
        .or(item_list.l_curly_token().map(|it| it.text_range().end()))
        // Defaults to inserts before the first item in the item list.
        .or(item_list
            .items()
            .next()
            .map(|it| it.syntax().text_range().start()))
        .unwrap_or(item_list.syntax().text_range().start())
}

/// Returns the offset for the end of an item list (e.g body of an AST item - i.e `mod` e.t.c).
pub fn item_insert_offset_end(item_list: &ast::ItemList) -> TextSize {
    item_list
        // Determines position after the last item in the item list.
        .items()
        .last()
        // Defaults to the start if item list is empty because it's easier to apply additional formatting that way.
        .map_or(item_insert_offset_start(item_list), |it| {
            it.syntax().text_range().end()
        })
}

/// Returns the offset after the end of the last `struct` (if any) in an item list (e.g body of an AST item - i.e `mod` e.t.c).
/// Defaults to the beginning of the item list if no `struct`s are present.
pub fn item_insert_offset_after_last_struct_or_start(item_list: &ast::ItemList) -> TextSize {
    item_list
        .items()
        .filter(|it| matches!(it, ast::Item::Struct(_)))
        .last()
        .map_or(item_insert_offset_start(item_list), |it| {
            it.syntax().text_range().end()
        })
}

/// Returns the offset after the end of the last `impl` (if any), or the end of the last `struct` (if any),
/// or the beginning of the first `mod` (if any) in an item list (e.g body of an AST item - i.e `mod` e.t.c).
/// Defaults to the end of the item list if no `impl`s, `struct`s nor `mod`s are present.
pub fn item_insert_offset_impl(item_list: &ast::ItemList) -> TextSize {
    item_list
        .items()
        // After last `impl` block.
        .filter(|it| matches!(it, ast::Item::Impl(_)))
        .last()
        // Or after last `struct` item.
        .or(item_list
            .items()
            .filter(|it| matches!(it, ast::Item::Struct(_)))
            .last())
        .map(|it| it.syntax().text_range().end())
        // Or before the first `mod` item.
        .or(item_list
            .items()
            .find(|it| matches!(it, ast::Item::Module(_)))
            .map(|it| it.syntax().text_range().start()))
        // Defaults to the end of the item list.
        .unwrap_or(item_insert_offset_end(item_list))
}

/// Returns the offset for inserting an item into an item list (e.g body of an AST item - i.e `mod` e.t.c.) based on the ink! entity name.
pub fn item_insert_offset_by_scope_name(
    item_list: &ast::ItemList,
    ink_scope_name: &str,
) -> TextSize {
    match ink_scope_name {
        // ink! storage is inserted at the beginning of the item list.
        "storage" => item_insert_offset_start(item_list),
        // ink! events are inserted either after the last `struct` (if any) or at the beginning of the item list.
        "event" => item_insert_offset_after_last_struct_or_start(item_list),
        // ink! `impl` are inserted either after the last `impl` (if any), or after the last `struct` (if any),
        // or before the fist `mod` (if any), or at the end of the item list.
        "impl" => item_insert_offset_impl(item_list),
        // Everything else is inserted at the end of the item list.
        _ => item_insert_offset_end(item_list),
    }
}

/// Returns the offset for the beginning of an associated item list (e.g body of an AST item - i.e `fn`, `trait` e.t.c).
pub fn assoc_item_insert_offset_start(assoc_item_list: &ast::AssocItemList) -> TextSize {
    assoc_item_list
        .l_curly_token()
        .map(|it| it.text_range().end())
        .or(assoc_item_list
            .assoc_items()
            .next()
            .map(|it| it.syntax().text_range().start()))
        .unwrap_or(assoc_item_list.syntax().text_range().start())
}

/// Returns the offset for the end of an associated item list (e.g body of an AST item - i.e `fn`, `trait` e.t.c).
pub fn assoc_item_insert_offset_end(assoc_item_list: &ast::AssocItemList) -> TextSize {
    assoc_item_list
        // Determines position after the last item in the associated item list.
        .assoc_items()
        .last()
        // Defaults to the start if associated item list is empty because it's easier to apply additional formatting that way.
        .map_or(assoc_item_insert_offset_start(assoc_item_list), |it| {
            it.syntax().text_range().end()
        })
}

/// Returns the offset for the beginning of a field list and affixes (prefix and suffix).
pub fn field_insert_offset_start_and_affixes(
    field_list: &ast::FieldList,
) -> (TextSize, Option<String>, Option<String>) {
    match field_list {
        ast::FieldList::RecordFieldList(record_field_list) => record_field_list
            .l_curly_token()
            .map(|it| (it.text_range().end(), None, None))
            .or(record_field_list.fields().next().map(|it| {
                (
                    it.syntax().text_range().start(),
                    None,
                    Some(format!(
                        "\n{}",
                        item_children_indenting(field_list.syntax())
                    )),
                )
            }))
            .unwrap_or((record_field_list.syntax().text_range().start(), None, None)),
        ast::FieldList::TupleFieldList(tuple_field_list) => tuple_field_list
            .l_paren_token()
            .map(|it| (it.text_range().end(), None, None))
            .or(tuple_field_list.fields().next().map(|it| {
                (
                    it.syntax().text_range().start(),
                    None,
                    Some(", ".to_string()),
                )
            }))
            .unwrap_or((tuple_field_list.syntax().text_range().start(), None, None)),
    }
}

/// Returns the offset for the end of a field list and affixes (prefix and suffix).
pub fn field_insert_offset_end_and_affixes(
    field_list: &ast::FieldList,
) -> (TextSize, Option<String>, Option<String>) {
    // Determines the insert prefix for inserting after a field item.
    let insert_after_prefix = |last_field: &SyntaxNode, is_block: bool| {
        let has_comma = last_field
            .last_token()
            .and_then(|token| {
                ink_analyzer_ir::closest_item_which(
                    &token,
                    SyntaxToken::next_token,
                    |subject| subject.kind() == SyntaxKind::COMMA,
                    |subject| !subject.kind().is_trivia(),
                )
            })
            .is_some();
        (!has_comma || is_block).then_some(format!(
            "{}{}",
            if has_comma { "" } else { "," },
            if is_block { "\n" } else { " " }
        ))
    };

    match field_list {
        ast::FieldList::RecordFieldList(record_field_list) => record_field_list
            // Determines position after the last field in the record field list.
            .fields()
            .last()
            .map(|it| {
                (
                    node_and_delimiter_range(it.syntax(), SyntaxKind::COMMA).end(),
                    insert_after_prefix(it.syntax(), true),
                    None,
                )
            }),
        ast::FieldList::TupleFieldList(tuple_field_list) => tuple_field_list
            // Determines position after the last field in the tuple field list.
            .fields()
            .last()
            .map(|it| {
                (
                    node_and_delimiter_range(it.syntax(), SyntaxKind::COMMA).end(),
                    insert_after_prefix(it.syntax(), false),
                    None,
                )
            }),
    }
    // Defaults to the start if field list is empty because it's easier to apply additional formatting that way.
    .unwrap_or(field_insert_offset_start_and_affixes(field_list))
}

/// Returns an offset, indenting and affixes (prefix and suffix)
/// for inserting an ink! callable (i.e ink! constructor or ink! message) into a contract.
pub fn callable_insert_offset_indent_and_affixes(
    contract: &Contract,
) -> Option<(TextSize, String, Option<String>, Option<String>)> {
    contract
        .module()
        // Gets the first non-trait `impl` block (if any).
        .and_then(ast::Module::item_list)
        .and_then(|it| {
            it.items().find_map(|it| match it {
                ast::Item::Impl(impl_item) => impl_item.trait_().is_none().then_some(impl_item),
                _ => None,
            })
        })
        .as_ref()
        // Gets the first non-trait ink! `impl` block (if any).
        .or(contract
            .impls()
            .iter()
            .find(|it| it.trait_type().is_none())
            .and_then(InkImpl::impl_item))
        .and_then(|impl_item| Some(impl_item).zip(impl_item.assoc_item_list()))
        .map(|(impl_item, assoc_item_list)| {
            // Sets insert offset at the end of the associated items list, insert indent based on `impl` block with no affixes.
            (
                assoc_item_insert_offset_end(&assoc_item_list),
                item_children_indenting(impl_item.syntax()),
                None,
                None,
            )
        })
        // Otherwise inserts a new `impl` block (returned as affixes).
        .or(contract
            .module()
            .and_then(ast::Module::item_list)
            .and_then(|item_list| {
                callable_impl_indent_and_affixes(contract).map(|(indent, prefix, suffix)| {
                    // Sets insert offset at the end of the associated items list, insert indent based on contract `mod` block
                    // and affixes (prefix and suffix) for wrapping callable in an `impl` block.
                    (
                        item_insert_offset_impl(&item_list),
                        format!("{indent}    "),
                        Some(prefix),
                        Some(suffix),
                    )
                })
            }))
}

/// Returns indenting and `impl` affixes (prefix and suffix)
/// for inserting an ink! callable (i.e ink! constructor or ink! message) into a contract.
pub fn callable_impl_indent_and_affixes(contract: &Contract) -> Option<(String, String, String)> {
    contract.module().and_then(|mod_item| {
        // Resolves the contract name (if possible).
        resolve_contract_name(contract).map(|name| {
            // Sets insert indent based on contract `mod` block
            // and affixes (prefix and suffix) for wrapping callable in an `impl` block.
            let indent = item_children_indenting(mod_item.syntax());
            let prefix = format!("{indent}impl {name} {{\n");
            let suffix = format!("\n{indent}}}",);
            (format!("{indent}    "), prefix, suffix)
        })
    })
}

/// Returns the "resolved" contract name.
///
/// NOTE: Either reads it directly from the ink! storage `struct` (if present),
/// or returns the name of the ink! contract `mod` in pascal case (i.e. UpperCamelCase).
pub fn resolve_contract_name(contract: &Contract) -> Option<String> {
    contract
        .storage()
        .and_then(Storage::struct_item)
        .and_then(HasName::name)
        .as_ref()
        .map(ToString::to_string)
        .or(contract
            .module()
            .and_then(HasName::name)
            .as_ref()
            .map(ToString::to_string)
            .as_deref()
            .map(utils::pascal_case))
}

/// Applies indenting to a snippet.
pub fn apply_indenting(input: &str, indent: &str) -> String {
    if indent.is_empty() {
        input.to_string()
    } else {
        let mut output = String::new();
        for (idx, line) in input.lines().enumerate() {
            if idx > 0 {
                output.push('\n');
            }
            if !line.is_empty() {
                output.push_str(indent);
                output.push_str(line);
            }
        }
        output
    }
}

/// Reduces indenting for a snippet.
pub fn reduce_indenting(input: &str, indent: &str) -> String {
    if indent.is_empty() {
        input.to_string()
    } else {
        let mut output = String::new();
        for (idx, line) in input.lines().enumerate() {
            if idx > 0 {
                output.push('\n');
            }
            if !line.is_empty() {
                // Removes indent if it's a prefix of the current line.
                output.push_str(line.strip_prefix(indent).unwrap_or(line));
            }
        }
        output
    }
}

/// Suggests a unique/unused id for an extension function.
pub fn suggest_unique_id(preferred_id: Option<u32>, unavailable_ids: &mut HashSet<u32>) -> u32 {
    // Finds a unique/unused id.
    let mut suggested_id = preferred_id.unwrap_or(1);
    while unavailable_ids.contains(&suggested_id) {
        suggested_id += 1;
    }

    // Makes the id unavailable for future calls to this function.
    unavailable_ids.insert(suggested_id);

    // Returns the unique id.
    suggested_id
}

/// Returns text range of the contract `mod` "declaration"
/// (i.e tokens between meta - attributes/rustdoc - and the start of the item list).
pub fn contract_declaration_range(contract: &Contract) -> TextRange {
    contract
        .module()
        .and_then(|it| ast_item_declaration_range(&ast::Item::Module(it.clone())))
        .unwrap_or(contract.syntax().text_range())
}

/// Returns text range of the ink! `impl` "declaration"
/// (i.e tokens between meta - attributes/rustdoc - and the start of the item list).
pub fn ink_impl_declaration_range(ink_impl: &InkImpl) -> TextRange {
    ink_impl
        .impl_item()
        .and_then(|it| ast_item_declaration_range(&ast::Item::Impl(it.clone())))
        .unwrap_or(ink_impl.syntax().text_range())
}

/// Returns text range of the `trait` "declaration"
/// (i.e tokens between meta - attributes/rustdoc - and the start of the item list) for a trait-based ink! entity.
pub fn ink_trait_declaration_range<T>(ink_trait_item: &T) -> TextRange
where
    T: InkEntity + IsInkTrait,
{
    ink_trait_item
        .trait_item()
        .and_then(|it| ast_item_declaration_range(&ast::Item::Trait(it.clone())))
        .unwrap_or(ink_trait_item.syntax().text_range())
}

/// Compares two node for equality while ignoring trivia (i.e. whitespace and comments).
pub fn is_trivia_insensitive_eq(a: &SyntaxNode, b: &SyntaxNode) -> bool {
    let strip_trivia = |node: &SyntaxNode| {
        node.children_with_tokens()
            .filter(|node| !node.kind().is_trivia())
            .join("")
    };
    strip_trivia(a) == strip_trivia(b)
}
