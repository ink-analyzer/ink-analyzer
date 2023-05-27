//! Utilities for ink! analysis.

use ink_analyzer_ir::syntax::{SyntaxKind, SyntaxNode};
use ink_analyzer_ir::{InkArgKind, InkAttributeKind, InkMacroKind};

/// Returns valid sibling ink! argument kinds for the given ink! attribute kind.
///
/// (i.e argument kinds that don't conflict with the given ink! attribute kind,
/// e.g for the `contract` attribute macro kind, this would be `env` and `keep_attr`
/// while for the `storage` attribute argument kind, this would be `default`, `payable` and `selector`).
pub fn valid_sibling_ink_args(attr_kind: &InkAttributeKind) -> Vec<InkArgKind> {
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
                _ => Vec::new(),
            }
        }
        // Returns valid sibling args (if any) for ink! attribute arguments.
        // IR crate already makes sure `arg_kind` is the best match regardless of source code order,
        // See `ink_analyzer_ir::attrs::utils::sort_ink_args_by_kind` doc.
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
pub fn valid_quasi_direct_descendant_ink_args(attr_kind: &InkAttributeKind) -> Vec<InkArgKind> {
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
                // ink! storage items and ink! tests can't have ink! descendants.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
                _ => Vec::new(),
            }
        }
        // Returns valid quasi-direct descendant args (if any) for ink! attribute arguments.
        // IR crate already makes sure `arg_kind` is the best match regardless of source code order,
        // See `ink_analyzer_ir::attrs::utils::sort_ink_args_by_kind` doc.
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
pub fn valid_quasi_direct_descendant_ink_macros(attr_kind: &InkAttributeKind) -> Vec<InkMacroKind> {
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
                ],
                // All other ink! attribute macros can't have ink! macro descendants.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
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
pub fn valid_ink_ink_args_by_syntax_kind(syntax_kind: SyntaxKind) -> Vec<InkArgKind> {
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
        SyntaxKind::FN | SyntaxKind::FN_KW => vec![InkMacroKind::Test],
        _ => Vec::new(),
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
        .map(|ink_arg| ink_arg.kind().to_owned())
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
            InkAttributeKind::Macro(macro_kind) => Some(macro_kind.to_owned()),
            InkAttributeKind::Arg(_) => None,
        })
        .collect();
    // Filters out duplicates.
    suggestions.retain(|arg_kind| !already_annotated_ink_macros.contains(arg_kind));
}

/// Filters out invalid ink! arguments from suggestions based on parent ink! scope.
pub fn remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
    suggestions: &mut Vec<InkArgKind>,
    attr_parent: &SyntaxNode,
) {
    let parent_ink_scope_valid_ink_args: Vec<InkArgKind> =
        ink_analyzer_ir::ink_attrs_closest_ancestors(attr_parent)
            .flat_map(|attr| valid_quasi_direct_descendant_ink_args(attr.kind()))
            .collect();

    // Filters out invalid arguments for the parent ink! scope (if any).
    if !parent_ink_scope_valid_ink_args.is_empty() {
        suggestions.retain(|arg_kind| {
            parent_ink_scope_valid_ink_args.is_empty()
                || parent_ink_scope_valid_ink_args.contains(arg_kind)
        });
    }
}

/// Filters out invalid ink! macros from suggestions based on parent ink! scope.
pub fn remove_invalid_ink_macro_suggestions_for_parent_ink_scope(
    suggestions: &mut Vec<InkMacroKind>,
    attr_parent: &SyntaxNode,
) {
    let parent_ink_scope_valid_ink_macros: Vec<InkMacroKind> =
        ink_analyzer_ir::ink_attrs_closest_ancestors(attr_parent)
            .flat_map(|attr| valid_quasi_direct_descendant_ink_macros(attr.kind()))
            .collect();

    // Filters out invalid arguments for the parent ink! scope (if any).
    if !parent_ink_scope_valid_ink_macros.is_empty() {
        suggestions.retain(|macro_kind| {
            parent_ink_scope_valid_ink_macros.is_empty()
                || parent_ink_scope_valid_ink_macros.contains(macro_kind)
        });
    }
}
