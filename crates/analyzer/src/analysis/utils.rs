//! Utilities for ink! analysis.

use std::collections::HashSet;

use ink_analyzer_ir::ast::{HasAttrs, HasDocComments, HasModuleItem, HasName};
use ink_analyzer_ir::syntax::{
    AstNode, AstToken, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, TextSize,
};
use ink_analyzer_ir::{
    ast, ChainExtension, Contract, Extension, Function, HasInkImplParent, InkArg, InkArgKind,
    InkArgValueKind, InkAttribute, InkAttributeKind, InkEntity, InkImpl, InkMacroKind,
    IsInkCallable, IsInkStruct, IsInkTrait, IsIntId, Message, MinorVersion, Selector, Storage,
    TraitDefinition,
};
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::{resolution, utils, Version};

/// Returns valid sibling ink! argument kinds for the given ink! attribute kind.
///
/// (i.e. argument kinds that don't conflict with the given ink! attribute kind,
/// e.g. for the `contract` attribute macro kind, this would be `env` and `keep_attr`
/// while for the `storage` attribute argument kind, this would be `default`, `payable` and `selector`).
pub fn valid_sibling_ink_args(attr_kind: InkAttributeKind, version: Version) -> Vec<InkArgKind> {
    match attr_kind {
        // Returns valid sibling args (if any) for ink! attribute macros.
        InkAttributeKind::Macro(macro_kind) => {
            match macro_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/ir/src/ir/chain_extension.rs#L601-L613>
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L897-L1337>
                // Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html>
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L188-L197>
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>
                InkMacroKind::ChainExtension if version.is_v5() => {
                    vec![InkArgKind::Extension]
                }
                InkMacroKind::ChainExtension => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L111-L199>.
                InkMacroKind::Contract => vec![InkArgKind::Env, InkArgKind::KeepAttr],
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/ir/src/ir/event/mod.rs#L129-L141>
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L656-L692>
                // Ref: <https://paritytech.github.io/ink/ink/attr.event.html>
                InkMacroKind::Event if version.is_gte_v5() => {
                    vec![InkArgKind::Anonymous, InkArgKind::SignatureTopic]
                }
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/config.rs#L36-L59>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
                InkMacroKind::StorageItem => vec![InkArgKind::Derive],
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27-L30>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
                InkMacroKind::Test => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/config.rs#L60-L85>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
                InkMacroKind::TraitDefinition => vec![InkArgKind::KeepAttr, InkArgKind::Namespace],
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/e2e/macro/src/config.rs#L83-L97>
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/e2e/macro/src/lib.rs#L41-L45>
                // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L49-L85>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L41-L45>.
                InkMacroKind::E2ETest if version.is_legacy() => vec![
                    InkArgKind::AdditionalContracts,
                    InkArgKind::Environment,
                    InkArgKind::KeepAttr,
                ],
                InkMacroKind::E2ETest => {
                    vec![InkArgKind::Backend, InkArgKind::Environment]
                }
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L1598-L1625>
                // Ref: <https://paritytech.github.io/ink/ink/attr.scale_derive.html>
                InkMacroKind::ScaleDerive if version.is_gte_v5() => {
                    vec![InkArgKind::Encode, InkArgKind::Decode, InkArgKind::TypeInfo]
                }
                _ => Vec::new(),
            }
        }
        // Returns valid sibling args (if any) for ink! attribute arguments.
        // IR crate already makes sure `arg_kind` is the best match regardless of order in source code.
        InkAttributeKind::Arg(arg_kind) => {
            match arg_kind {
                // Unambiguous `arg_kind`.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L83-L93>.
                InkArgKind::Storage => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/ir/src/ir/event/mod.rs#L129-L141>
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L88-L98>.
                InkArgKind::Event if version.is_legacy() => vec![InkArgKind::Anonymous],
                InkArgKind::Event => {
                    vec![InkArgKind::Anonymous, InkArgKind::SignatureTopic]
                }
                InkArgKind::SignatureTopic if version.is_gte_v5() => {
                    vec![InkArgKind::Event]
                }
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
                // In ink! v5, `extension` is a sole/required attribute argument for the `chain_extension` macro attribute,
                // see `chain_extension` macro pattern for details.
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/ir/src/ir/chain_extension.rs#L601-L613>
                InkArgKind::Extension if version.is_legacy() => vec![InkArgKind::HandleStatus],
                InkArgKind::Extension => Vec::new(),
                InkArgKind::Function if version.is_v5() => {
                    vec![InkArgKind::HandleStatus]
                }
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
                InkArgKind::HandleStatus if version.is_legacy() => vec![InkArgKind::Extension],
                // See `function` pattern above for references.
                InkArgKind::HandleStatus if version.is_v5() => vec![InkArgKind::Function],
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
/// (i.e. argument kinds that are allowed in the scope of the given ink! attribute kind,
/// e.g. for the `chain_extension` attribute macro kind, this would be `extension` and `handle_status`
/// while for the `event` attribute argument kind, this would be `topic`).
pub fn valid_quasi_direct_descendant_ink_args(
    attr_kind: InkAttributeKind,
    version: Version,
) -> Vec<InkArgKind> {
    match attr_kind {
        // Returns valid quasi-direct descendant args (if any) for ink! attribute macros.
        InkAttributeKind::Macro(macro_kind) => {
            match macro_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/ir/src/ir/chain_extension.rs#L601-L613>
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L897-L1337>
                // Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html>
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L476-L487>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>.
                InkMacroKind::ChainExtension if version.is_legacy() => {
                    vec![InkArgKind::Extension, InkArgKind::HandleStatus]
                }
                InkMacroKind::ChainExtension if version.is_v5() => {
                    vec![InkArgKind::Function, InkArgKind::HandleStatus]
                }
                InkMacroKind::ChainExtension => Vec::new(),
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L58-L116>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L111-L199>.
                InkMacroKind::Contract if version.is_legacy() => vec![
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
                    InkArgKind::SignatureTopic,
                    InkArgKind::Storage,
                ],
                // Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L656-L692>
                // Ref: <https://paritytech.github.io/ink/ink/attr.event.html>
                InkMacroKind::Event if version.is_gte_v5() => {
                    vec![InkArgKind::Topic]
                }
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
        // IR crate already makes sure `arg_kind` is the best match regardless of order in source code.
        InkAttributeKind::Arg(arg_kind) => {
            match arg_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L132-L139>.
                InkArgKind::Event | InkArgKind::Anonymous => vec![InkArgKind::Topic],
                InkArgKind::SignatureTopic if version.is_gte_v5() => {
                    vec![InkArgKind::Topic]
                }
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
/// (i.e. macro kinds that are allowed in the scope of the given ink! attribute kind,
/// e.g. for the `contract` attribute macro kind, this would be `chain_extension`, `storage_item`,
/// `test` and `trait_definition`).
pub fn valid_quasi_direct_descendant_ink_macros(
    attr_kind: InkAttributeKind,
    version: Version,
) -> Vec<InkMacroKind> {
    match attr_kind {
        // Returns valid quasi-direct descendant macros (if any) for ink! attribute macros.
        InkAttributeKind::Macro(macro_kind) => {
            match macro_kind {
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L111-L199>.
                InkMacroKind::Contract if version.is_legacy() => {
                    vec![
                        InkMacroKind::ChainExtension,
                        InkMacroKind::StorageItem,
                        InkMacroKind::Test,
                        InkMacroKind::TraitDefinition,
                        InkMacroKind::E2ETest,
                    ]
                }
                InkMacroKind::Contract if version.is_v5() => {
                    vec![
                        InkMacroKind::ChainExtension,
                        InkMacroKind::Event,
                        InkMacroKind::ScaleDerive,
                        InkMacroKind::StorageItem,
                        InkMacroKind::Test,
                        InkMacroKind::TraitDefinition,
                        InkMacroKind::E2ETest,
                    ]
                }
                InkMacroKind::Contract => {
                    vec![
                        InkMacroKind::ContractRef,
                        InkMacroKind::Error,
                        InkMacroKind::Event,
                        InkMacroKind::ScaleDerive,
                        InkMacroKind::StorageItem,
                        InkMacroKind::Test,
                        InkMacroKind::TraitDefinition,
                        InkMacroKind::E2ETest,
                    ]
                }
                // All ink! >= 5.x macros that are either applied to `fn` items or
                // can have associated `fn` items can have `scale_derive` as a descendant
                // (essentially everything except `event`, `scale_derive` and `storage_item`).
                // But chain extensions are deprecated in ink! >= 6.x, so we special case that.
                InkMacroKind::ChainExtension if version.is_gte_v6() => Vec::new(),
                InkMacroKind::ChainExtension
                | InkMacroKind::TraitDefinition
                | InkMacroKind::Test
                | InkMacroKind::E2ETest
                    if version.is_gte_v5() =>
                {
                    vec![InkMacroKind::ScaleDerive]
                }
                // All other v4 ink! attribute macros can't have ink! macro descendants.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L848-L1280>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L772-L799>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L805-L846>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L597-L643>.
                // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/ir.rs#L37-L48>.
                _ => Vec::new(),
            }
        }
        // Returns valid quasi-direct descendant macros (if any) for ink! attribute arguments.
        InkAttributeKind::Arg(arg_kind) => {
            match arg_kind {
                // All v5 attribute arguments used on `fn` items can have scale_derive as a descendant
                // (essentially everything except `storage`, `event`, `anonymous` and `signature_topic`).
                InkArgKind::Storage
                | InkArgKind::Event
                | InkArgKind::Anonymous
                | InkArgKind::SignatureTopic
                    if version.is_gte_v5() =>
                {
                    Vec::new()
                }
                _ if version.is_gte_v5() => {
                    vec![InkMacroKind::ScaleDerive]
                }
                // v4 ink! attribute arguments can't have ink! macro descendants.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs>.
                _ => Vec::new(),
            }
        }
    }
}

/// Returns valid ink! argument kinds for the given syntax kind.
///
/// (i.e. argument kinds that can be applied to the given syntax kind,
/// e.g. for the `impl` syntax kind, this would be `impl` and `namespace`).
pub fn valid_ink_args_by_syntax_kind(syntax_kind: SyntaxKind, version: Version) -> Vec<InkArgKind> {
    match syntax_kind {
        // `env` and `keep_attr` can only be applied to a `mod` as siblings of an `ink::contract` macro.
        SyntaxKind::MODULE | SyntaxKind::MOD_KW => Vec::new(),
        // `keep_attr` and `namespace` can only be applied to a `trait` as siblings of an `ink::trait_definition` macro.
        SyntaxKind::TRAIT | SyntaxKind::TRAIT_KW => Vec::new(),
        // `derive` can only be applied to an ADT (`enum`, `struct` or `union`) as a sibling of an `ink::storage_item` macro.
        SyntaxKind::STRUCT | SyntaxKind::STRUCT_KW if version.is_legacy() => vec![
            InkArgKind::Anonymous,
            InkArgKind::Event,
            InkArgKind::Storage,
        ],
        SyntaxKind::STRUCT | SyntaxKind::STRUCT_KW => vec![
            InkArgKind::Anonymous,
            InkArgKind::Event,
            InkArgKind::SignatureTopic,
            InkArgKind::Storage,
        ],
        SyntaxKind::ENUM | SyntaxKind::ENUM_KW | SyntaxKind::UNION | SyntaxKind::UNION_KW => {
            Vec::new()
        }
        SyntaxKind::RECORD_FIELD => vec![InkArgKind::Topic],
        SyntaxKind::FN | SyntaxKind::FN_KW if version.is_legacy() => vec![
            InkArgKind::Constructor,
            InkArgKind::Default,
            InkArgKind::Extension,
            InkArgKind::HandleStatus,
            InkArgKind::Message,
            InkArgKind::Payable,
            InkArgKind::Selector,
        ],
        SyntaxKind::FN | SyntaxKind::FN_KW if version.is_v5() => vec![
            InkArgKind::Constructor,
            InkArgKind::Default,
            InkArgKind::Function,
            InkArgKind::HandleStatus,
            InkArgKind::Message,
            InkArgKind::Payable,
            InkArgKind::Selector,
        ],
        SyntaxKind::FN | SyntaxKind::FN_KW => vec![
            InkArgKind::Constructor,
            InkArgKind::Default,
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
/// (i.e. macro kinds that can be applied to the given syntax kind,
/// e.g. for the `module` syntax kind, this would be `contract`).
pub fn valid_ink_macros_by_syntax_kind(
    syntax_kind: SyntaxKind,
    version: Version,
) -> Vec<InkMacroKind> {
    match syntax_kind {
        SyntaxKind::MODULE | SyntaxKind::MOD_KW => vec![InkMacroKind::Contract],
        SyntaxKind::TRAIT | SyntaxKind::TRAIT_KW if version.is_lte_v5() => {
            vec![InkMacroKind::ChainExtension, InkMacroKind::TraitDefinition]
        }
        SyntaxKind::TRAIT | SyntaxKind::TRAIT_KW => {
            vec![InkMacroKind::ContractRef, InkMacroKind::TraitDefinition]
        }
        SyntaxKind::ENUM
        | SyntaxKind::ENUM_KW
        | SyntaxKind::STRUCT
        | SyntaxKind::STRUCT_KW
        | SyntaxKind::UNION
        | SyntaxKind::UNION_KW
            if version.is_legacy() =>
        {
            vec![InkMacroKind::StorageItem]
        }
        SyntaxKind::STRUCT | SyntaxKind::STRUCT_KW => vec![
            InkMacroKind::Event,
            InkMacroKind::ScaleDerive,
            InkMacroKind::StorageItem,
        ],
        SyntaxKind::ENUM | SyntaxKind::ENUM_KW | SyntaxKind::UNION | SyntaxKind::UNION_KW => {
            vec![InkMacroKind::ScaleDerive, InkMacroKind::StorageItem]
        }
        SyntaxKind::FN | SyntaxKind::FN_KW => vec![InkMacroKind::Test, InkMacroKind::E2ETest],
        _ => Vec::new(),
    }
}

/// Returns the primary ink! attribute candidate for the syntax node (if any),
/// a boolean flag indicating whether it's the first ink! attribute.
///
/// (i.e. returns either the first valid ink! attribute macro or the highest ranked ink! attribute argument,
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
                // and a flag indicating whether it's the first ink! attribute.
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
    version: Version,
) -> Vec<InkAttributeKind> {
    match attr_kind {
        InkAttributeKind::Arg(arg_kind) => {
            // Only ink! attribute arguments when set as the primary attribute have
            // the potential to be either incomplete or ambiguous.
            // See respective match pattern in the [`utils::valid_sibling_ink_args`] function for the rationale and references.
            match arg_kind {
                InkArgKind::AdditionalContracts if version.is_legacy() => {
                    vec![InkAttributeKind::Macro(InkMacroKind::E2ETest)]
                }
                InkArgKind::AdditionalContracts => Vec::new(),
                InkArgKind::Anonymous if version.is_legacy() => {
                    vec![InkAttributeKind::Arg(InkArgKind::Event)]
                }
                InkArgKind::Anonymous => {
                    vec![
                        InkAttributeKind::Macro(InkMacroKind::Event),
                        // TODO: Maybe only suggest `#[ink::event] for >= 5.x?
                        InkAttributeKind::Arg(InkArgKind::Event),
                    ]
                }
                InkArgKind::Backend if version.is_gte_v5() => {
                    vec![InkAttributeKind::Macro(InkMacroKind::E2ETest)]
                }
                InkArgKind::Decode | InkArgKind::Encode | InkArgKind::TypeInfo
                    if version.is_gte_v5() =>
                {
                    vec![InkAttributeKind::Macro(InkMacroKind::ScaleDerive)]
                }
                InkArgKind::Environment => {
                    vec![InkAttributeKind::Macro(InkMacroKind::E2ETest)]
                }
                InkArgKind::KeepAttr => vec![
                    InkAttributeKind::Macro(InkMacroKind::Contract),
                    InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                    InkAttributeKind::Macro(InkMacroKind::E2ETest),
                ],
                InkArgKind::HandleStatus if version.is_legacy() => {
                    vec![InkAttributeKind::Arg(InkArgKind::Extension)]
                }
                InkArgKind::HandleStatus if version.is_v5() => {
                    vec![InkAttributeKind::Arg(InkArgKind::Function)]
                }
                InkArgKind::HandleStatus => Vec::new(),
                InkArgKind::Namespace => vec![
                    InkAttributeKind::Macro(InkMacroKind::TraitDefinition),
                    InkAttributeKind::Arg(InkArgKind::Impl),
                ],
                InkArgKind::Payable | InkArgKind::Default | InkArgKind::Selector => vec![
                    InkAttributeKind::Arg(InkArgKind::Constructor),
                    InkAttributeKind::Arg(InkArgKind::Message),
                ],
                InkArgKind::SignatureTopic if version.is_gte_v5() => {
                    vec![
                        InkAttributeKind::Macro(InkMacroKind::Event),
                        // TODO: Maybe only suggest `#[ink::event] for >= 5.x?
                        InkAttributeKind::Arg(InkArgKind::Event),
                    ]
                }
                // Default
                _ => Vec::new(),
            }
        }
        // ink! attribute macros are always complete and unambiguous on their own.
        InkAttributeKind::Macro(_) => Vec::new(),
    }
}

/// Filters out duplicate ink! arguments from suggestions
/// (i.e. ink! arguments that are already applied to the attribute's parent node).
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
/// (i.e. ink! macros that are already applied to the attribute's parent node).
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
/// (i.e. ink! arguments that aren't valid siblings of the best candidate for primary ink! attribute kind of the parent node).
pub fn remove_conflicting_ink_arg_suggestions(
    suggestions: &mut Vec<InkArgKind>,
    attr_parent: &SyntaxNode,
    version: Version,
) {
    // Gets the primary ink! attribute candidate (if any).
    if let Some((primary_ink_attr, ..)) =
        primary_ink_attribute_candidate(ink_analyzer_ir::ink_attrs(attr_parent))
    {
        let attr_kind = primary_ink_attr.kind();
        let valid_siblings = valid_sibling_ink_args(*attr_kind, version);
        // Filters out invalid siblings.
        suggestions.retain(|arg_kind| valid_siblings.contains(arg_kind));

        // For ink! >= 5.x, `anonymous` and `signature_topic` conflict.
        // We need a special check since neither is a "primary" attribute argument.
        if version.is_gte_v5()
            && matches!(
                attr_kind,
                InkAttributeKind::Macro(InkMacroKind::Event)
                    | InkAttributeKind::Arg(
                        InkArgKind::Event | InkArgKind::Anonymous | InkArgKind::SignatureTopic
                    )
            )
        {
            let is_anonymous = *attr_kind == InkAttributeKind::Arg(InkArgKind::Anonymous)
                || ink_analyzer_ir::ink_arg_by_kind(attr_parent, InkArgKind::Anonymous).is_some();
            let has_signature = *attr_kind == InkAttributeKind::Arg(InkArgKind::SignatureTopic)
                || ink_analyzer_ir::ink_arg_by_kind(attr_parent, InkArgKind::SignatureTopic)
                    .is_some();
            if is_anonymous || has_signature {
                suggestions.retain(|arg_kind| {
                    (!is_anonymous || *arg_kind != InkArgKind::SignatureTopic)
                        && (!has_signature || *arg_kind != InkArgKind::Anonymous)
                });
            }
        }
    }
}

/// Filters out invalid ink! arguments from suggestions based on parent item's invariants.
///
/// (e.g. ink! namespace arguments can't be applied to trait `impl` blocks).
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
    version: Version,
) {
    let mut has_chain_extension_parent = false;
    let parent_ink_scope_valid_ink_args: Vec<InkArgKind> =
        ink_analyzer_ir::ink_attrs_closest_ancestors(attr_parent)
            .flat_map(|attr| {
                if *attr.kind() == InkAttributeKind::Macro(InkMacroKind::ChainExtension) {
                    has_chain_extension_parent = true;
                }
                valid_quasi_direct_descendant_ink_args(*attr.kind(), version)
            })
            .collect();

    // For ink! >= 6.x, chain extensions are deprecated, so we invalidate any "child" suggestions.
    if version.is_gte_v6()
        && has_chain_extension_parent
        && parent_ink_scope_valid_ink_args.is_empty()
    {
        *suggestions = Vec::new();
        return;
    }

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
    version: Version,
) {
    if let Some(attr_parent) = ink_attr.syntax().parent() {
        // Filters out duplicate ink! attribute argument suggestions.
        remove_duplicate_ink_arg_suggestions(suggestions, &attr_parent);

        // Filters out conflicting ink! attribute argument actions.
        remove_conflicting_ink_arg_suggestions(suggestions, &attr_parent, version);

        // Filters out invalid (based on parent ink! scope) ink! attribute argument actions,
        // Doesn't apply to ink! attribute macros as their arguments are not influenced by the parent scope.
        if let InkAttributeKind::Arg(_) = ink_attr.kind() {
            remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
                suggestions,
                &attr_parent,
                version,
            );
        }
    }
}

/// Filters out invalid ink! macros from suggestions based on parent ink! scope.
pub fn remove_invalid_ink_macro_suggestions_for_parent_ink_scope(
    suggestions: &mut Vec<InkMacroKind>,
    attr_parent: &SyntaxNode,
    version: Version,
) {
    let mut ink_ancestors = ink_analyzer_ir::ink_attrs_closest_ancestors(attr_parent);
    // Filters out invalid ink! macros for the parent ink! scope (if any).
    if let Some(first_ancestor) = ink_ancestors.next() {
        let parent_ink_scope_valid_ink_macros: Vec<InkMacroKind> = [first_ancestor]
            .into_iter()
            .chain(ink_ancestors)
            .flat_map(|attr| valid_quasi_direct_descendant_ink_macros(*attr.kind(), version))
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
    attr.path()
        .is_some_and(|path| path.to_string().trim() == "cfg")
        && attr.token_tree().is_some_and(|token_tree| {
            static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[(,]\s*test\s*[,)]").unwrap());
            RE.is_match(&token_tree.syntax().to_string())
        })
}

/// Returns true if the attribute is a conditional compilation flag for test builds
/// with an additional `e2e-tests` feature condition.
pub fn is_cfg_e2e_tests_attr(attr: &ast::Attr) -> bool {
    is_cfg_test_attr(attr)
        && attr.token_tree().is_some_and(|token_tree| {
            static RE: Lazy<Regex> =
                Lazy::new(|| Regex::new(r#"[(,]\s*feature\s*=\s*"e2e-tests"\s*[,)]"#).unwrap());
            RE.is_match(&token_tree.syntax().to_string())
        })
}

/// Returns the insert text and snippet (if appropriate) for ink! attribute argument including
/// the `=` symbol after the ink! attribute argument name if necessary.
///
/// (i.e. for `selector`, we return `"selector="` while for `payable`, we simply return `"payable"`)
pub fn ink_arg_insert_text(
    arg_kind: InkArgKind,
    version: Version,
    insert_offset_option: Option<TextSize>,
    parent_attr_option: Option<&InkAttribute>,
) -> (String, Option<String>) {
    // Determines whether to insert an `=` symbol after the ink! attribute argument name.
    let value_kind = InkArgValueKind::from(arg_kind);
    let next_non_trivia_token = || {
        parent_attr_option
            .map(InkAttribute::syntax)
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
                        if is_next_non_trivia_token(&token) {
                            Some(token)
                        } else {
                            ink_analyzer_ir::closest_item_which(
                                &token,
                                SyntaxToken::next_token,
                                is_next_non_trivia_token,
                                is_next_non_trivia_token,
                            )
                        }
                    })
            })
    };
    let insert_equal_token = match value_kind {
        // No `=` symbol is inserted after ink! attribute arguments that should not have a value.
        InkArgValueKind::None | InkArgValueKind::Arg(..) | InkArgValueKind::Choice(..) => false,
        // Adds an `=` symbol after the ink! attribute argument name if an `=` symbol is not
        // the next closest non-trivia token after the insert offset.
        _ => next_non_trivia_token()
            .map(|next_token| match next_token.kind() {
                SyntaxKind::EQ => false,
                // Adds an `=` symbol only if the next closest non-trivia token is not an `=` symbol.
                _ => true,
            })
            // Defaults to inserting the `=` symbol (e.g. if either parent attribute is `None` or
            // the next closest non-trivia token can't be determined).
            .unwrap_or(true),
    };
    // Determines whether to insert a nested value after the ink! attribute argument name.
    let insert_nested_value = !insert_equal_token
        && match value_kind {
            // Don't add default values for optional nested arguments.
            InkArgValueKind::Arg(_, false) | InkArgValueKind::Choice(_, _, false) => false,
            // Adds a nested value after the ink! attribute argument name if a `(` symbol is not
            // the next closest non-trivia token after the insert offset.
            InkArgValueKind::Arg(_, true) | InkArgValueKind::Choice(_, _, true) => {
                next_non_trivia_token()
                    .map(|next_token| match next_token.kind() {
                        SyntaxKind::L_PAREN => false,
                        // Adds a nested value only if the next closest non-trivia token is not a `(` symbol.
                        _ => true,
                    })
                    // Defaults to inserting the nested value (e.g. if either parent attribute is `None`
                    // or the next closest non-trivia token can't be determined).
                    .unwrap_or(true)
            }
            // ink! attribute kinds that don't take nested values are ignored.
            _ => false,
        };
    let (text_value, snippet_value) = if insert_equal_token || insert_nested_value {
        match arg_kind {
            InkArgKind::AdditionalContracts | InkArgKind::KeepAttr | InkArgKind::SignatureTopic => {
                (r#""""#.to_owned(), r#""$1""#.to_owned())
            }
            InkArgKind::Backend => ("node".to_owned(), "${1:node}".to_owned()),
            InkArgKind::Derive | InkArgKind::HandleStatus => {
                ("true".to_owned(), "${1:true}".to_owned())
            }
            InkArgKind::Env | InkArgKind::Environment => {
                let path = parent_attr_option
                    .and_then(|ink_attr| {
                        resolution::candidate_adt_by_name_or_external_trait_impl(
                            "Environment",
                            &["ink::env", "ink_env"],
                            ink_attr.syntax(),
                            None,
                        )
                    })
                    .as_ref()
                    .and_then(resolution::item_path)
                    .unwrap_or("ink::env::DefaultEnvironment".to_owned());
                let snippet = format!("${{1:{path}}}");
                (path, snippet)
            }
            InkArgKind::Extension => {
                let mut unavailable_ids = HashSet::new();
                if let Some(ink_attr) = parent_attr_option {
                    let parent_fn = ink_attr
                        .syntax()
                        .parent()
                        .filter(|parent| ast::Fn::can_cast(parent.kind()));
                    if let Some(chain_extension) = parent_fn
                        .as_ref()
                        .and_then(ink_analyzer_ir::ink_parent::<ChainExtension>)
                    {
                        unavailable_ids = chain_extension
                            .extensions()
                            .iter()
                            .filter_map(Extension::id)
                            .collect();
                    }
                }

                let id = suggest_unique_id(None, &unavailable_ids).unwrap_or(1);
                (format!("{id}"), format!("${{1:{id}}}"))
            }
            InkArgKind::Function => {
                let mut unavailable_ids = HashSet::new();
                if let Some(ink_attr) = parent_attr_option {
                    let parent_fn = ink_attr
                        .syntax()
                        .parent()
                        .filter(|parent| ast::Fn::can_cast(parent.kind()));
                    if let Some(chain_extension) = parent_fn
                        .as_ref()
                        .and_then(ink_analyzer_ir::ink_parent::<ChainExtension>)
                    {
                        unavailable_ids = chain_extension
                            .functions()
                            .iter()
                            .filter_map(Function::id)
                            .collect();
                    }
                }

                let id = suggest_unique_id(None, &unavailable_ids).unwrap_or(1);
                (format!("{id}"), format!("${{1:{id}}}"))
            }
            InkArgKind::Namespace => (
                r#""my_namespace""#.to_owned(),
                r#""${1:my_namespace}""#.to_owned(),
            ),
            InkArgKind::Sandbox => match version {
                Version::Legacy => (String::new(), String::new()),
                Version::V5(MinorVersion::Base) => (
                    "ink_e2e::MinimalSandbox".to_owned(),
                    "${{1:ink_e2e::MinimalSandbox}}".to_owned(),
                ),
                // For versions >= 5.1.x
                _ => (
                    "ink_e2e::DefaultSandbox".to_owned(),
                    "${{1:ink_e2e::DefaultSandbox}}".to_owned(),
                ),
            },
            InkArgKind::Selector => {
                let mut unavailable_ids = HashSet::new();

                if let Some(ink_attr) = parent_attr_option {
                    let parent_fn = ink_attr
                        .syntax()
                        .parent()
                        .filter(|parent| ast::Fn::can_cast(parent.kind()));
                    let parent_trait_def = || {
                        parent_fn
                            .as_ref()
                            .and_then(ink_analyzer_ir::ink_parent::<TraitDefinition>)
                    };
                    let parent_contract = || {
                        parent_fn.as_ref().and_then(|parent_fn| {
                            ink_analyzer_ir::ink_ancestors::<Contract>(parent_fn).next()
                        })
                    };
                    if let Some(trait_def) = parent_trait_def() {
                        unavailable_ids = trait_def
                            .messages()
                            .iter()
                            .filter_map(|msg| msg.composed_selector().map(Selector::into_be_u32))
                            .collect();
                    } else if let Some(contract) = parent_contract() {
                        let is_ctor = ink_attr
                            .args()
                            .iter()
                            .any(|arg| *arg.kind() == InkArgKind::Constructor);
                        unavailable_ids = if is_ctor {
                            contract
                                .constructors()
                                .iter()
                                .filter_map(|ctor| {
                                    ctor.composed_selector().map(Selector::into_be_u32)
                                })
                                .collect()
                        } else {
                            contract
                                .messages()
                                .iter()
                                .filter_map(|msg| {
                                    msg.composed_selector().map(Selector::into_be_u32)
                                })
                                .collect()
                        };
                    }
                }

                let id = suggest_unique_id(None, &unavailable_ids).unwrap_or(1);
                (format!("{id}"), format!("${{1:{id}}}"))
            }
            InkArgKind::Url => (
                r#""ws://127.0.0.1:9000""#.to_owned(),
                r#""${1:ws://127.0.0.1:9000}""#.to_owned(),
            ),
            _ => (String::new(), String::new()),
        }
    } else {
        (String::new(), String::new())
    };
    // Creates insert text with default values (if appropriate).
    let eq_sign = if insert_equal_token { " = " } else { "" };
    let (l_paren, r_paren) = if insert_nested_value {
        ("(", ")")
    } else {
        ("", "")
    };
    let text = format!("{arg_kind}{eq_sign}{l_paren}{text_value}{r_paren}");
    // Creates a snippet with tab stops and/or placeholders (where applicable).
    let snippet = (insert_equal_token || insert_nested_value)
        .then(|| format!("{arg_kind}{eq_sign}{l_paren}{snippet_value}{r_paren}"));
    (text, snippet)
}

/// Returns the insert offset for an ink! attribute.
pub fn ink_attribute_insert_offset(node: &SyntaxNode) -> TextSize {
    ink_analyzer_ir::ink_attrs(node)
        // Finds last token of last ink! attribute.
        .last()
        .as_ref()
        .map(InkAttribute::syntax)
        .and_then(SyntaxNode::last_token)
        // Finds  last token of last attribute.
        .or_else(|| {
            node.children()
                .filter_map(ast::Attr::cast)
                .last()
                .as_ref()
                .map(ast::Attr::syntax)
                .and_then(SyntaxNode::last_token)
        })
        // Otherwise finds the first trivia/rustdoc token for item (if any).
        .or_else(|| {
            node.first_token()
                .filter(|first_token| first_token.kind().is_trivia())
        })
        // Finds the first non-(attribute/rustdoc/trivia) token for the item.
        .and_then(|it| ink_analyzer_ir::closest_non_trivia_token(&it, SyntaxToken::next_token))
        .as_ref()
        // Defaults to the start of the node.
        .map(SyntaxToken::text_range)
        .unwrap_or_else(|| node.text_range())
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
    // Determines if it's a "primary" attribute argument
    // as those get inserted at the beginning of the argument list while everything else gets inserted at the end.
    let is_primary = arg_kind_option
        .as_ref()
        .is_some_and(InkArgKind::is_entity_type);

    // Only computes insert context for closed attributes because
    // unclosed attributes are too tricky for useful contextual edits.
    ink_attr.ast().r_brack_token().map(|r_bracket| {
        ink_attr
            .ast()
            .token_tree()
            .as_ref()
            .map(|token_tree| {
                (
                    // Computes the insert offset.
                    if is_primary {
                        // "Primary" attribute argument get inserted at the beginning of the argument list.
                        token_tree
                            .l_paren_token()
                            // Inserts just after left parenthesis if it exists, otherwise defaults to the end of the attribute.
                            .map(|r_paren| r_paren.text_range().end())
                            .unwrap_or_else(|| token_tree.syntax().text_range().end())
                    } else {
                        // Other attribute arguments get inserted at the end of the argument list.
                        token_tree
                            .r_paren_token()
                            // Inserts just before right parenthesis if it exists, otherwise defaults to the end of the attribute.
                            .map(|r_paren| r_paren.text_range().start())
                            .unwrap_or_else(|| token_tree.syntax().text_range().end())
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
                                    .or_else(|| {
                                        token_tree.syntax().last_token().and_then(|last_token| {
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
                                        })
                                    })
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
                                    .or_else(|| {
                                        token_tree.syntax().first_token().and_then(|first_token| {
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
                                        })
                                    })
                            } else {
                                // No suffix for "non-primary" attribute arguments that already have a right parenthesis after them.
                                None
                            }
                        }
                        // Adds a right parenthesis if none already exists.
                        None => Some(")"),
                    },
                )
            })
            .unwrap_or_else(|| (r_bracket.text_range().start(), Some("("), Some(")")))
    })
}

/// Returns the insert offset for the first ink! attribute.
pub fn first_ink_attribute_insert_offset(node: &SyntaxNode) -> TextSize {
    ink_analyzer_ir::ink_attrs(node)
        // Finds the first ink! attribute.
        .next()
        .map(|it| it.syntax().text_range().start())
        .unwrap_or_else(|| ink_attribute_insert_offset(node))
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
        .or_else(|| {
            ink_attr
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
                })
        })
        .or_else(|| {
            ink_attr
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
                })
        })
        .or_else(|| {
            ink_attr.ast().r_brack_token().map(|r_bracket| {
                // Otherwise, insert before right bracket (if present).
                (r_bracket.text_range().start(), Some("("), Some(")"))
            })
        })
}

/// Returns the indenting (preceding whitespace) of the syntax node.
pub fn item_indenting(node: &SyntaxNode) -> Option<String> {
    node.prev_sibling_or_token().and_then(|prev_elem| {
        (prev_elem.kind() == SyntaxKind::WHITESPACE)
            .then(|| end_indenting(prev_elem.to_string().as_str()))
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
        .and_then(|ident| (!ident.is_empty()).then(|| format!("{ident}{ident}")))
        .unwrap_or("    ".to_owned())
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
                    .map(SyntaxToken::text_range)
                    .unwrap_or_else(|| it.syntax().text_range())
            })
            .or_else(|| {
                module
                    .semicolon_token()
                    .as_ref()
                    .map(SyntaxToken::text_range)
            }),
        ast::Item::Trait(trait_item) => trait_item.assoc_item_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map(SyntaxToken::text_range)
                .unwrap_or_else(|| it.syntax().text_range())
        }),
        ast::Item::Impl(impl_item) => impl_item.assoc_item_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map(SyntaxToken::text_range)
                .unwrap_or_else(|| it.syntax().text_range())
        }),
        ast::Item::Fn(fn_item) => fn_item
            .body()
            .map(|it| {
                it.stmt_list()
                    .map(|it| {
                        it.l_curly_token()
                            .as_ref()
                            .map(SyntaxToken::text_range)
                            .unwrap_or_else(|| it.syntax().text_range())
                    })
                    .unwrap_or_else(|| it.syntax().text_range())
            })
            .or_else(|| {
                fn_item
                    .semicolon_token()
                    .as_ref()
                    .map(SyntaxToken::text_range)
            }),
        ast::Item::Enum(enum_item) => enum_item.variant_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map(SyntaxToken::text_range)
                .unwrap_or_else(|| it.syntax().text_range())
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
                            .or_else(|| it.r_paren_token().as_ref().map(SyntaxToken::text_range))
                            // should be end.
                            .or_else(|| Some(it.syntax().text_range()))
                    }
                }
                .unwrap_or(it.syntax().text_range())
            })
            .or_else(|| {
                struct_item
                    .semicolon_token()
                    .as_ref()
                    .map(SyntaxToken::text_range)
            }),
        ast::Item::Union(union_item) => union_item.record_field_list().map(|it| {
            it.l_curly_token()
                .as_ref()
                .map(SyntaxToken::text_range)
                .unwrap_or_else(|| it.syntax().text_range())
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
            .map(SyntaxToken::text_range)
            .unwrap_or_else(|| item.syntax().text_range())
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
            .or_else(|| module.semicolon_token()),
        ast::Item::Trait(trait_item) => trait_item
            .assoc_item_list()
            .and_then(|it| it.r_curly_token()),
        ast::Item::Impl(impl_item) => impl_item
            .assoc_item_list()
            .and_then(|it| it.r_curly_token()),
        ast::Item::Fn(fn_item) => fn_item
            .body()
            .and_then(|it| it.stmt_list().and_then(|it| it.r_curly_token()))
            .or_else(|| fn_item.semicolon_token()),
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
                            .or_else(|| it.r_paren_token())
                    }
                }
            })
            .or_else(|| struct_item.semicolon_token()),
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
            .map(|it| it.text_range().start())
            .unwrap_or_else(|| token.text_range().end()),
    )
}

/// Returns text range of the syntax node and it's immediate (next) trivia (whitespace and comments).
pub fn node_and_trivia_range(node: &SyntaxNode) -> TextRange {
    TextRange::new(
        node.text_range().start(),
        // Either the start of the next non-trivia token or the end of the target node.
        node.last_token()
            .as_ref()
            .map(token_and_trivia_range)
            .unwrap_or_else(|| node.text_range())
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
        if next_delimiter.is_none() {
            // Returns the previous delimiter token (if any).
            ink_analyzer_ir::closest_item_which(
                token,
                SyntaxToken::prev_token,
                |subject| subject.kind() == delimiter,
                |subject| !subject.kind().is_trivia(),
            )
        } else {
            None
        }
        .as_ref()
        .unwrap_or(token)
        .text_range()
        .start(),
        // Either the end of the next delimiter token (if any) or the end of the target token.
        next_delimiter
            .as_ref()
            .map(SyntaxToken::text_range)
            .unwrap_or_else(|| token.text_range())
            .end(),
    )
}

/// Returns text range of the syntax node and it's immediate (next or previous) delimiter (e.g comma - ",").
pub fn node_and_delimiter_range(node: &SyntaxNode, delimiter: SyntaxKind) -> TextRange {
    // Gets the end position.
    let end = node
        .last_token()
        .as_ref()
        .map(|token| token_and_delimiter_range(token, delimiter))
        .unwrap_or_else(|| node.text_range())
        .end();
    TextRange::new(
        // Either the start of the previous delimiter token (if any and there's no next delimiter)
        // or the start of the target node.
        if end == node.text_range().end() {
            // Returns a text range including previous delimiter token (if any).
            // Previous is implied because we know there's no next delimiter
            // because `end == node.text_range().end()`.
            node.first_token()
                .map(|token| token_and_delimiter_range(&token, delimiter))
        } else {
            None
        }
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
    let last_token_option = arg.meta().elements().last().and_then(|elem| match elem {
        SyntaxElement::Node(node) => node.last_token(),
        SyntaxElement::Token(token) => Some(token.clone()),
    });

    // Determines the parent attribute for the argument.
    if let Some(attr) = parent_attr_option.cloned().or_else(|| {
        last_token_option
            .as_ref()
            .and_then(|token| {
                ink_analyzer_ir::closest_ancestor_ast_type::<SyntaxToken, ast::Attr>(token)
            })
            .and_then(InkAttribute::cast)
    }) {
        if attr.args().len() == 1 {
            match attr.kind() {
                // Returns the text range for the attribute meta, if the attribute has only a single argument.
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
        .map(|token| token_and_delimiter_range(token, SyntaxKind::COMMA))
        .unwrap_or_else(|| arg.text_range())
        .end();

    // Returns the text range of attribute argument + delimiter (if any) .
    TextRange::new(
        if end == arg.text_range().end() {
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
                .or_else(|| arg.meta().eq().map(|eq| eq.syntax().clone()))
                // First token argument value if no name nor equal symbol is present.
                .or_else(|| {
                    arg.meta()
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
                        })
                })
        } else {
            None
        }
        .as_ref()
        // Returns a text range including previous delimiter token (if any).
        // Previous is implied because we know there's no next delimiter
        // because `end == arg.text_range().end()`.
        .map(|token| token_and_delimiter_range(token, SyntaxKind::COMMA))
        .unwrap_or_else(|| arg.text_range())
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
        .or_else(|| item_list.l_curly_token().map(|it| it.text_range().end()))
        // Defaults to inserts before the first item in the item list.
        .or_else(|| {
            item_list
                .items()
                .next()
                .map(|it| it.syntax().text_range().start())
        })
        .unwrap_or(item_list.syntax().text_range().start())
}

/// Returns the offset for the end of an item list (e.g body of an AST item - i.e `mod` e.t.c).
pub fn item_insert_offset_end(item_list: &ast::ItemList) -> TextSize {
    item_list
        // Determines position after the last item in the item list.
        .items()
        .last()
        // Defaults to the start if item list is empty because it's easier to apply additional formatting that way.
        .map(|it| it.syntax().text_range().end())
        .unwrap_or_else(|| item_insert_offset_start(item_list))
}

/// Returns the offset after the end of the last `struct` (if any) in an item list (e.g body of an AST item - i.e `mod` e.t.c).
/// Defaults to the beginning of the item list if no `struct`s are present.
pub fn item_insert_offset_after_last_struct_or_start(item_list: &ast::ItemList) -> TextSize {
    item_list
        .items()
        .filter(|it| matches!(it, ast::Item::Struct(_)))
        .last()
        .map(|it| it.syntax().text_range().end())
        .unwrap_or_else(|| item_insert_offset_start(item_list))
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
        .or_else(|| {
            item_list
                .items()
                .filter(|it| matches!(it, ast::Item::Struct(_)))
                .last()
        })
        .map(|it| it.syntax().text_range().end())
        // Or before the first `mod` item.
        .or_else(|| {
            item_list
                .items()
                .find(|it| matches!(it, ast::Item::Module(_)))
                .map(|it| it.syntax().text_range().start())
        })
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
        .or_else(|| {
            assoc_item_list
                .assoc_items()
                .next()
                .map(|it| it.syntax().text_range().start())
        })
        .unwrap_or(assoc_item_list.syntax().text_range().start())
}

/// Returns the offset for the end of an associated item list (e.g body of an AST item - i.e `fn`, `trait` e.t.c).
pub fn assoc_item_insert_offset_end(assoc_item_list: &ast::AssocItemList) -> TextSize {
    assoc_item_list
        // Determines position after the last item in the associated item list.
        .assoc_items()
        .last()
        // Defaults to the start if associated item list is empty because it's easier to apply additional formatting that way.
        .map(|it| it.syntax().text_range().end())
        .unwrap_or_else(|| assoc_item_insert_offset_start(assoc_item_list))
}

/// Returns the offset for the beginning of a field list and affixes (prefix and suffix).
pub fn field_insert_offset_start_and_affixes(
    field_list: &ast::FieldList,
) -> (TextSize, Option<String>, Option<String>) {
    match field_list {
        ast::FieldList::RecordFieldList(record_field_list) => record_field_list
            .l_curly_token()
            .map(|it| (it.text_range().end(), None, None))
            .or_else(|| {
                record_field_list.fields().next().map(|it| {
                    (
                        it.syntax().text_range().start(),
                        None,
                        Some(format!(
                            "\n{}",
                            item_children_indenting(field_list.syntax())
                        )),
                    )
                })
            })
            .unwrap_or((record_field_list.syntax().text_range().start(), None, None)),
        ast::FieldList::TupleFieldList(tuple_field_list) => tuple_field_list
            .l_paren_token()
            .map(|it| (it.text_range().end(), None, None))
            .or_else(|| {
                tuple_field_list.fields().next().map(|it| {
                    (
                        it.syntax().text_range().start(),
                        None,
                        Some(", ".to_owned()),
                    )
                })
            })
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
        (!has_comma || is_block).then(|| {
            format!(
                "{}{}",
                if has_comma { "" } else { "," },
                if is_block { "\n" } else { " " }
            )
        })
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
        .or_else(|| {
            contract
                .impls()
                .iter()
                .find(|it| it.trait_type().is_none())
                .and_then(InkImpl::impl_item)
        })
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
        .or_else(|| {
            contract
                .module()
                .and_then(ast::Module::item_list)
                .and_then(|item_list| {
                    callable_impl_indent_and_affixes(contract).map(|(indent, prefix, suffix)| {
                        // Sets insert offset at the end of the associated items list, insert indent based on contract `mod` block
                        // and affixes (prefix and suffix) for wrapping callable in an `impl` block.
                        (
                            item_insert_offset_impl(&item_list),
                            indent,
                            Some(prefix),
                            Some(suffix),
                        )
                    })
                })
        })
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
        .or_else(|| {
            contract
                .module()
                .and_then(HasName::name)
                .as_ref()
                .map(ToString::to_string)
                .as_deref()
                .map(utils::pascal_case)
        })
}

/// Applies indenting to a snippet.
pub fn apply_indenting(input: &str, indent: &str) -> String {
    if indent.is_empty() {
        input.to_owned()
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
        input.to_owned()
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

// Checks whether the given text starts with at least 2 new lines
// (the new lines can be interspersed with other whitespace).
pub fn starts_with_two_or_more_newlines(text: &str) -> bool {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^([^\S\n]*\n[^\S\n]*){2,}").unwrap());
    RE.is_match(text)
}

/// Suggests a unique/unused id for an constructor, message or extension function.
pub fn suggest_unique_id_mut<T>(
    preferred_id: Option<T>,
    unavailable_ids: &mut HashSet<T>,
) -> Option<T>
where
    T: IsIntId,
{
    // Finds a unique/unused id.
    let suggested_id = suggest_unique_id(preferred_id, unavailable_ids)?;

    // Makes the id unavailable for future calls to this function.
    unavailable_ids.insert(suggested_id);

    // Returns the unique id.
    Some(suggested_id)
}

/// Suggests a unique/unused id for an constructor, message or extension function.
pub fn suggest_unique_id<T>(preferred_id: Option<T>, unavailable_ids: &HashSet<T>) -> Option<T>
where
    T: IsIntId,
{
    // Finds a unique/unused id.
    let mut suggested_id = preferred_id.unwrap_or(1.into());
    while unavailable_ids.contains(&suggested_id) {
        if suggested_id == T::MAX {
            // Bail if we've already reached the max value.
            return None;
        }
        suggested_id += 1.into();
    }

    // Returns the unique id.
    Some(suggested_id)
}

/// Suggests a unique/unused name for an constructor, message or extension function.
pub fn suggest_unique_name(preferred_name: &str, unavailable_names: &HashSet<String>) -> String {
    // Finds a unique/unused name.
    let mut suggested_name = preferred_name.to_owned();
    let mut suffix = 2;
    while unavailable_names.contains(&suggested_name) {
        if suffix == u8::MAX {
            // Bail if we can't find a unique name after all these tries and use the preferred name.
            preferred_name.clone_into(&mut suggested_name);
            break;
        }
        suggested_name = format!("{preferred_name}{suffix}");
        suffix += 1;
    }

    // Returns the suggested name.
    suggested_name
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
/// (i.e. tokens between meta - attributes/rustdoc - and the start of the item list).
pub fn ink_impl_declaration_range(ink_impl: &InkImpl) -> TextRange {
    ink_impl
        .impl_item()
        .and_then(|it| ast_item_declaration_range(&ast::Item::Impl(it.clone())))
        .unwrap_or(ink_impl.syntax().text_range())
}

/// Returns text range of the `trait` "declaration"
/// (i.e. tokens between meta - attributes/rustdoc - and the start of the item list) for a trait-based ink! entity.
pub fn ink_trait_declaration_range<T>(ink_trait_item: &T) -> TextRange
where
    T: IsInkTrait,
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

/// Returns true if the syntax node is a trait definition implementation message.
pub fn is_trait_definition_impl_message(target: &SyntaxNode) -> bool {
    Message::can_cast(target)
        && Message::cast(target.clone())
            .expect("Should be able to cast to message.")
            .parent_impl_item()
            .is_some_and(|impl_item| impl_item.trait_().is_some())
}

/// Converts token tree into a non-delimited (i.e. with brackets excluded) string.
///
/// # Example:
/// `TokenTree` for `#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]` becomes
/// "scale::Encode, scale::Decode, scale_info::TypeInfo"
pub fn token_tree_to_non_delimited_meta_string(token_tree: &ast::TokenTree) -> String {
    let r_paren_option = token_tree.r_paren_token();
    token_tree
        .syntax()
        .children_with_tokens()
        .skip(usize::from(token_tree.l_paren_token().is_some()))
        .take_while(|it| r_paren_option.is_none() || it.as_token() != r_paren_option.as_ref())
        .join("")
}
