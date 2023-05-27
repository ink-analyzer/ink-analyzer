//! Hover content for ink! attributes.

use ink_analyzer_ir::{InkArgKind, InkAttributeKind, InkMacroKind};

mod args;
mod macros;

/// Returns documentation for the ink! attribute kind.
pub fn doc(attr_kind: &InkAttributeKind) -> &str {
    match attr_kind {
        InkAttributeKind::Arg(arg_kind) => match arg_kind {
            InkArgKind::Anonymous => args::ANONYMOUS_DOC,
            InkArgKind::Constructor => args::CONSTRUCTOR_DOC,
            InkArgKind::Default => args::DEFAULT_DOC,
            InkArgKind::Derive => args::DERIVE_DOC,
            InkArgKind::Env => args::ENV_DOC,
            InkArgKind::Event => args::EVENT_DOC,
            InkArgKind::Extension => args::EXTENSION_DOC,
            InkArgKind::HandleStatus => args::HANDLE_STATUS_DOC,
            InkArgKind::Impl => args::IMPL_DOC,
            InkArgKind::KeepAttr => args::KEEP_ATTR_DOC,
            InkArgKind::Message => args::MESSAGE_DOC,
            InkArgKind::Namespace => args::NAMESPACE_DOC,
            InkArgKind::Payable => args::PAYABLE_DOC,
            InkArgKind::Selector => args::SELECTOR_DOC,
            InkArgKind::Storage => args::STORAGE_DOC,
            InkArgKind::Topic => args::TOPIC_DOC,
            InkArgKind::Unknown => "",
        },
        InkAttributeKind::Macro(macro_kind) => match macro_kind {
            InkMacroKind::ChainExtension => macros::CHAIN_EXTENSION_DOC,
            InkMacroKind::Contract => macros::CONTRACT_DOC,
            InkMacroKind::StorageItem => macros::STORAGE_ITEM_DOC,
            InkMacroKind::Test => macros::TEST_DOC,
            InkMacroKind::TraitDefinition => macros::TRAIT_DEFINITION_DOC,
            InkMacroKind::Unknown => "",
        },
    }
}
