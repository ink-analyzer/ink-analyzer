//! ink! attribute hover content.

mod args;
mod macros;

use ink_analyzer_ir::syntax::{AstNode, AstToken, TextRange};
use ink_analyzer_ir::{InkArgKind, InkAttributeKind, InkFile, InkMacroKind};

use crate::analysis::utils;
use crate::Version;

/// An ink! attribute hover result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hover {
    /// Range the hover content applies to.
    pub range: TextRange,
    /// Hover text.
    pub content: String,
}

/// Returns descriptive/informational text for the ink! attribute at the given position (if any).
pub fn hover(file: &InkFile, range: TextRange, version: Version) -> Option<Hover> {
    // Finds the covering ink! attribute for the text range (if any).
    let covering_ink_attr = utils::covering_ink_attribute(file, range);

    // Returns hover content only if the text range is covered by an ink! attribute.
    covering_ink_attr.and_then(|ink_attr| {
        // Finds the covered ink! attribute argument (if any).
        let ink_arg = ink_attr
            .args()
            .iter()
            .find(|arg| arg.text_range().contains_range(range));
        match ink_arg {
            // Returns hover content for the covered ink! attribute argument if it's valid.
            Some(ink_arg) => {
                let arg_attr_kind = InkAttributeKind::Arg(*ink_arg.kind());
                let primary_attr_kind =
                    (*ink_attr.kind() != arg_attr_kind).then_some(ink_attr.kind());
                let doc = content(&arg_attr_kind, version, primary_attr_kind);
                (!doc.is_empty()).then(|| Hover {
                    range: ink_arg
                        .name()
                        .map(|ink_arg_name| ink_arg_name.syntax().text_range())
                        .unwrap_or_else(|| ink_arg.text_range()),
                    content: doc.to_owned(),
                })
            }
            // Returns hover content based on the ink! attribute macro, ink! e2e attribute macro
            // or "primary" ink! attribute argument for the ink! attribute.
            None => {
                let doc = content(ink_attr.kind(), version, None);
                (!doc.is_empty()).then(|| Hover {
                    range: match ink_attr.kind() {
                        InkAttributeKind::Arg(_) => ink_attr
                            .ink_arg_name()
                            .map(|ink_arg_name| ink_arg_name.syntax().text_range())
                            .unwrap_or_else(|| ink_attr.syntax().text_range()),
                        InkAttributeKind::Macro(_) => ink_attr
                            .ink_macro()
                            .map(|path_segment| path_segment.syntax().text_range())
                            .unwrap_or_else(|| ink_attr.syntax().text_range()),
                    },
                    content: doc.to_owned(),
                })
            }
        }
    })
}

/// Returns documentation for the ink! attribute kind.
pub fn content(
    attr_kind: &InkAttributeKind,
    version: Version,
    primary_attr_kind: Option<&InkAttributeKind>,
) -> &'static str {
    match attr_kind {
        InkAttributeKind::Arg(arg_kind) => match arg_kind {
            InkArgKind::AdditionalContracts if version.is_v5() => {
                args::ADDITIONAL_CONTRACTS_DOC_V5_DEPRECATED
            }
            InkArgKind::AdditionalContracts => args::ADDITIONAL_CONTRACTS_DOC,
            InkArgKind::Anonymous if version.is_v5() => args::ANONYMOUS_DOC_V5,
            InkArgKind::Anonymous => args::ANONYMOUS_DOC,
            // We enumerate the nested args for `backend` arg here, but, at the moment,
            // only the "top-level" `backend` arg is ever sent even when focused on a nested arg.
            InkArgKind::Backend
            | InkArgKind::Node
            | InkArgKind::Url
            | InkArgKind::RuntimeOnly
            | InkArgKind::Sandbox
                if version.is_v5_0_x() =>
            {
                args::BACKEND_DOC_V5_0
            }
            InkArgKind::Backend
            | InkArgKind::Node
            | InkArgKind::Url
            | InkArgKind::RuntimeOnly
            | InkArgKind::Sandbox
                if version.is_gte_v5_1() =>
            {
                args::BACKEND_DOC
            }
            InkArgKind::Constructor => args::CONSTRUCTOR_DOC,
            InkArgKind::Decode if version.is_v5() => args::DECODE_DOC,
            InkArgKind::Default => args::DEFAULT_DOC,
            InkArgKind::Derive => args::DERIVE_DOC,
            InkArgKind::Encode if version.is_v5() => args::ENCODE_DOC,
            InkArgKind::Env | InkArgKind::Environment => args::ENV_DOC,
            InkArgKind::Event => args::EVENT_DOC,
            InkArgKind::Extension if version.is_v5() => args::EXTENSION_DOC_V5,
            InkArgKind::Extension => args::EXTENSION_DOC,
            InkArgKind::Function if version.is_v5() => args::FUNCTION_DOC,
            InkArgKind::HandleStatus => args::HANDLE_STATUS_DOC,
            InkArgKind::Impl => args::IMPL_DOC,
            InkArgKind::KeepAttr
                if version.is_v5()
                    && matches!(
                        primary_attr_kind,
                        Some(InkAttributeKind::Macro(InkMacroKind::E2ETest))
                    ) =>
            {
                args::KEEP_ATTR_E2E_DOC_V5_DEPRECATED
            }
            InkArgKind::KeepAttr if version.is_v5() => args::KEEP_ATTR_DOC_V5,
            InkArgKind::KeepAttr => args::KEEP_ATTR_DOC,
            InkArgKind::Message => args::MESSAGE_DOC,
            InkArgKind::Namespace => args::NAMESPACE_DOC,
            InkArgKind::Payable => args::PAYABLE_DOC,
            InkArgKind::Selector if version.is_v5() => args::SELECTOR_DOC_V5,
            InkArgKind::Selector => args::SELECTOR_DOC,
            InkArgKind::SignatureTopic if version.is_v5() => args::SIGNATURE_TOPIC,
            InkArgKind::Storage => args::STORAGE_DOC,
            InkArgKind::Topic if version.is_v5() => args::TOPIC_DOC_V5,
            InkArgKind::Topic => args::TOPIC_DOC,
            InkArgKind::TypeInfo if version.is_v5() => args::TYPE_INFO_DOC,
            _ => "",
        },
        InkAttributeKind::Macro(macro_kind) => match macro_kind {
            InkMacroKind::ChainExtension if version.is_v5() => macros::CHAIN_EXTENSION_DOC_V5,
            InkMacroKind::ChainExtension => macros::CHAIN_EXTENSION_DOC,
            InkMacroKind::Contract => macros::CONTRACT_DOC,
            InkMacroKind::Event if version.is_v5() => macros::EVENT_DOC,
            InkMacroKind::ScaleDerive if version.is_v5() => macros::SCALE_DERIVE_DOC,
            InkMacroKind::StorageItem => macros::STORAGE_ITEM_DOC,
            InkMacroKind::Test => macros::TEST_DOC,
            InkMacroKind::TraitDefinition => macros::TRAIT_DEFINITION_DOC,
            InkMacroKind::E2ETest => macros::E2E_TEST_DOC,
            _ => "",
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{syntax::TextSize, InkArgKind, InkMacroKind, MinorVersion};
    use test_utils::parse_offset_at;

    fn content(attr_kind: &InkAttributeKind) -> (&str, &str) {
        (
            super::content(attr_kind, Version::V4, None),
            super::content(attr_kind, Version::V5(MinorVersion::V5_0), None),
        )
    }

    #[test]
    fn hover_works() {
        for (code, test_cases) in [
            // (code, pat, [(text, pat_start, pat_end)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // text = text expected to be present in the hover content (represented without whitespace for simplicity),
            // pat_start = substring used to find the start of the hover offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the hover offset (see `test_utils::parse_offset_at` doc).

            // ink! attribute macros.
            (
                "#[ink::contract]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-contract"),
                        Some("contract"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                ],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                "#,
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-contract"),
                        Some("contract"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-env="),
                        Some("(env"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Env)),
                            Some("<-env="),
                            Some("(env"),
                        )),
                    ),
                    (
                        Some("<-my::env::Types"),
                        Some("my::env::Types"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Env)),
                            Some("<-env="),
                            Some("(env"),
                        )),
                    ),
                    (
                        Some("<-,"),
                        Some(","),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-keep_attr"),
                        Some("keep_attr"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::KeepAttr)),
                            Some("<-keep_attr"),
                            Some("keep_attr"),
                        )),
                    ),
                    (
                        Some(r#"<-"foo,bar""#),
                        Some(r#""foo,bar""#),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::KeepAttr)),
                            Some("<-keep_attr"),
                            Some("keep_attr"),
                        )),
                    ),
                ],
            ),
            (
                "#[ink_e2e::test]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::E2ETest)),
                            Some("<-test"),
                            Some("test"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::E2ETest)),
                            Some("<-test"),
                            Some("test"),
                        )),
                    ),
                    (
                        Some("<-test"),
                        Some("test"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::E2ETest)),
                            Some("<-test"),
                            Some("test"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content(&InkAttributeKind::Macro(InkMacroKind::E2ETest)),
                            Some("<-test"),
                            Some("test"),
                        )),
                    ),
                ],
            ),
            // ink! attribute arguments.
            (
                "#[ink(storage)]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                    (
                        Some("<-storage"),
                        Some("storage"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                ],
            ),
            (
                "#[ink(message, default, payable, selector=_)]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-message"),
                        Some("message"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-payable"),
                        Some("payable"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Payable)),
                            Some("<-payable"),
                            Some("payable"),
                        )),
                    ),
                    (
                        Some("<-selector"),
                        Some("selector"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Selector)),
                            Some("<-selector"),
                            Some("selector"),
                        )),
                    ),
                    (
                        Some("<-_"),
                        Some("_"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Selector)),
                            Some("<-selector"),
                            Some("selector"),
                        )),
                    ),
                ],
            ),
            (
                "#[ink(extension=1, handle_status=true)]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-extension"),
                        Some("extension"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-1"),
                        Some("1"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-handle_status"),
                        Some("handle_status"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::HandleStatus)),
                            Some("<-handle_status"),
                            Some("handle_status"),
                        )),
                    ),
                    (
                        Some("<-true"),
                        Some("true"),
                        Some((
                            content(&InkAttributeKind::Arg(InkArgKind::HandleStatus)),
                            Some("<-handle_status"),
                            Some("handle_status"),
                        )),
                    ),
                ],
            ),
        ] {
            for (pat_start, pat_end, expect_result) in test_cases {
                let range = TextRange::new(
                    TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                    TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32),
                );

                let result_v4 = hover(&InkFile::parse(code), range, Version::V4);
                let result_v5 = hover(
                    &InkFile::parse(code),
                    range,
                    Version::V5(MinorVersion::V5_0),
                );

                assert_eq!(
                    result_v4
                        .as_ref()
                        .map(|hover_result| (hover_result.content.as_str(), hover_result.range)),
                    expect_result.map(|((content_v4, _), pat_start, pat_end)| (
                        content_v4,
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    )),
                    "code: {code}, start: {:?}, end: {:?}",
                    pat_start,
                    pat_end
                );

                assert_eq!(
                    result_v5
                        .as_ref()
                        .map(|hover_result| (hover_result.content.as_str(), hover_result.range)),
                    expect_result.map(|((_, content_v5), pat_start, pat_end)| (
                        content_v5,
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    )),
                    "code: {code}, start: {:?}, end: {:?}",
                    pat_start,
                    pat_end
                );
            }
        }
    }
}
