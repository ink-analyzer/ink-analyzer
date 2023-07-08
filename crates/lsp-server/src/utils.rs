//! ink! Language Server utilities.

use lsp_types::{ClientCapabilities, CodeActionKind, PositionEncodingKind};
use std::collections::HashSet;

const SERVER_CODE_ACTION_KINDS: [CodeActionKind; 4] = [
    CodeActionKind::EMPTY,
    CodeActionKind::QUICKFIX,
    CodeActionKind::REFACTOR,
    CodeActionKind::REFACTOR_REWRITE,
];

/// Returns the preferred LSP `PositionEncodingKind` based on the LSP client's capabilities.
pub fn position_encoding(client_capabilities: &ClientCapabilities) -> PositionEncodingKind {
    client_capabilities
        .general
        .as_ref()
        .and_then(|it| it.position_encodings.as_deref())
        .unwrap_or_default()
        .iter()
        .find_map(|encoding| {
            // Prefer the first of UTF-8 or UTF-32 if supported by the client
            // because they don't require any re-encoding.
            (encoding == &PositionEncodingKind::UTF8 || encoding == &PositionEncodingKind::UTF32)
                .then_some(encoding.clone())
        })
        // Fallback to UTF-16 if either no encoding where sent by client or
        // if neither UTF-8 nor UTF-32 are supported by the client.
        .unwrap_or(PositionEncodingKind::UTF16)
}

/// Returns the supported LSP `CodeActionKind`s (if any) based on the LSP client's capabilities.
pub fn code_actions_kinds(client_capabilities: &ClientCapabilities) -> Option<Vec<CodeActionKind>> {
    client_capabilities
        .text_document
        .as_ref()
        .and_then(|it| it.code_action.as_ref())
        .and_then(|it| it.code_action_literal_support.as_ref())
        .and_then(|it| {
            // If client defines supported code action kinds,
            // return only code actions supported by both the client and server (if any), otherwise return none.
            let code_actions_kinds: Vec<CodeActionKind> = HashSet::from(SERVER_CODE_ACTION_KINDS)
                .intersection(
                    &it.code_action_kind
                        .value_set
                        .iter()
                        .map(|it| CodeActionKind::from(it.to_string()))
                        .collect::<HashSet<CodeActionKind>>(),
                )
                .cloned()
                .collect();
            (!code_actions_kinds.is_empty()).then_some(code_actions_kinds)
        })
}

/// Returns true if the LSP client advertises completion snippet support, or false otherwise.
pub fn snippet_support(client_capabilities: &ClientCapabilities) -> bool {
    client_capabilities
        .text_document
        .as_ref()
        .and_then(|it| it.completion.as_ref())
        .and_then(|it| it.completion_item.as_ref())
        .and_then(|it| it.snippet_support)
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::{code_actions_kinds, position_encoding, SERVER_CODE_ACTION_KINDS};
    use lsp_types::{
        CodeActionClientCapabilities, CodeActionKindLiteralSupport, CodeActionLiteralSupport,
        GeneralClientCapabilities, TextDocumentClientCapabilities,
    };

    fn config_with_encodings(encodings: Option<Vec<PositionEncodingKind>>) -> ClientCapabilities {
        ClientCapabilities {
            general: Some(GeneralClientCapabilities {
                position_encodings: encodings,
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    fn config_with_code_action_kinds(code_action_kinds: Vec<CodeActionKind>) -> ClientCapabilities {
        ClientCapabilities {
            text_document: Some(TextDocumentClientCapabilities {
                code_action: Some(CodeActionClientCapabilities {
                    code_action_literal_support: Some(CodeActionLiteralSupport {
                        code_action_kind: CodeActionKindLiteralSupport {
                            value_set: code_action_kinds
                                .into_iter()
                                .map(|code_action| code_action.as_str().to_string())
                                .collect(),
                        },
                    }),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    #[test]
    fn position_encoding_works() {
        for (client_capabilities, expected_encoding) in [
            // Default position encoding is UTF-16.
            (ClientCapabilities::default(), PositionEncodingKind::UTF16),
            // No encoding defaults to UTF-16.
            (config_with_encodings(None), PositionEncodingKind::UTF16),
            // UTF-8 and UTF-32 are preferred over UTF-16 when supported by the client.
            (
                config_with_encodings(Some(vec![
                    PositionEncodingKind::UTF16,
                    PositionEncodingKind::UTF8,
                ])),
                PositionEncodingKind::UTF8,
            ),
            (
                config_with_encodings(Some(vec![
                    PositionEncodingKind::UTF16,
                    PositionEncodingKind::UTF32,
                ])),
                PositionEncodingKind::UTF32,
            ),
            // UTF-8 or UTF-32 preference is respected.
            (
                config_with_encodings(Some(vec![
                    PositionEncodingKind::UTF8,
                    PositionEncodingKind::UTF32,
                ])),
                PositionEncodingKind::UTF8,
            ),
            (
                config_with_encodings(Some(vec![
                    PositionEncodingKind::UTF16,
                    PositionEncodingKind::UTF8,
                    PositionEncodingKind::UTF32,
                ])),
                PositionEncodingKind::UTF8,
            ),
            (
                config_with_encodings(Some(vec![
                    PositionEncodingKind::UTF32,
                    PositionEncodingKind::UTF8,
                ])),
                PositionEncodingKind::UTF32,
            ),
            (
                config_with_encodings(Some(vec![
                    PositionEncodingKind::UTF16,
                    PositionEncodingKind::UTF32,
                    PositionEncodingKind::UTF8,
                ])),
                PositionEncodingKind::UTF32,
            ),
            // UTF-16 only support works.
            (
                config_with_encodings(Some(vec![PositionEncodingKind::UTF16])),
                PositionEncodingKind::UTF16,
            ),
        ] {
            // Verifies the position encoding is properly set based on client capabilities.
            assert_eq!(position_encoding(&client_capabilities), expected_encoding);
        }
    }

    #[test]
    fn code_actions_kinds_works() {
        for (client_capabilities, expected_results) in [
            // Default is None.
            (ClientCapabilities::default(), None),
            // If client defines supported code action kinds,
            // return only code actions supported by both the client and server (if any), otherwise return none.
            (
                config_with_code_action_kinds(
                    [
                        CodeActionKind::EMPTY,
                        CodeActionKind::QUICKFIX,
                        CodeActionKind::REFACTOR,
                        CodeActionKind::REFACTOR_EXTRACT,
                        CodeActionKind::REFACTOR_INLINE,
                        CodeActionKind::REFACTOR_REWRITE,
                        CodeActionKind::SOURCE,
                        CodeActionKind::SOURCE_ORGANIZE_IMPORTS,
                        CodeActionKind::SOURCE_FIX_ALL,
                    ]
                    .to_vec(),
                ),
                Some(HashSet::from(SERVER_CODE_ACTION_KINDS)),
            ),
            (
                config_with_code_action_kinds(
                    [CodeActionKind::EMPTY, CodeActionKind::SOURCE].to_vec(),
                ),
                Some(HashSet::from([CodeActionKind::EMPTY])),
            ),
            (
                config_with_code_action_kinds(
                    [CodeActionKind::EMPTY, CodeActionKind::SOURCE].to_vec(),
                ),
                Some(HashSet::from([CodeActionKind::EMPTY])),
            ),
            (
                config_with_code_action_kinds([CodeActionKind::SOURCE].to_vec()),
                None,
            ),
        ] {
            // Verifies the position encoding is properly set based on client capabilities.
            assert_eq!(
                code_actions_kinds(&client_capabilities)
                    .map(|it| it.into_iter().collect::<HashSet<CodeActionKind>>()),
                expected_results
            );
        }
    }
}
