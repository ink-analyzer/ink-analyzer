//! ink! Language Server utilities.

use lsp_server::RequestId;
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

/// Information about supported signature information features.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SignatureSupport {
    pub active_parameter_support: bool,
    pub label_offset_support: bool,
}

/// Returns information about supported signature information features based on the LSP client's capabilities.
pub fn signature_support(client_capabilities: &ClientCapabilities) -> SignatureSupport {
    client_capabilities
        .text_document
        .as_ref()
        .and_then(|it| it.signature_help.as_ref())
        .and_then(|it| it.signature_information.as_ref())
        .map_or(
            SignatureSupport {
                active_parameter_support: false,
                label_offset_support: false,
            },
            |it| SignatureSupport {
                active_parameter_support: it.active_parameter_support.unwrap_or(false),
                label_offset_support: it
                    .parameter_information
                    .as_ref()
                    .and_then(|it| it.label_offset_support)
                    .unwrap_or(false),
            },
        )
}

/// Returns a string representation of the request id
/// but only if its internal representation is a `String` else returns None.
pub fn request_id_as_str(id: RequestId) -> Option<String> {
    id.to_string()
        .strip_prefix('\"')
        .and_then(|it| it.strip_suffix('\"'))
        .map(ToString::to_string)
}

/// Returns true if the LSP client advertises capabilities needed to create new projects via workspace edit, or false otherwise.
pub fn can_create_project_via_workspace_edit(client_capabilities: &ClientCapabilities) -> bool {
    client_capabilities.workspace.as_ref().is_some_and(|it| {
        // Checks support for workspace/applyEdit requests.
        it.apply_edit.unwrap_or(false)
            && it.workspace_edit.as_ref().is_some_and(|it| {
                // Checks support for versioned document changes.
                it.document_changes.unwrap_or(false)
                    // Checks support for create file operation.
                    && it.resource_operations.as_ref().is_some_and(|it| {
                        it.contains(&lsp_types::ResourceOperationKind::Create)
                    })
            })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::{code_actions_kinds, position_encoding, SERVER_CODE_ACTION_KINDS};
    use lsp_types::{
        CodeActionClientCapabilities, CodeActionKindLiteralSupport, CodeActionLiteralSupport,
        CompletionClientCapabilities, CompletionItemCapability, GeneralClientCapabilities,
        ParameterInformationSettings, ResourceOperationKind, SignatureHelpClientCapabilities,
        SignatureInformationSettings, TextDocumentClientCapabilities, WorkspaceClientCapabilities,
        WorkspaceEditClientCapabilities,
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

    fn config_with_snippet_support(snippet_support: Option<bool>) -> ClientCapabilities {
        ClientCapabilities {
            text_document: Some(TextDocumentClientCapabilities {
                completion: Some(CompletionClientCapabilities {
                    completion_item: Some(CompletionItemCapability {
                        snippet_support,
                        ..Default::default()
                    }),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    fn config_with_signature_support(
        active_parameter_support: Option<bool>,
        label_offset_support: Option<bool>,
    ) -> ClientCapabilities {
        ClientCapabilities {
            text_document: Some(TextDocumentClientCapabilities {
                signature_help: Some(SignatureHelpClientCapabilities {
                    signature_information: Some(SignatureInformationSettings {
                        parameter_information: Some(ParameterInformationSettings {
                            label_offset_support,
                        }),
                        active_parameter_support,
                        ..Default::default()
                    }),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    fn config_with_workspace_edit_capabilities(
        apply_edit: Option<bool>,
        document_changes: Option<bool>,
        resource_operations: Option<Vec<ResourceOperationKind>>,
    ) -> ClientCapabilities {
        ClientCapabilities {
            workspace: Some(WorkspaceClientCapabilities {
                apply_edit,
                workspace_edit: Some(WorkspaceEditClientCapabilities {
                    document_changes,
                    resource_operations,
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

    #[test]
    fn snippet_support_works() {
        for (client_capabilities, expected_result) in [
            // Default is `false`.
            (ClientCapabilities::default(), false),
            // None is `false`.
            (config_with_snippet_support(None), false),
            // Set flag is properly parsed.
            (config_with_snippet_support(Some(true)), true),
            (config_with_snippet_support(Some(false)), false),
        ] {
            // Verifies the snippet support is parsed properly based on client capabilities.
            assert_eq!(snippet_support(&client_capabilities), expected_result);
        }
    }

    #[test]
    fn signature_support_works() {
        for (client_capabilities, expected_result) in [
            // Default is `false`.
            (
                ClientCapabilities::default(),
                SignatureSupport {
                    active_parameter_support: false,
                    label_offset_support: false,
                },
            ),
            // None is `false`.
            (
                config_with_signature_support(None, None),
                SignatureSupport {
                    active_parameter_support: false,
                    label_offset_support: false,
                },
            ),
            // Set flag is properly parsed.
            (
                config_with_signature_support(Some(true), Some(true)),
                SignatureSupport {
                    active_parameter_support: true,
                    label_offset_support: true,
                },
            ),
            (
                config_with_signature_support(Some(true), Some(false)),
                SignatureSupport {
                    active_parameter_support: true,
                    label_offset_support: false,
                },
            ),
            (
                config_with_signature_support(Some(false), Some(true)),
                SignatureSupport {
                    active_parameter_support: false,
                    label_offset_support: true,
                },
            ),
            (
                config_with_signature_support(Some(false), Some(false)),
                SignatureSupport {
                    active_parameter_support: false,
                    label_offset_support: false,
                },
            ),
        ] {
            // Verifies the signature support is parsed properly based on client capabilities.
            assert_eq!(signature_support(&client_capabilities), expected_result);
        }
    }

    #[test]
    fn can_create_project_via_workspace_edit_works() {
        for (client_capabilities, expected_result) in [
            // Default is `false`.
            (ClientCapabilities::default(), false),
            // None is `false`.
            (
                config_with_workspace_edit_capabilities(None, None, None),
                false,
            ),
            // Partial support for some conditions fails.
            (
                config_with_workspace_edit_capabilities(Some(true), None, None),
                false,
            ),
            (
                config_with_workspace_edit_capabilities(None, Some(true), None),
                false,
            ),
            (
                config_with_workspace_edit_capabilities(
                    None,
                    None,
                    Some(vec![ResourceOperationKind::Create]),
                ),
                false,
            ),
            (
                config_with_workspace_edit_capabilities(
                    Some(true),
                    Some(true),
                    // Missing create operation.
                    Some(vec![
                        ResourceOperationKind::Delete,
                        ResourceOperationKind::Rename,
                    ]),
                ),
                false,
            ),
            // Full support for all conditions works.
            (
                config_with_workspace_edit_capabilities(
                    Some(true),
                    Some(true),
                    Some(vec![ResourceOperationKind::Create]),
                ),
                true,
            ),
        ] {
            // Verifies the expected result based on client capabilities.
            assert_eq!(
                can_create_project_via_workspace_edit(&client_capabilities),
                expected_result
            );
        }
    }
}
