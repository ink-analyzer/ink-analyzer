//! LSP request handlers.

use std::str::FromStr;

use super::command;
use crate::dispatch::Snapshots;
use crate::utils::{COMMAND_CREATE_PROJECT, COMMAND_EXTRACT_EVENT, COMMAND_MIGRATE_PROJECT};
use crate::{translator, utils};

/// Handles completion request.
pub fn handle_completion(
    params: lsp_types::CompletionParams,
    snapshots: &Snapshots,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<lsp_types::CompletionResponse>> {
    // Gets document uri and retrieves document from memory.
    let id = params.text_document_position.text_document.uri.to_string();
    match snapshots.get(&id) {
        Some(snapshot) => {
            // Converts LSP position to ink! analyzer offset.
            let offset = translator::from_lsp::offset(
                params.text_document_position.position,
                &snapshot.context,
            )
            .ok_or(anyhow::format_err!("Invalid offset."))?;

            // Computes ink! analyzer completions and translates them into an LSP completion list.
            let completion_items: Vec<lsp_types::CompletionItem> = snapshot
                .analysis
                .completions(offset)
                .into_iter()
                .filter_map(|completion| {
                    translator::to_lsp::completion(
                        completion,
                        utils::snippet_support(client_capabilities),
                        &snapshot.context,
                    )
                })
                .collect();

            // Compose LSP completion response.
            Ok((!completion_items.is_empty()).then(|| {
                lsp_types::CompletionList {
                    is_incomplete: true,
                    items: completion_items,
                }
                .into()
            }))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

/// Handles hover request.
pub fn handle_hover(
    params: lsp_types::HoverParams,
    snapshots: &Snapshots,
    _: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<lsp_types::Hover>> {
    // Gets document uri and retrieves document from memory.
    let id = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();
    match snapshots.get(&id) {
        Some(snapshot) => {
            // Converts LSP position to ink! analyzer offset.
            let offset = translator::from_lsp::offset(
                params.text_document_position_params.position,
                &snapshot.context,
            )
            .ok_or(anyhow::format_err!("Invalid offset."))?;

            // Computes ink! analyzer hover content and translates it to an LSP hover.
            Ok(snapshot
                .analysis
                .hover(ink_analyzer::TextRange::empty(offset))
                .and_then(|hover| translator::to_lsp::hover(hover, &snapshot.context)))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

/// Handles code action request.
pub fn handle_code_action(
    params: lsp_types::CodeActionParams,
    snapshots: &Snapshots,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<lsp_types::CodeActionResponse>> {
    // Gets document uri and retrieves document from memory.
    let uri = params.text_document.uri;
    let id = uri.to_string();
    match snapshots.get(&id) {
        Some(snapshot) => {
            // Converts LSP range to ink! analyzer text range.
            let text_range = translator::from_lsp::text_range(params.range, &snapshot.context)
                .ok_or(anyhow::format_err!("Invalid range."))?;

            // Computes ink! analyzer actions and translates them to LSP code actions.
            Ok(Some(
                snapshot
                    .analysis
                    .actions(text_range)
                    .into_iter()
                    .filter_map(|action| {
                        translator::to_lsp::code_action(
                            action,
                            uri.clone(),
                            utils::code_action_edit_resolve_support(client_capabilities),
                            &snapshot.context,
                        )
                        .map(Into::into)
                    })
                    .collect(),
            ))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

/// Handles code action resolve request.
pub fn handle_code_action_resolve(
    mut code_action: lsp_types::CodeAction,
    snapshots: &Snapshots,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<lsp_types::CodeAction> {
    // Only project migration and event extraction edits are computed lazily.
    let Some((cmd, data)) = code_action
        .data
        .as_ref()
        .and_then(serde_json::Value::as_object)
        .and_then(|data| {
            data.get("command")
                .and_then(serde_json::Value::as_str)
                .zip(Some(data))
        })
    else {
        return Err(anyhow::format_err!("Missing command!"));
    };
    let parse_uri = || {
        data.get("uri")
            .and_then(serde_json::Value::as_str)
            .and_then(|value| lsp_types::Uri::from_str(value).ok())
    };
    match cmd {
        COMMAND_MIGRATE_PROJECT => {
            let Some(uri) = parse_uri() else {
                return Err(anyhow::format_err!("The `uri` argument is required!"));
            };

            // Computes migration edits.
            let res = command::handle_migrate_project(&uri, snapshots, client_capabilities)?;
            code_action.command = None;
            code_action.edit = Some(lsp_types::WorkspaceEdit {
                changes: Some(res.edits),
                ..Default::default()
            });
            Ok(code_action)
        }
        COMMAND_EXTRACT_EVENT => {
            let Some((uri, range)) = parse_uri().zip(data.get("range").and_then(parse_range))
            else {
                return Err(anyhow::format_err!(
                    "The `uri` and `range` arguments are required!"
                ));
            };
            // Computes event extraction edits.
            let res = command::handle_extract_event(&uri, range, snapshots, client_capabilities)?;
            code_action.command = None;
            code_action.edit = Some(lsp_types::WorkspaceEdit {
                document_changes: Some(lsp_types::DocumentChanges::from(res)),
                ..Default::default()
            });
            Ok(code_action)
        }
        _ => Err(anyhow::format_err!("Unsupported command: {}!", cmd)),
    }
}

/// Handles inlay hint request.
pub fn handle_inlay_hint(
    params: lsp_types::InlayHintParams,
    snapshots: &Snapshots,
    _: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<Vec<lsp_types::InlayHint>>> {
    // Gets document uri and retrieves document from memory.
    let uri = params.text_document.uri;
    let id = uri.to_string();
    match snapshots.get(&id) {
        Some(snapshot) => {
            // Converts LSP range to ink! analyzer text range.
            let text_range = translator::from_lsp::text_range(params.range, &snapshot.context)
                .ok_or(anyhow::format_err!("Invalid range."))?;

            // Computes ink! analyzer inlay hints and translates them to LSP inlay hints.
            Ok(Some(
                snapshot
                    .analysis
                    .inlay_hints(Some(text_range))
                    .into_iter()
                    .filter_map(|hint| translator::to_lsp::inlay_hint(hint, &snapshot.context))
                    .collect(),
            ))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

/// Handles signature help request.
pub fn handle_signature_help(
    params: lsp_types::SignatureHelpParams,
    snapshots: &Snapshots,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<lsp_types::SignatureHelp>> {
    // Gets document uri and retrieves document from memory.
    let uri = params.text_document_position_params.text_document.uri;
    let id = uri.to_string();
    match snapshots.get(&id) {
        Some(snapshot) => {
            // Converts LSP position to ink! analyzer offset.
            let offset = translator::from_lsp::offset(
                params.text_document_position_params.position,
                &snapshot.context,
            )
            .ok_or(anyhow::format_err!("Invalid offset."))?;

            // Computes ink! analyzer signature help and translates it to LSP signature help.
            Ok(translator::to_lsp::signature_help(
                &snapshot.analysis.signature_help(offset),
                &utils::signature_support(client_capabilities),
                params
                    .context
                    .as_ref()
                    .and_then(|ctx| ctx.active_signature_help.as_ref()),
                &snapshot.context,
            ))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

/// Handles execute command request.
pub fn handle_execute_command(
    params: lsp_types::ExecuteCommandParams,
    snapshots: &Snapshots,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<serde_json::Value>> {
    let parse_uri = |value: &serde_json::Value| {
        value
            .as_object()
            .and_then(|arg| arg.get("uri"))
            .and_then(serde_json::Value::as_str)
            .and_then(|value| lsp_types::Uri::from_str(value).ok())
    };
    match params.command.as_str() {
        COMMAND_CREATE_PROJECT => {
            let args = params
                .arguments
                .first()
                .and_then(serde_json::Value::as_object)
                .and_then(|arg| {
                    arg.get("name").and_then(|it| it.as_str()).zip(
                        arg.get("root").and_then(|it| it.as_str()).and_then(|it| {
                            lsp_types::Uri::from_str(&format!(
                                "{it}{}",
                                if it.ends_with('/') { "" } else { "/" }
                            ))
                            .ok()
                        }),
                    )
                });
            let Some((name, root)) = args else {
                return Err(anyhow::format_err!(
                    "The `name` and `root` arguments are required!"
                ));
            };
            command::handle_create_project(name, &root)
                .map(serde_json::to_value)
                .map(Result::ok)
        }
        COMMAND_MIGRATE_PROJECT => {
            let uri = params.arguments.first().and_then(parse_uri);
            let Some(uri) = uri else {
                return Err(anyhow::format_err!("The `uri` argument is required!"));
            };
            command::handle_migrate_project(&uri, snapshots, client_capabilities)
                .map(serde_json::to_value)
                .map(Result::ok)
        }
        COMMAND_EXTRACT_EVENT => {
            let args = params.arguments.first().and_then(|arg| {
                parse_uri(arg).zip(
                    arg.as_object()
                        .and_then(|arg| arg.get("range"))
                        .and_then(parse_range),
                )
            });
            let Some((uri, range)) = args else {
                return Err(anyhow::format_err!(
                    "The `uri` and `range` arguments are required!"
                ));
            };
            command::handle_extract_event(&uri, range, snapshots, client_capabilities)
                .map(serde_json::to_value)
                .map(Result::ok)
        }
        _ => Err(anyhow::format_err!("Unknown command: {}!", params.command)),
    }
}

fn parse_range(value: &serde_json::Value) -> Option<ink_analyzer::TextRange> {
    value
        .as_object()
        .and_then(|arg| {
            arg.get("start")
                .and_then(serde_json::Value::as_u64)
                .zip(arg.get("end").and_then(serde_json::Value::as_u64))
        })
        .map(|(start, end)| {
            ink_analyzer::TextRange::new(
                ink_analyzer::TextSize::from(start as u32),
                ink_analyzer::TextSize::from(end as u32),
            )
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::init_snapshots;
    use ink_analyzer::{MinorVersion, Version};
    use test_utils::simple_client_config;

    #[test]
    fn handle_completion_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        for version in [Version::Legacy, Version::V5(MinorVersion::Base)] {
            // Initializes snapshots with test document.
            let (snapshots, uri) =
                init_snapshots(String::from("#[ink::co]"), &client_capabilities, version);

            // Calls handler and verifies that the expected completion items are returned.
            let result = handle_completion(
                lsp_types::CompletionParams {
                    text_document_position: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri },
                        position: lsp_types::Position {
                            line: 0,
                            character: 9,
                        },
                    },
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                    context: None,
                },
                &snapshots,
                &client_capabilities,
            );
            assert!(result.is_ok());
            let completion_items = match result.unwrap().unwrap() {
                lsp_types::CompletionResponse::List(it) => Some(it),
                lsp_types::CompletionResponse::Array(_) => None,
            }
            .unwrap()
            .items;
            assert!(completion_items[0].label.contains("contract"));
        }
    }

    #[test]
    fn handle_hover_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        for version in [Version::Legacy, Version::V5(MinorVersion::Base)] {
            // Initializes snapshots with test document.
            let (snapshots, uri) = init_snapshots(
                String::from("#[ink::contract]"),
                &client_capabilities,
                version,
            );

            // Calls handler and verifies that the expected hover content is returned.
            let result = handle_hover(
                lsp_types::HoverParams {
                    text_document_position_params: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri },
                        position: lsp_types::Position {
                            line: 0,
                            character: 1,
                        },
                    },
                    work_done_progress_params: Default::default(),
                },
                &snapshots,
                &client_capabilities,
            );
            assert!(result.is_ok());
            let hover_content = match result.unwrap().unwrap().contents {
                lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(it)) => Some(it),
                _ => None,
            }
            .unwrap();
            assert!(hover_content.contains("`#[ink::contract]`"));
        }
    }

    #[test]
    fn handle_code_action_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        for version in [Version::Legacy, Version::V5(MinorVersion::Base)] {
            // Initializes snapshots with test document.
            let (snapshots, uri) = init_snapshots(
                String::from("mod my_contract {}"),
                &client_capabilities,
                version,
            );

            // Calls handler and verifies that the expected code actions are returned.
            let result = handle_code_action(
                lsp_types::CodeActionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    range: lsp_types::Range {
                        start: lsp_types::Position {
                            line: 0,
                            character: 0,
                        },
                        end: lsp_types::Position {
                            line: 0,
                            character: 15,
                        },
                    },
                    context: Default::default(),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                },
                &snapshots,
                &client_capabilities,
            );
            assert!(result.is_ok());
            let code_actions = result.unwrap().unwrap();
            assert!(match &code_actions[0] {
                lsp_types::CodeActionOrCommand::CodeAction(it) => Some(it),
                lsp_types::CodeActionOrCommand::Command(_) => None,
            }
            .unwrap()
            .title
            .contains("Add ink! contract"));
        }
    }

    #[test]
    fn handle_inlay_hint_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        for version in [Version::Legacy, Version::V5(MinorVersion::Base)] {
            // Initializes snapshots with test document.
            let (snapshots, uri) = init_snapshots(
                String::from(r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#),
                &client_capabilities,
                version,
            );

            // Calls handler and verifies that the expected inlay hints are returned.
            let result = handle_inlay_hint(
                lsp_types::InlayHintParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    range: lsp_types::Range {
                        start: lsp_types::Position {
                            line: 0,
                            character: 0,
                        },
                        end: lsp_types::Position {
                            line: 0,
                            character: 57,
                        },
                    },
                    work_done_progress_params: Default::default(),
                },
                &snapshots,
                &client_capabilities,
            );
            assert!(result.is_ok());
            let inlay_hints = result.unwrap().unwrap();
            assert_eq!(
                match &inlay_hints[0].label {
                    lsp_types::InlayHintLabel::String(value) => Some(value.as_str()),
                    _ => None,
                }
                .unwrap(),
                ": impl Environment"
            );
            assert_eq!(
                match &inlay_hints[1].label {
                    lsp_types::InlayHintLabel::String(value) => Some(value.as_str()),
                    _ => None,
                }
                .unwrap(),
                ": &str"
            );
            assert!(match &inlay_hints[1].tooltip.as_ref().unwrap() {
                lsp_types::InlayHintTooltip::String(value) => Some(value.as_str()),
                _ => None,
            }
            .unwrap()
            .contains("comma separated"));
        }
    }

    #[test]
    fn handle_signature_help_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        for version in [Version::Legacy, Version::V5(MinorVersion::Base)] {
            // Initializes snapshots with test document.
            let (snapshots, uri) = init_snapshots(
                String::from("#[ink::contract()]"),
                &client_capabilities,
                version,
            );

            // Calls handler and verifies that the expected signature help is returned.
            let result = handle_signature_help(
                lsp_types::SignatureHelpParams {
                    text_document_position_params: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri },
                        position: lsp_types::Position {
                            line: 0,
                            character: 16,
                        },
                    },
                    work_done_progress_params: Default::default(),
                    context: None,
                },
                &snapshots,
                &simple_client_config(),
            );
            assert!(result.is_ok());
            let signature_help = result.unwrap().unwrap();
            let signature_label = &signature_help.signatures[0].label;
            assert_eq!(
                signature_help.signatures[0].label,
                "env: impl Environment, keep_attr: &str"
            );
            let params: Vec<[u32; 2]> = signature_help.signatures[0]
                .parameters
                .as_ref()
                .unwrap()
                .iter()
                .map(|param| match &param.label {
                    lsp_types::ParameterLabel::LabelOffsets(offsets) => [offsets[0], offsets[1]],
                    lsp_types::ParameterLabel::Simple(label) => {
                        let end_offset = test_utils::parse_offset_at(signature_label, Some(label))
                            .unwrap() as u32;
                        let start_offset = end_offset - label.len() as u32;
                        [start_offset, end_offset]
                    }
                })
                .collect();
            assert_eq!(params, vec![[0, 21], [23, 38]]);
            assert_eq!(signature_help.active_parameter.unwrap(), 0);
        }
    }
}
