//! LSP request handlers.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::dispatch::Snapshots;
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
            Ok((!completion_items.is_empty()).then_some(
                lsp_types::CompletionList {
                    is_incomplete: true,
                    items: completion_items,
                }
                .into(),
            ))
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
    _: &lsp_types::ClientCapabilities,
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
                        translator::to_lsp::code_action(action, uri.clone(), &snapshot.context)
                            .map(Into::into)
                    })
                    .collect(),
            ))
        }
        // Empty response for missing documents.
        None => Ok(None),
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
                    .filter_map(|hint| {
                        translator::to_lsp::inlay_hint(hint, &snapshot.context).map(Into::into)
                    })
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

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateProjectResponse {
    pub name: String,
    pub uri: lsp_types::Url,
    pub files: HashMap<lsp_types::Url, String>,
}

/// Handles execute command request.
pub fn handle_execute_command(
    params: lsp_types::ExecuteCommandParams,
    _: &Snapshots,
    _: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<serde_json::Value>> {
    // Handles create project command.
    if params.command == "createProject" {
        let args = params
            .arguments
            .first()
            .and_then(serde_json::Value::as_object)
            .and_then(|arg| {
                arg.get("name").and_then(|it| it.as_str()).zip(
                    arg.get("root").and_then(|it| it.as_str()).and_then(|it| {
                        lsp_types::Url::parse(&format!(
                            "{it}{}",
                            if it.ends_with('/') { "" } else { "/" }
                        ))
                        .ok()
                    }),
                )
            });

        match args {
            Some((name, root)) => {
                let uris = root
                    .clone()
                    .join("lib.rs")
                    .ok()
                    .zip(root.join("Cargo.toml").ok());
                match uris {
                    Some((lib_uri, cargo_uri)) => match ink_analyzer::new_project(name.to_string())
                    {
                        Ok(project) => {
                            // Returns create project edits.
                            Ok(serde_json::to_value(CreateProjectResponse {
                                name: name.to_owned(),
                                uri: root,
                                files: HashMap::from([
                                    (lib_uri, project.lib.plain),
                                    (cargo_uri, project.cargo.plain),
                                ]),
                            })
                            .ok())
                        }
                        Err(_) => Err(anyhow::format_err!(
                            "Failed to create ink! project: {name}\n\
                            ink! project names must begin with an alphabetic character, \
                            and only contain alphanumeric characters, underscores and hyphens"
                        )),
                    },
                    None => Err(anyhow::format_err!("Failed to create ink! project: {name}")),
                }
            }
            // Error for missing or invalid args.
            None => Err(anyhow::format_err!(
                "The name and root arguments are required!"
            )),
        }
    } else {
        Err(anyhow::format_err!("Unknown command: {}!", params.command))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::init_snapshots;
    use test_utils::simple_client_config;

    #[test]
    fn handle_completion_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Initializes snapshots with test document.
        let (snapshots, uri) = init_snapshots(String::from("#[ink::co]"), &client_capabilities);

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

    #[test]
    fn handle_hover_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Initializes snapshots with test document.
        let (snapshots, uri) =
            init_snapshots(String::from("#[ink::contract]"), &client_capabilities);

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

    #[test]
    fn handle_code_action_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Initializes snapshots with test document.
        let (snapshots, uri) =
            init_snapshots(String::from("mod my_contract {}"), &client_capabilities);

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

    #[test]
    fn handle_inlay_hint_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Initializes snapshots with test document.
        let (snapshots, uri) = init_snapshots(
            String::from(r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#),
            &client_capabilities,
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

    #[test]
    fn handle_signature_help_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Initializes snapshots with test document.
        let (snapshots, uri) =
            init_snapshots(String::from("#[ink::contract()]"), &client_capabilities);

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
                    let end_offset =
                        test_utils::parse_offset_at(signature_label, Some(label)).unwrap() as u32;
                    let start_offset = end_offset - label.len() as u32;
                    [start_offset, end_offset]
                }
            })
            .collect();
        assert_eq!(params, vec![[0, 21], [23, 38]]);
        assert_eq!(signature_help.active_parameter.unwrap(), 0);
    }

    #[test]
    fn handle_execute_command_new_project_works() {
        // Creates test project.
        let project_name = "hello_ink";
        let project_uri = lsp_types::Url::parse("file:///tmp/hello_ink/").unwrap();

        // Calls handler and verifies that the expected response is returned.
        let result = handle_execute_command(
            lsp_types::ExecuteCommandParams {
                command: "createProject".to_string(),
                arguments: vec![serde_json::json!({
                    "name": project_name,
                    "root": project_uri
                })],
                work_done_progress_params: Default::default(),
            },
            &Snapshots::new(),
            &simple_client_config(),
        );
        assert!(result.is_ok());
        let resp: CreateProjectResponse = serde_json::from_value(result.unwrap().unwrap()).unwrap();
        // Verifies project metadata.
        assert_eq!(resp.name, project_name);
        assert_eq!(resp.uri, project_uri);
        // Verifies project files and their contents.
        let lib_uri = project_uri.clone().join("lib.rs").unwrap();
        let cargo_uri = project_uri.clone().join("Cargo.toml").unwrap();
        let lib_content = resp.files.get(&lib_uri).unwrap();
        let cargo_content = resp.files.get(&cargo_uri).unwrap();
        assert!(resp.files.contains_key(&lib_uri));
        assert!(resp.files.contains_key(&cargo_uri));
        assert!(lib_content.contains("#[ink::contract]\npub mod hello_ink {"));
        assert!(cargo_content.contains(r#"name = "hello_ink""#));
    }
}
