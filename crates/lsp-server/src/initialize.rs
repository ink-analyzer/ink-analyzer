//! LSP Server [initialization](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize) implementation.

use crate::utils;

/// Implements LSP server initialization.
///
/// Ref: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize>.
pub fn initialize(
    connection: lsp_server::Connection,
) -> anyhow::Result<(lsp_server::Connection, lsp_types::InitializeParams)> {
    // Starts initialization (blocks and waits for initialize request from the client).
    let (initialize_id, initialize_params_json) = connection.initialize_start()?;
    let initialize_params: lsp_types::InitializeParams =
        serde_json::from_value(initialize_params_json).map_err(|error| {
            anyhow::format_err!("Failed to deserialize initialize parameters: {error}")
        })?;

    // Composes initialization result.
    let initialize_result = serde_json::to_value(lsp_types::InitializeResult {
        // Sets server capabilities based on client's capabilities.
        capabilities: server_capabilities(&initialize_params.capabilities),
        server_info: Some(lsp_types::ServerInfo {
            name: "ink-analyzer".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
        offset_encoding: None,
    })
    .map_err(|error| anyhow::format_err!("Failed to serialize initialize result: {error}"))?;

    // Finishes initialization (sends `InitializeResult` back to the client).
    connection.initialize_finish(initialize_id, initialize_result)?;

    Ok((connection, initialize_params))
}

/// Returns the capabilities of the language server based on the given client capabilities.
///
/// Ref: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities>.
pub fn server_capabilities(
    client_capabilities: &lsp_types::ClientCapabilities,
) -> lsp_types::ServerCapabilities {
    lsp_types::ServerCapabilities {
        position_encoding: Some(utils::position_encoding(client_capabilities)),
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            lsp_types::TextDocumentSyncOptions {
                open_close: Some(true),
                // ink! projects are currently single file and tend to be pretty small,
                // so full document sync is fine (for now).
                change: Some(lsp_types::TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(lsp_types::SaveOptions::default().into()),
            },
        )),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        completion_provider: Some(lsp_types::CompletionOptions {
            resolve_provider: None,
            // ink! completions are all attribute based.
            trigger_characters: Some(vec![
                "[".to_string(),
                "(".to_string(),
                ",".to_string(),
                ":".to_string(),
            ]),
            all_commit_characters: None,
            work_done_progress_options: Default::default(),
            completion_item: Default::default(),
        }),
        code_action_provider: Some(utils::code_actions_kinds(client_capabilities).map_or(
            lsp_types::CodeActionProviderCapability::Simple(true),
            |code_action_kinds| {
                lsp_types::CodeActionProviderCapability::Options(lsp_types::CodeActionOptions {
                    code_action_kinds: Some(code_action_kinds),
                    work_done_progress_options: Default::default(),
                    resolve_provider: None,
                })
            },
        )),
        inlay_hint_provider: Some(lsp_types::OneOf::Right(
            lsp_types::InlayHintServerCapabilities::Options(lsp_types::InlayHintOptions {
                work_done_progress_options: Default::default(),
                resolve_provider: None,
            }),
        )),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn initialize_works() {
        // Creates pair of in-memory connections to simulate an LSP client and server.
        let (server_connection, client_connection) = lsp_server::Connection::memory();

        // Starts server initialization on a separate thread (because `initialize` function is blocking).
        thread::spawn(|| initialize(server_connection));

        // Verifies that an initialization request (from client to server) gets an `InitializeResult` response (from server to client).
        // Creates initialization request.
        use lsp_types::request::Request;
        let init_req_id = lsp_server::RequestId::from(1);
        let init_req = lsp_server::Request {
            id: init_req_id.clone(),
            method: lsp_types::request::Initialize::METHOD.to_string(),
            params: serde_json::to_value(lsp_types::InitializeParams::default()).unwrap(),
        };
        // Sends initialization request from client to server.
        client_connection.sender.send(init_req.into()).unwrap();
        // Confirms receipt of `InitializeResult` response by the client.
        let message = client_connection.receiver.recv().unwrap();
        let init_result_resp = match message {
            lsp_server::Message::Response(it) => Some(it),
            _ => None,
        }
        .unwrap();
        assert_eq!(&init_result_resp.id, &init_req_id);
        // Verifies that an initialization result is created with the expected server name.
        let init_result: lsp_types::InitializeResult =
            serde_json::from_value(init_result_resp.result.unwrap()).unwrap();
        assert_eq!(&init_result.server_info.unwrap().name, "ink-analyzer");
    }

    #[test]
    fn server_capabilities_works() {
        // Creates server capabilities based on client capabilities.
        let server_capabilities = server_capabilities(&Default::default());

        // Verifies the expected default server capabilities.
        // NOTE: See `translator` module for unit tests for the `utils::position_encoding` and `utils::code_actions_kinds` utilities
        // which are used to generate the `position_encoding` and `code_action_provider` server capabilities,
        // and are currently the only server capabilities fields that aren't statically defined (i.e change based on the client capabilities).
        assert_eq!(
            server_capabilities.position_encoding,
            Some(lsp_types::PositionEncodingKind::UTF16)
        );
        assert_eq!(
            server_capabilities.code_action_provider,
            Some(lsp_types::CodeActionProviderCapability::Simple(true))
        );
    }
}
