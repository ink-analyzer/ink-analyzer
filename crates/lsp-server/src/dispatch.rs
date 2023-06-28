//! LSP server main loop for dispatching requests, notifications and handling responses.

use crossbeam_channel::Sender;
use std::collections::HashSet;

use crate::dispatch::routers::{NotificationRouter, RequestRouter};
use crate::memory::Memory;

mod actions;
mod handlers;
mod routers;

/// Implements the main loop for dispatching LSP requests, notifications and handling responses.
pub fn main_loop(
    connection: lsp_server::Connection,
    client_capabilities: lsp_types::ClientCapabilities,
) -> anyhow::Result<()> {
    // Creates a dispatcher.
    let mut dispatcher = Dispatcher::new(&connection.sender, client_capabilities);

    // Iterates over a crossbeam channel receiver for LSP messages (blocks until next message is received).
    // Ref: <https://docs.rs/crossbeam-channel/0.5.8/crossbeam_channel/#iteration>.
    for msg in &connection.receiver {
        match msg {
            lsp_server::Message::Request(req) => {
                // Handles shutdown request.
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                // Handles all other requests using the dispatcher.
                dispatcher.handle_request(req)?;
            }
            lsp_server::Message::Notification(not) => {
                // Handles exit notification in case it comes out of band of a shutdown request.
                use lsp_types::notification::Notification;
                if not.method == lsp_types::notification::Exit::METHOD {
                    return Ok(());
                }

                // Handles all other notifications using the dispatcher.
                dispatcher.handle_notification(not)?;
            }
            // We don't currently initiate any requests, so all responses are ignored.
            lsp_server::Message::Response(_) => (),
        }
    }

    Ok(())
}

/// A stateful type for dispatching LSP requests and notifications
struct Dispatcher<'a> {
    sender: &'a Sender<lsp_server::Message>,
    client_capabilities: lsp_types::ClientCapabilities,
    memory: Memory,
}

impl<'a> Dispatcher<'a> {
    /// Creates a dispatcher for an LSP server connection.
    fn new(
        sender: &'a Sender<lsp_server::Message>,
        client_capabilities: lsp_types::ClientCapabilities,
    ) -> Self {
        Self {
            sender,
            client_capabilities,
            memory: Memory::new(),
        }
    }

    /// Handles LSP requests and sends responses (if any) as appropriate.
    fn handle_request(&mut self, req: lsp_server::Request) -> anyhow::Result<()> {
        // Computes request response (if any).
        let mut router = RequestRouter::new(req, &mut self.memory, &self.client_capabilities);
        let result = router
            .process::<lsp_types::request::Completion>(handlers::request::handle_completion)
            .process::<lsp_types::request::HoverRequest>(handlers::request::handle_hover)
            .process::<lsp_types::request::CodeActionRequest>(handlers::request::handle_code_action)
            .finish();

        // Sends response (if any).
        if let Some(resp) = result {
            self.send(resp.into())?;
        }

        // Process memory changes (if any) made by request handlers.
        self.process_changes()?;

        Ok(())
    }

    /// Handles LSP notifications and processes resulting changes to state (if any) as appropriate.
    pub fn handle_notification(&mut self, not: lsp_server::Notification) -> anyhow::Result<()> {
        // Routes notification to appropriate handler (if any).
        let mut router = NotificationRouter::new(not, &mut self.memory);
        router
            .process::<lsp_types::notification::DidOpenTextDocument>(
                handlers::notification::handle_did_open_text_document,
            )?
            .process::<lsp_types::notification::DidChangeTextDocument>(
                handlers::notification::handle_did_change_text_document,
            )?
            .process::<lsp_types::notification::DidCloseTextDocument>(
                handlers::notification::handle_did_close_text_document,
            )?
            .finish();

        // Process memory changes (if any) made by notification handlers.
        self.process_changes()?;

        Ok(())
    }

    /// Processes changes to state and triggers appropriate actions (if any).
    fn process_changes(&mut self) -> anyhow::Result<()> {
        // Retrieves document changes (if any).
        if let Some(changes) = self.memory.take_changes() {
            // Converts doc ids to LSP URIs.
            let changes = changes
                .iter()
                .filter_map(|id| lsp_types::Url::parse(id).ok())
                .collect();

            // publish diagnostics.
            self.publish_diagnostics(&changes)?;
        }

        Ok(())
    }

    /// Sends diagnostics notifications to the client for changed (including new) documents.
    fn publish_diagnostics(&mut self, changes: &HashSet<lsp_types::Url>) -> anyhow::Result<()> {
        // Composes `PublishDiagnostics` notification parameters for documents with changes.
        if let Some(params_list) =
            actions::publish_diagnostics(changes, &mut self.memory, &self.client_capabilities)?
        {
            // Composes and sends `PublishDiagnostics` notifications fir all documents with changes.
            for params in params_list {
                use lsp_types::notification::Notification;
                let notification = lsp_server::Notification::new(
                    lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
                    params,
                );
                self.send(notification.into())?;
            }
        }

        Ok(())
    }

    /// Sends an LSP message to the client.
    fn send(&self, msg: lsp_server::Message) -> anyhow::Result<()> {
        self.sender
            .send(msg)
            .map_err(|error| anyhow::format_err!("Failed to send message: {error}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{document_uri, simple_client_config};
    use std::thread;

    #[test]
    fn main_loop_and_dispatcher_works() {
        // Creates pair of in-memory connections to simulate an LSP client and server.
        let (server_connection, client_connection) = lsp_server::Connection::memory();

        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Runs the message dispatch loop on a separate thread (because `main_loop` function is blocking).
        thread::spawn(|| main_loop(server_connection, client_capabilities));

        // Creates test document URI.
        let uri = document_uri();

        // Verifies that document synchronization notifications (from client to server) trigger `PublishDiagnostics` notifications (from server to client).
        // Creates `DidOpenTextDocument` notification.
        use lsp_types::notification::Notification;
        let open_document_notification = lsp_server::Notification {
            method: lsp_types::notification::DidOpenTextDocument::METHOD.to_string(),
            params: serde_json::to_value(&lsp_types::DidOpenTextDocumentParams {
                text_document: lsp_types::TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "rust".to_string(),
                    version: 0,
                    text: "".to_string(),
                },
            })
            .unwrap(),
        };
        // Sends `DidOpenTextDocument` notification from client to server.
        client_connection
            .sender
            .send(open_document_notification.into())
            .unwrap();
        // Confirms receipt of `PublishDiagnostics` notification by the client.
        let message = client_connection.receiver.recv().unwrap();
        let publish_diagnostics_notification = match message {
            lsp_server::Message::Notification(it) => Some(it),
            _ => None,
        }
        .unwrap();
        assert_eq!(
            &publish_diagnostics_notification.method,
            lsp_types::notification::PublishDiagnostics::METHOD
        );

        // Verifies that LSP requests (from client to server) get appropriate LSP responses (from server to client).
        // Creates LSP completion request.
        use lsp_types::request::Request;
        let completion_request_id = lsp_server::RequestId::from(1);
        let completion_request = lsp_server::Request {
            id: completion_request_id.clone(),
            method: lsp_types::request::Completion::METHOD.to_string(),
            params: serde_json::to_value(&lsp_types::CompletionParams {
                text_document_position: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    position: Default::default(),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            })
            .unwrap(),
        };
        // Sends LSP completion request from client to server.
        client_connection
            .sender
            .send(completion_request.into())
            .unwrap();
        // Confirms receipt of LSP completion response by the client.
        let message = client_connection.receiver.recv().unwrap();
        let completion_response = match message {
            lsp_server::Message::Response(it) => Some(it),
            _ => None,
        }
        .unwrap();
        assert_eq!(&completion_response.id, &completion_request_id);
    }
}
