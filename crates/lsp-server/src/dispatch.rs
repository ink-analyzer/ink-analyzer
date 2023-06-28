//! LSP server main loop for dispatching requests, notifications and handling responses.

use crossbeam_channel::Sender;
use ink_analyzer::Analysis;
use line_index::LineIndex;

use crate::dispatch::routers::{NotificationRouter, RequestRouter};
use crate::memory::Memory;
use crate::translator;
use crate::translator::PositionTranslationContext;

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
        // Compute and publish diagnostics if memory has been updated.
        if self.memory.has_changes() {
            self.publish_diagnostics()?;
        }

        Ok(())
    }

    /// Sends diagnostics notifications to the client for changed (including new) documents.
    fn publish_diagnostics(&mut self) -> anyhow::Result<()> {
        // Iterates over all documents with changes.
        for id in self.memory.take_changes() {
            // Only continue if the document id is a valid LSP URI.
            if let Ok(uri) = lsp_types::Url::parse(&id) {
                let (diagnostics, version, line_index) = match self.memory.get(&id) {
                    // Compute diagnostics for document.
                    Some(doc) => (
                        Analysis::new(&doc.content).diagnostics(),
                        Some(doc.version),
                        Some(LineIndex::new(&doc.content)),
                    ),
                    // Clear diagnostics for missing documents.
                    None => (Vec::new(), None, None),
                };

                // Composes translation context.
                let translation_context = line_index.map(|line_index| PositionTranslationContext {
                    encoding: translator::position_encoding(&self.client_capabilities),
                    line_index,
                });

                // Composes and send `PublishDiagnostics` notification.
                use lsp_types::notification::Notification;
                let notification = lsp_server::Notification::new(
                    lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
                    lsp_types::PublishDiagnosticsParams {
                        uri,
                        diagnostics: diagnostics
                            .into_iter()
                            .filter_map(|diagnostic| {
                                translation_context.as_ref().and_then(|context| {
                                    translator::to_lsp::diagnostic(diagnostic, context)
                                })
                            })
                            .collect(),
                        version,
                    },
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
