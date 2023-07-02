//! LSP notification router.

use serde::de::DeserializeOwned;

use crate::memory::Memory;

/// A chainable type for routing LSP notifications to appropriate handlers.
pub struct NotificationRouter<'a> {
    not: Option<lsp_server::Notification>,
    memory: &'a mut Memory,
}

impl<'a> NotificationRouter<'a> {
    /// Creates a router for a notification.
    pub fn new(req: lsp_server::Notification, memory: &'a mut Memory) -> Self {
        Self {
            not: Some(req),
            memory,
        }
    }

    /// Routes the notification (if hasn't been consumed yet) through a handler based on the notification method.
    pub fn process<N>(
        &mut self,
        handler: fn(N::Params, &mut Memory) -> anyhow::Result<()>,
    ) -> anyhow::Result<&mut Self>
    where
        N: lsp_types::notification::Notification,
        N::Params: DeserializeOwned,
    {
        // Unwrap notification if it hasn't been consumed yet, otherwise return immediately.
        let not = match self.not.take() {
            Some(it) => it,
            None => return Ok(self),
        };

        // Match notification to method and call handler with extracted params (if possible).
        match not.extract::<N::Params>(N::METHOD) {
            // Handles and consumes the notification if it matches the method.
            Ok(params) => {
                // Composes LSP response using method handler.
                handler(params, self.memory)?;
                Ok(self)
            }
            // Re-sets the router/notification (so that another handler can be tried) if method is not a match.
            Err(lsp_server::ExtractError::MethodMismatch(not)) => {
                self.not = Some(not);
                Ok(self)
            }
            // Returns an error result and consumes the notification if JSON is invalid.
            Err(lsp_server::ExtractError::JsonError { method, error }) => Err(anyhow::format_err!(
                "Invalid notification parameters\nMethod: {method}\n error: {error}"
            )),
        }
    }

    /// Returns true if the notification was processed through a handler, or false otherwise.
    pub fn finish(&mut self) -> bool {
        self.not.is_none()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dispatch::handlers;
    use crate::test_utils::document_uri;

    #[test]
    fn request_router_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = document_uri();

        // Creates `DidOpenTextDocument` notification.
        use lsp_types::notification::Notification;
        let notification_method = lsp_types::notification::DidOpenTextDocument::METHOD;
        let not = lsp_server::Notification {
            method: notification_method.to_string(),
            params: serde_json::to_value(lsp_types::DidOpenTextDocumentParams {
                text_document: lsp_types::TextDocumentItem {
                    uri,
                    language_id: "rust".to_string(),
                    version: 0,
                    text: "".to_string(),
                },
            })
            .unwrap(),
        };

        // Processes `DidOpenTextDocument` notification through a notification router with a `DidOpenTextDocument` notification handler
        // and verifies that the notification is processed successfully
        // (i.e all `router.process` calls return `Ok` results and `router.finish` returns true).
        let mut router = NotificationRouter::new(not.clone(), &mut memory);
        let result: anyhow::Result<bool> = (|| {
            Ok(router
                .process::<lsp_types::notification::DidChangeTextDocument>(
                    handlers::notification::handle_did_change_text_document,
                )?
                .process::<lsp_types::notification::DidOpenTextDocument>(
                    handlers::notification::handle_did_open_text_document,
                )?
                .process::<lsp_types::notification::DidCloseTextDocument>(
                    handlers::notification::handle_did_close_text_document,
                )?
                .finish())
        })();
        // No errors (`Ok` results from all `router.process` calls).
        assert!(result.is_ok());
        // `router.finish` returns true.
        assert!(result.unwrap());

        // Processes `DidOpenTextDocument` notification through a notification router with NO `DidOpenTextDocument` notification handler
        // and verifies that the notification is not processed but is error free
        // (i.e all `router.process` calls return `Ok` results and `router.finish` returns false).
        let mut router = NotificationRouter::new(not.clone(), &mut memory);
        let result: anyhow::Result<bool> = (|| {
            Ok(router
                .process::<lsp_types::notification::DidChangeTextDocument>(
                    handlers::notification::handle_did_change_text_document,
                )?
                .process::<lsp_types::notification::DidCloseTextDocument>(
                    handlers::notification::handle_did_close_text_document,
                )?
                .finish())
        })();
        // No errors (`Ok` results from all `router.process` calls).
        assert!(result.is_ok());
        // `router.finish` returns false.
        assert!(!result.unwrap());

        // Processes `DidOpenTextDocument` notification through a notification router with a `DidOpenTextDocument` notification handler
        // and verifies that the notification is not processed due to an error
        // (i.e the `DidOpenTextDocument` notification handler returns an `Err` result and `router.finish` is never reached).
        let mut notification_invalid = not;
        notification_invalid.params = serde_json::Value::Null;
        let mut router = NotificationRouter::new(notification_invalid, &mut memory);
        let result: anyhow::Result<bool> = (|| {
            Ok(router
                .process::<lsp_types::notification::DidChangeTextDocument>(
                    handlers::notification::handle_did_change_text_document,
                )?
                .process::<lsp_types::notification::DidOpenTextDocument>(
                    handlers::notification::handle_did_open_text_document,
                )?
                .finish())
        })();
        // An error is returned (`Err` result from one of the `router.process` calls).
        assert!(result.is_err());
        let error_message = result.unwrap_err().to_string();
        assert!(error_message.contains("invalid") && error_message.contains("parameters"));
        // The error is returned by the `DidOpenTextDocument` notification handler.
        assert!(error_message.contains(notification_method));
    }
}
