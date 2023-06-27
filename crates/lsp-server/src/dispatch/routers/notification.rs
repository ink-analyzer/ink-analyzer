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
