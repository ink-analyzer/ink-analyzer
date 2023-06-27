//! LSP request router.

use serde::{de::DeserializeOwned, Serialize};

use crate::memory::Memory;

/// A chainable type for routing LSP requests to appropriate handlers and composing responses (if appropriate).
pub struct RequestRouter<'a> {
    req: Option<lsp_server::Request>,
    resp: Option<lsp_server::Response>,
    memory: &'a mut Memory,
    client_capabilities: &'a lsp_types::ClientCapabilities,
}

impl<'a> RequestRouter<'a> {
    /// Creates a router for a request.
    pub fn new(
        req: lsp_server::Request,
        memory: &'a mut Memory,
        client_capabilities: &'a lsp_types::ClientCapabilities,
    ) -> Self {
        Self {
            req: Some(req),
            resp: None,
            memory,
            client_capabilities,
        }
    }

    /// Routes the request (if hasn't been consumed yet) through a handler based on the request method.
    pub fn process<R>(
        &mut self,
        handler: fn(
            R::Params,
            &mut Memory,
            &lsp_types::ClientCapabilities,
        ) -> anyhow::Result<R::Result>,
    ) -> &mut Self
    where
        R: lsp_types::request::Request,
        R::Params: DeserializeOwned,
        R::Result: Serialize,
    {
        // Return immediately if a response has already been determined.
        if self.resp.is_some() {
            return self;
        }

        // Unwrap request if it hasn't been consumed yet, otherwise return immediately.
        let req = match self.req.take() {
            Some(it) => it,
            None => return self,
        };

        // Clone request id so we can use it with a JSON error since it's not returned by `req.extract` for that case.
        let req_id = req.id.clone();

        // Match request to method and call handler with extracted params (if possible).
        match req.extract::<R::Params>(R::METHOD) {
            // Handles and consumes the request if it matches the method.
            Ok((id, params)) => {
                // Composes LSP response using method handler.
                let resp = match handler(params, self.memory, self.client_capabilities) {
                    Ok(resp) => lsp_server::Response::new_ok(id, resp),
                    Err(error) => lsp_server::Response::new_err(
                        id,
                        lsp_server::ErrorCode::InternalError as i32,
                        error.to_string(),
                    ),
                };

                // Sets LSP response (possibly an error).
                self.resp = Some(resp);
            }
            // Re-sets the router/request (so that another handler can be tried) if method is not a match.
            Err(lsp_server::ExtractError::MethodMismatch(req)) => {
                self.req = Some(req);
            }
            // Sets LSP error response and consumes the request if JSON is invalid.
            Err(lsp_server::ExtractError::JsonError { method, error }) => {
                self.resp = Some(lsp_server::Response::new_err(
                    req_id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    format!("Invalid request parameters\nMethod: {method}\n error: {error}"),
                ));
            }
        }

        self
    }

    /// Returns (if possible) the processed response for request (which may be an error response) or
    /// an error response for unknown or unsupported requests.
    pub fn finish(&mut self) -> Option<lsp_server::Response> {
        match self.resp.take() {
            Some(resp) => Some(resp),
            None => self.req.take().map(|req| {
                lsp_server::Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::MethodNotFound as i32,
                    "Unknown or unsupported request.".to_string(),
                )
            }),
        }
    }
}
