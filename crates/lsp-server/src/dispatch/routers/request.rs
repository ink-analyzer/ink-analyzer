//! LSP request router.

use serde::{de::DeserializeOwned, Serialize};

use crate::dispatch::Snapshots;

/// Chainable type for routing LSP requests to appropriate handlers and composing responses (if appropriate).
pub struct RequestRouter<'a> {
    req: Option<lsp_server::Request>,
    resp: Option<lsp_server::Response>,
    snapshots: &'a Snapshots,
    client_capabilities: &'a lsp_types::ClientCapabilities,
}

impl<'a> RequestRouter<'a> {
    /// Creates router for a request.
    pub fn new(
        req: lsp_server::Request,
        snapshots: &'a Snapshots,
        client_capabilities: &'a lsp_types::ClientCapabilities,
    ) -> Self {
        Self {
            req: Some(req),
            resp: None,
            snapshots,
            client_capabilities,
        }
    }

    /// Routes the request (if it hasn't been consumed yet) through a handler based on the request method.
    pub fn process<R>(
        &mut self,
        handler: fn(
            R::Params,
            &Snapshots,
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
        let Some(req) = self.req.take() else {
            return self;
        };

        // Clone request id so we can use it with a JSON error since it's not returned by `req.extract` for that case.
        let req_id = req.id.clone();

        // Match request to method and call handler with extracted params (if possible).
        match req.extract::<R::Params>(R::METHOD) {
            // Handles and consumes the request if it matches the method.
            Ok((id, params)) => {
                // Composes LSP response using method handler.
                let resp = match handler(params, self.snapshots, self.client_capabilities) {
                    Ok(resp) => lsp_server::Response::new_ok(id, resp),
                    Err(error) => lsp_server::Response::new_err(
                        id,
                        lsp_server::ErrorCode::InternalError as i32,
                        error.to_string(),
                    ),
                };

                // Sets LSP response (possibly an error response).
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

    /// Returns (if possible) the processed response for the request (which may be an error response) or
    /// an error response for unknown or unsupported requests.
    pub fn finish(&mut self) -> Option<lsp_server::Response> {
        match self.resp.take() {
            Some(resp) => Some(resp),
            None => self.req.take().map(|req| {
                lsp_server::Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::MethodNotFound as i32,
                    "Unknown or unsupported request.".to_owned(),
                )
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dispatch::handlers;
    use crate::test_utils::init_snapshots;
    use test_utils::simple_client_config;

    #[test]
    fn request_router_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Initializes snapshots with test document.
        let (snapshots, uri) = init_snapshots(String::from(""), &client_capabilities);

        // Creates LSP completion request.
        use lsp_types::request::Request;
        let req_id = lsp_server::RequestId::from(1);
        let req = lsp_server::Request {
            id: req_id.clone(),
            method: lsp_types::request::Completion::METHOD.to_owned(),
            params: serde_json::to_value(lsp_types::CompletionParams {
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

        // Processes completion request through a request router with a completion handler,
        // retrieves response and verifies that it's a success response.
        let mut router = RequestRouter::new(req.clone(), &snapshots, &client_capabilities);
        let result = router
            .process::<lsp_types::request::HoverRequest>(handlers::request::handle_hover)
            .process::<lsp_types::request::Completion>(handlers::request::handle_completion)
            .process::<lsp_types::request::CodeActionRequest>(handlers::request::handle_code_action)
            .finish();
        assert!(result.is_some());
        let response = result.unwrap();
        assert_eq!(response.id, req_id);
        assert!(response.result.is_some());
        assert!(response.error.is_none());

        // Processes completion request through a request router with NO completion handler,
        // retrieves response and verifies that it's an "unknown or unsupported request" error response.
        let mut router = RequestRouter::new(req.clone(), &snapshots, &client_capabilities);
        let result = router
            .process::<lsp_types::request::HoverRequest>(handlers::request::handle_hover)
            .process::<lsp_types::request::CodeActionRequest>(handlers::request::handle_code_action)
            .finish();
        assert!(result.is_some());
        let response = result.unwrap();
        assert_eq!(response.id, req_id);
        assert!(response.result.is_none());
        assert!(response.error.is_some());
        let message = response.error.as_ref().unwrap().message.to_lowercase();
        assert!(message.contains("unknown") || message.contains("unsupported"));

        // Processes modified completion request with invalid parameters through a request router with a completion handler,
        // retrieves response and verifies that it's an "invalid request" error response.
        let mut req_invalid = req;
        req_invalid.params = serde_json::Value::Null;
        let req_id_invalid = req_invalid.id.clone();
        let mut router = RequestRouter::new(req_invalid, &snapshots, &client_capabilities);
        let result = router
            .process::<lsp_types::request::HoverRequest>(handlers::request::handle_hover)
            .process::<lsp_types::request::Completion>(handlers::request::handle_completion)
            .finish();
        assert!(result.is_some());
        let response = result.unwrap();
        assert_eq!(response.id, req_id_invalid);
        assert!(response.result.is_none());
        assert!(response.error.is_some());
        let message = response.error.as_ref().unwrap().message.to_lowercase();
        assert!(message.contains("invalid") && message.contains("parameters"));
    }
}
