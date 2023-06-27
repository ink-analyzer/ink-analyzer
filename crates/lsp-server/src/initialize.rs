//! LSP Server initialization.

use lsp_types::{InitializeParams, InitializeResult};

use crate::capabilities;

/// Returns an initialization result based on the given initialization parameters.
///
/// Ref: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeResult>.
pub fn initialize(initialize_params: &InitializeParams) -> InitializeResult {
    InitializeResult {
        // Sets server capabilities based on client sent initialization parameters.
        capabilities: capabilities::server_capabilities(&initialize_params.capabilities),
        server_info: Some(lsp_types::ServerInfo {
            name: "ink-analyzer".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
        offset_encoding: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initialize_works() {
        // Creates initialization result based on initialization params.
        let initialize_result = initialize(&InitializeParams::default());

        // Verifies that an initialization result is created with the expected server name.
        // NOTE: See `capabilities` module for unit tests for the `capabilities::server_capabilities` utility.
        assert_eq!(
            initialize_result.server_info.unwrap().name,
            "ink-analyzer".to_string()
        );
    }
}
