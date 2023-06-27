//! LSP Server capabilities.

use lsp_types::{
    ClientCapabilities, CodeActionOptions, CodeActionProviderCapability, CompletionOptions,
    HoverProviderCapability, SaveOptions, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions,
};

use crate::translator;

/// Returns the capabilities of the language server based on the given client capabilities.
///
/// Ref: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities>.
pub fn server_capabilities(client_capabilities: &ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(translator::position_encoding(client_capabilities)),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                // ink! projects are currently single file and tend to be pretty small,
                // so full document sync is fine (for now).
                change: Some(TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(SaveOptions::default().into()),
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
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
        code_action_provider: Some(translator::code_actions_kinds(client_capabilities).map_or(
            CodeActionProviderCapability::Simple(true),
            |code_action_kinds| {
                CodeActionProviderCapability::Options(CodeActionOptions {
                    code_action_kinds: Some(code_action_kinds),
                    work_done_progress_options: Default::default(),
                    resolve_provider: None,
                })
            },
        )),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::PositionEncodingKind;

    #[test]
    fn server_capabilities_works() {
        // Creates server capabilities based on client capabilities.
        let server_capabilities = server_capabilities(&Default::default());

        // Verifies the expected default server capabilities.
        // NOTE: See `translator` module for unit tests for the `translator::position_encoding` and `translator::code_actions_kinds` utilities
        // which are used to generate the `position_encoding` and `code_action_provider` server capabilities,
        // which are currently the only server capabilities that aren't statically defined (i.e change based on the client capabilities).
        assert_eq!(
            server_capabilities.position_encoding,
            Some(PositionEncodingKind::UTF16)
        );
        assert_eq!(
            server_capabilities.code_action_provider,
            Some(CodeActionProviderCapability::Simple(true))
        );
    }
}
