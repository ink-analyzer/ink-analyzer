//! integration tests for ink! Language Server diagnostics.

use test_utils::TestCaseResults;

mod utils;

// The high-level methodology for diagnostics test cases is:
// - Read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test_data/contracts/erc20.rs).
// - (Optionally) Make modifications to the source code at specific offsets/text ranges to create a specific test case.
// - Send the the modified source code from client to server via a `DidOpenTextDocument` or `DidChangeTextDocument` LSP notification.
// - Retrieve the `PublishDiagnostics` notification that's sent from the server to the client when the server receives a `DidOpenTextDocument` or `DidChangeTextDocument` LSP notification.
// - Verify that the diagnostics in the `PublishDiagnostics` notification match the expected results.
// See inline comments for more details.
#[test]
fn diagnostics_works() {
    // Creates an in-memory connection to an initialized LSP server.
    let client_connection = utils::create_initialized_lsp_server();

    // Iterates over all test case groups (see [`test_utils::fixtures::diagnostics_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::diagnostics_fixtures() {
        // Gets the original source code.
        let original_code = test_utils::get_source_code(test_group.source);

        // Creates an LSP URI for the source file.
        let uri = test_utils::get_source_uri(test_group.source);

        // Iterates over all test cases.
        for (idx, test_case) in test_group.test_cases.into_iter().enumerate() {
            // Creates a copy of test code for this test case.
            let mut test_code = original_code.clone();

            // Applies test case modifications (if any).
            if let Some(modifications) = test_case.modifications {
                test_utils::apply_test_modifications(&mut test_code, &modifications);
            }

            // Sets the LSP document version as the index of the test case.
            let version = idx as i32;

            // Sends a `DidOpenTextDocument` or `DidChangeTextDocument` notification (depending on the current value of `version`) from client to server .
            test_utils::versioned_document_sync_notification(
                uri.clone(),
                test_code,
                version,
                &client_connection.sender,
            );

            // Retrieves the `PublishDiagnostics` notification (from the server) on the client.
            let message = client_connection.receiver.recv().unwrap();
            let diagnostics_notification = match message {
                lsp_server::Message::Notification(it) => Some(it),
                _ => None,
            }
            .unwrap();
            use lsp_types::notification::Notification;
            assert_eq!(
                &diagnostics_notification.method,
                lsp_types::notification::PublishDiagnostics::METHOD
            );
            // Verifies that the diagnostics response is for the current source file and modified version.
            let diagnostics_params: lsp_types::PublishDiagnosticsParams =
                serde_json::from_value(diagnostics_notification.params).unwrap();
            assert_eq!(&diagnostics_params.uri, &uri);
            assert_eq!(diagnostics_params.version.unwrap(), version);
            // Verifies expected diagnostics results.
            let results = diagnostics_params.diagnostics;
            let expected_results = match test_case.results {
                TestCaseResults::Diagnostic(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(results.len(), expected_results);
        }
    }
}
