//! integration tests for ink! Language Server completions.

mod utils;

use line_index::LineIndex;
use test_utils::{TestCaseParams, TestCaseResults};

// The high-level methodology for completions test cases is:
// - Read the source code of an ink! entity file in the `test-fixtures` directory
//   (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test-fixtures/contracts/erc20.rs).
// - (Optionally) Make some modifications to the source code at a specific offset/text range to create a specific test case.
// - Send the modified source code from client to server via a `DidOpenTextDocument` or `DidChangeTextDocument` LSP notification.
// - Send an LSP completion request from client to server for the test case.
// - Retrieve the LSP completion response sent from the server to the client.
// - Verify that the completion response results match the expected results.
// See inline comments for more details.
#[test]
fn completions_works() {
    // Creates an in-memory connection to an initialized LSP server.
    let client_connection = utils::create_initialized_lsp_server();

    // Iterates over all test case groups (see [`test_utils::fixtures::completions_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::completions_fixtures() {
        // Gets the original source code.
        let original_code = test_utils::read_source_code(test_group.source);

        // Creates an LSP URI for the source file.
        let uri = test_utils::source_uri(test_group.source);

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

            // Sets the cursor position.
            let offset_pat = match test_case.params.unwrap() {
                TestCaseParams::Completion(it) => Some(it),
                _ => None,
            }
            .unwrap()
            .pat;
            let offset = ink_analyzer::TextSize::from(
                test_utils::parse_offset_at(&test_code, offset_pat).unwrap() as u32,
            );
            // Translates the offset to an LSP position.
            let translation_context = ink_lsp_server::translator::PositionTranslationContext {
                encoding: lsp_types::PositionEncodingKind::UTF8,
                line_index: LineIndex::new(&test_code),
            };
            let position =
                ink_lsp_server::translator::to_lsp::position(offset, &translation_context).unwrap();

            // Sends a `DidOpenTextDocument` or `DidChangeTextDocument` notification (depending on the current value of `version`) from client to server .
            test_utils::versioned_document_sync_notification(
                uri.clone(),
                test_code.clone(),
                version,
                &client_connection.sender,
            );

            // Creates LSP completion request.
            use lsp_types::request::Request;
            let req_id = lsp_server::RequestId::from(idx as i32);
            let req = lsp_server::Request {
                id: req_id.clone(),
                method: lsp_types::request::Completion::METHOD.to_owned(),
                params: serde_json::to_value(&lsp_types::CompletionParams {
                    text_document_position: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                        position,
                    },
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                    context: None,
                })
                .unwrap(),
            };
            // Sends LSP completion request from client to server.
            client_connection.sender.send(req.into()).unwrap();

            // Retrieves the LSP completion response (from the server) on the client.
            let resp = client_connection
                .receiver
                .iter()
                .find_map(|message| match message {
                    lsp_server::Message::Response(it) => Some(it),
                    _ => None,
                })
                .unwrap();
            // Verifies that the completion response is for the current request.
            assert_eq!(resp.id, req_id);

            // Verifies expected completion results.
            let expected_results = match test_case.results {
                TestCaseResults::Completion(it) => Some(it),
                _ => None,
            }
            .unwrap();
            if expected_results.is_empty() {
                // Response result should be null if no completions are expected.
                assert_eq!(resp.result.unwrap(), serde_json::Value::Null);
            } else {
                // Otherwise serialize response result and verify the expected results.
                let completion_response: lsp_types::CompletionResponse =
                    serde_json::from_value(resp.result.unwrap()).unwrap();
                let results = match completion_response {
                    lsp_types::CompletionResponse::List(it) => it.items,
                    lsp_types::CompletionResponse::Array(it) => it,
                };
                assert_eq!(
                    results
                        .iter()
                        .filter_map(|completion| completion
                            .text_edit
                            .as_ref()
                            .and_then(|edit| match edit {
                                lsp_types::CompletionTextEdit::Edit(it) => Some(it),
                                lsp_types::CompletionTextEdit::InsertAndReplace(_) => None,
                            })
                            .map(|edit| (
                                test_utils::remove_whitespace(edit.new_text.clone()),
                                edit.range
                            )))
                        .collect::<Vec<(String, lsp_types::Range)>>(),
                    expected_results
                        .iter()
                        .map(|result| (
                            test_utils::remove_whitespace(result.text.to_owned()),
                            ink_lsp_server::translator::to_lsp::range(
                                ink_analyzer::TextRange::new(
                                    ink_analyzer::TextSize::from(
                                        test_utils::parse_offset_at(&test_code, result.start_pat)
                                            .unwrap()
                                            as u32
                                    ),
                                    ink_analyzer::TextSize::from(
                                        test_utils::parse_offset_at(&test_code, result.end_pat)
                                            .unwrap()
                                            as u32
                                    )
                                ),
                                &translation_context
                            )
                            .unwrap()
                        ))
                        .collect::<Vec<(String, lsp_types::Range)>>(),
                    "source: {}",
                    test_group.source
                );
            }
        }
    }
}
