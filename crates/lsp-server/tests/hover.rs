//! integration tests for ink! Language Server hover content.

use line_index::LineIndex;
use test_utils::{TestCaseParams, TestCaseResults};

mod utils;

// The high-level methodology for hover content test cases is:
// - Read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test_data/contracts/erc20.rs).
// - (Optionally) Make some modifications to the source code at a specific offset/text range to create a specific test case.
// - Send the the modified source code from client to server via a `DidOpenTextDocument` or `DidChangeTextDocument` LSP notification.
// - Send an LSP hover request from client to server for the test case.
// - Retrieve the LSP hover response sent from the server to the client.
// - Verify that the hover response results match the expected results.
// See inline comments for mode details.
#[test]
fn hover_works() {
    // Creates an in-memory connection to an initialized LSP server.
    let client_connection = utils::create_initialized_lsp_server();

    // Iterates over all test case groups (see [`test_utils::fixtures::hover_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::hover_fixtures() {
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

            // Sets the focus range.
            let (range_start_pat, range_end_pat) = match test_case.params.unwrap() {
                TestCaseParams::Hover(it) => Some((it.range_start_pat, it.range_end_pat)),
                _ => None,
            }
            .unwrap();
            let text_range = ink_analyzer::TextRange::new(
                ink_analyzer::TextSize::from(
                    test_utils::parse_offset_at(&test_code, range_start_pat).unwrap() as u32,
                ),
                ink_analyzer::TextSize::from(
                    test_utils::parse_offset_at(&test_code, range_end_pat).unwrap() as u32,
                ),
            );
            // Translates the text range to an LSP range.
            let translation_context = ink_lsp_server::translator::PositionTranslationContext {
                encoding: lsp_types::PositionEncodingKind::UTF8,
                line_index: LineIndex::new(&test_code),
            };
            let range = ink_lsp_server::translator::to_lsp::range(text_range, &translation_context)
                .unwrap();

            // Sends a `DidOpenTextDocument` or `DidChangeTextDocument` notification (depending on the current value of `version`) from client to server .
            test_utils::versioned_document_sync_notification(
                uri.clone(),
                test_code.clone(),
                version,
                &client_connection.sender,
            );

            // Creates LSP hover request.
            use lsp_types::request::Request;
            let req_id = lsp_server::RequestId::from(idx as i32);
            let req = lsp_server::Request {
                id: req_id.clone(),
                method: lsp_types::request::HoverRequest::METHOD.to_string(),
                params: serde_json::to_value(&lsp_types::HoverParams {
                    text_document_position_params: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                        position: range.start,
                    },
                    work_done_progress_params: Default::default(),
                })
                .unwrap(),
            };
            // Sends LSP hover request from client to server.
            client_connection.sender.send(req.into()).unwrap();

            // Retrieves the LSP hover response (from the server) on the client.
            let resp = client_connection
                .receiver
                .iter()
                .find_map(|message| match message {
                    lsp_server::Message::Response(it) => Some(it),
                    _ => None,
                })
                .unwrap();
            // Verifies that the hover response are for the current request.
            assert_eq!(&resp.id, &req_id);

            // Verifies expected hover content results.
            let expected_results = match test_case.results {
                TestCaseResults::Hover(it) => Some(it),
                _ => None,
            }
            .unwrap();
            if expected_results.is_none() {
                // Response result should be null if no hover content is expected.
                assert_eq!(resp.result.unwrap(), serde_json::Value::Null);
            } else {
                // Otherwise serialize response result and verify the expected results.
                let hover_response: lsp_types::Hover =
                    serde_json::from_value(resp.result.unwrap()).unwrap();
                let hover_content = match hover_response.contents {
                    lsp_types::HoverContents::Scalar(it) => match it {
                        lsp_types::MarkedString::String(it) => Some(it),
                        lsp_types::MarkedString::LanguageString(it) => Some(it.value),
                    },
                    lsp_types::HoverContents::Markup(it) => Some(it.value),
                    lsp_types::HoverContents::Array(_) => None,
                }
                .unwrap();
                assert!(
                    hover_content.contains(expected_results.as_ref().unwrap().text),
                    "source: {}",
                    test_group.source
                );
                assert_eq!(
                    hover_response.range.unwrap(),
                    ink_lsp_server::translator::to_lsp::range(
                        ink_analyzer::TextRange::new(
                            ink_analyzer::TextSize::from(
                                test_utils::parse_offset_at(
                                    &test_code,
                                    expected_results.as_ref().unwrap().start_pat
                                )
                                .unwrap() as u32
                            ),
                            ink_analyzer::TextSize::from(
                                test_utils::parse_offset_at(
                                    &test_code,
                                    expected_results.as_ref().unwrap().end_pat
                                )
                                .unwrap() as u32
                            ),
                        ),
                        &translation_context
                    )
                    .unwrap(),
                    "source: {}",
                    test_group.source
                );
            }
        }
    }
}
