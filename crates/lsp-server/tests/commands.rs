//! integration tests for ink! Language Server commands.

mod utils;

use std::path::Path;
use std::str::FromStr;

// Tests the create new project command.
// The high-level methodology for the "creatProject" command test is:
// - Send an LSP execute command request for "createProject" from client to server including a project name and uri.
// - Retrieve the LSP execute command response sent from the server to the client
//   (which should be an empty result that simply signifies that the server received the request).
// - Retrieve the LSP workspace/applyEdit request sent from the server to the client which is triggered by the "createProject" command.
// - Verify that the workspace edit creates the expected files and contents.
// See inline comments for more details.
#[test]
fn create_project_command_works() {
    // Creates an in-memory connection to an initialized LSP server.
    let client_connection = utils::create_initialized_lsp_server();

    // Creates test project.
    let project_name = "hello_ink";
    let project_uri = lsp_types::Uri::from_str("file:///tmp/hello_ink/").unwrap();

    // Creates LSP execute command request.
    use lsp_types::request::Request;
    let req_id = lsp_server::RequestId::from("create-project::hello_ink".to_owned());
    let req = lsp_server::Request {
        id: req_id.clone(),
        method: lsp_types::request::ExecuteCommand::METHOD.to_owned(),
        params: serde_json::to_value(lsp_types::ExecuteCommandParams {
            command: "createProject".to_owned(),
            arguments: vec![serde_json::json!({
                "name": project_name,
                "root": project_uri
            })],
            work_done_progress_params: Default::default(),
        })
        .unwrap(),
    };
    // Sends LSP execute command request from client to server.
    client_connection.sender.send(req.into()).unwrap();

    // Retrieves the LSP execute command response (from the server) on the client.
    let resp = client_connection
        .receiver
        .iter()
        .find_map(|message| match message {
            lsp_server::Message::Response(it) => Some(it),
            _ => None,
        })
        .unwrap();
    // Verifies that the execute command response is for the current request
    // and includes the expected results (i.e. successful but with an empty result).
    assert_eq!(resp.id, req_id);
    assert_eq!(resp.result, Some(serde_json::Value::Null));

    // Retrieves the LSP `workspace/applyEdit` request (from the server) on the client.
    let workspace_edit_req = client_connection
        .receiver
        .iter()
        .find_map(|message| match message {
            lsp_server::Message::Request(it) => {
                (it.method == lsp_types::request::ApplyWorkspaceEdit::METHOD).then_some(it)
            }
            _ => None,
        })
        .unwrap();
    // Verifies the expected workspace edit/ document changes.
    let params: lsp_types::ApplyWorkspaceEditParams =
        serde_json::from_value(workspace_edit_req.params).unwrap();
    assert_eq!(params.label.as_deref(), Some("New ink! project"));
    let document_changes = match params.edit.document_changes.unwrap() {
        lsp_types::DocumentChanges::Edits(_) => None,
        lsp_types::DocumentChanges::Operations(it) => Some(it),
    }
    .unwrap();
    // Verifies that `lib.rs` and `Cargo.toml` files are created and contain expected content.
    let lib_uri = lsp_types::Uri::from_str(
        Path::new(project_uri.as_str())
            .join("lib.rs")
            .to_str()
            .unwrap(),
    )
    .unwrap();
    let cargo_uri = lsp_types::Uri::from_str(
        Path::new(project_uri.as_str())
            .join("Cargo.toml")
            .to_str()
            .unwrap(),
    )
    .unwrap();
    let contains_file_create = |uri: &lsp_types::Uri| {
        document_changes.iter().any(|change| match change {
            lsp_types::DocumentChangeOperation::Op(lsp_types::ResourceOp::Create(it)) => {
                &it.uri == uri
            }
            _ => false,
        })
    };
    let contains_file_content = |uri: &lsp_types::Uri, pat: &str| {
        document_changes.iter().any(|change| match change {
            lsp_types::DocumentChangeOperation::Edit(it) => {
                let edit_text = match &it.edits[0] {
                    lsp_types::OneOf::Left(it) => &it.new_text,
                    lsp_types::OneOf::Right(it) => &it.text_edit.new_text,
                };
                &it.text_document.uri == uri && edit_text.contains(pat)
            }
            _ => false,
        })
    };
    assert!(contains_file_create(&lib_uri));
    assert!(contains_file_create(&cargo_uri));
    assert!(contains_file_content(
        &lib_uri,
        "#[ink::contract]\npub mod hello_ink {"
    ));
    assert!(contains_file_content(&cargo_uri, r#"name = "hello_ink""#));
}
