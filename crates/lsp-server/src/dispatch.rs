//! LSP server main loop for dispatching requests, notifications and handling responses.

mod actions;
mod handlers;
mod routers;

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use crossbeam_channel::Sender;
use ink_analyzer::{Analysis, MinorVersion, Version};
use lsp_types::request::Request;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::dispatch::handlers::command::{
    CreateProjectResponse, ExtractEventResponse, MigrateProjectResponse,
};
use crate::dispatch::routers::{NotificationRouter, RequestRouter};
use crate::memory::Memory;
use crate::translator::PositionTranslationContext;
use crate::utils;
use crate::utils::{COMMAND_CREATE_PROJECT, COMMAND_EXTRACT_EVENT, COMMAND_MIGRATE_PROJECT};

/// Implements the main loop for dispatching LSP requests, notifications and handling responses.
pub fn main_loop(
    connection: lsp_server::Connection,
    client_capabilities: lsp_types::ClientCapabilities,
) -> anyhow::Result<()> {
    // Creates a dispatcher.
    let mut dispatcher = Dispatcher::new(&connection.sender, client_capabilities);

    // Iterates over a crossbeam channel receiver for LSP messages (blocks until next message is received).
    // Ref: <https://docs.rs/crossbeam-channel/0.5.8/crossbeam_channel/#iteration>.
    for msg in &connection.receiver {
        match msg {
            lsp_server::Message::Request(req) => {
                // Handles shutdown request.
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                // Handles all other requests using the dispatcher.
                dispatcher.handle_request(req)?;
            }
            lsp_server::Message::Notification(not) => {
                // Handles exit notification in case it comes out of band of a shutdown request.
                use lsp_types::notification::Notification;
                if not.method == lsp_types::notification::Exit::METHOD {
                    return Ok(());
                }

                // Handles all other notifications using the dispatcher.
                dispatcher.handle_notification(not)?;
            }
            // Handles responses to requests initiated by the server (e.g workspace edits).
            lsp_server::Message::Response(resp) => dispatcher.handle_response(resp)?,
        }
    }

    Ok(())
}

/// A stateful type for dispatching LSP requests and notifications.
struct Dispatcher<'a> {
    sender: &'a Sender<lsp_server::Message>,
    client_capabilities: lsp_types::ClientCapabilities,
    memory: Memory,
    snapshots: Snapshots,
    version_check_fuel: u8,
}

pub type Snapshots = HashMap<String, Snapshot>;

/// An immutable analysis snapshot (and metadata).
pub struct Snapshot {
    analysis: Analysis,
    context: PositionTranslationContext,
    doc_version: Option<i32>,
    lang_version: Version,
}

impl Snapshot {
    /// Creates an Analysis snapshot.
    pub fn new(
        content: String,
        encoding: lsp_types::PositionEncodingKind,
        doc_version: Option<i32>,
        lang_version: Version,
    ) -> Self {
        Self {
            analysis: Analysis::new(&content, lang_version),
            context: PositionTranslationContext::new(&content, encoding),
            doc_version,
            lang_version,
        }
    }
}

const INITIALIZE_PROJECT_ID_PREFIX: &str = "initialize-project::";
const SHOW_DOCUMENT_ID_PREFIX: &str = "show-document::";
const MIGRATE_PROJECT_ID_PREFIX: &str = "migrate-project::";
const EXTRACT_EVENT_ID_PREFIX: &str = "extract-event::";

impl<'a> Dispatcher<'a> {
    /// Creates a dispatcher for an LSP server connection.
    fn new(
        sender: &'a Sender<lsp_server::Message>,
        client_capabilities: lsp_types::ClientCapabilities,
    ) -> Self {
        Self {
            sender,
            client_capabilities,
            memory: Memory::new(),
            snapshots: Snapshots::new(),
            version_check_fuel: 0,
        }
    }

    /// Handles LSP requests and sends responses (if any) as appropriate.
    fn handle_request(&mut self, req: lsp_server::Request) -> anyhow::Result<()> {
        // Computes request response (if any).
        let cmd = if req.method == lsp_types::request::ExecuteCommand::METHOD {
            req.params
                .as_object()
                .and_then(|params| params.get("command"))
                .and_then(serde_json::Value::as_str)
                .map(ToString::to_string)
        } else {
            None
        };
        let is_migration_resolve = req.method
            == lsp_types::request::CodeActionResolveRequest::METHOD
            && req
                .params
                .as_object()
                .and_then(|params| params.get("data"))
                .and_then(serde_json::Value::as_object)
                .and_then(|params| params.get("command"))
                .and_then(serde_json::Value::as_str)
                .is_some_and(|cmd| cmd == COMMAND_MIGRATE_PROJECT);
        let mut router = RequestRouter::new(req, &self.snapshots, &self.client_capabilities);
        let result = router
            .process::<lsp_types::request::Completion>(handlers::request::handle_completion)
            .process::<lsp_types::request::HoverRequest>(handlers::request::handle_hover)
            .process::<lsp_types::request::CodeActionRequest>(handlers::request::handle_code_action)
            .process::<lsp_types::request::CodeActionResolveRequest>(
                handlers::request::handle_code_action_resolve,
            )
            .process::<lsp_types::request::InlayHintRequest>(handlers::request::handle_inlay_hint)
            .process::<lsp_types::request::SignatureHelpRequest>(
                handlers::request::handle_signature_help,
            )
            .process::<lsp_types::request::ExecuteCommand>(
                handlers::request::handle_execute_command,
            )
            .finish();

        // Sends response (if any).
        if let Some(resp) = result {
            if let Some(ref cmd) = cmd {
                // Handles command responses.
                self.process_command_response(cmd, resp)?;
            } else {
                // Otherwise return response.
                self.send(resp.into())?;
            }
        }

        // Process memory changes made by request handlers (if any).
        self.process_changes()?;

        // Increase the "version check fuel" if we just attempted an ink! version migration.
        if is_migration_resolve
            || cmd
                .as_ref()
                .is_some_and(|cmd| cmd == COMMAND_MIGRATE_PROJECT)
        {
            self.version_check_fuel = 2;
        }

        Ok(())
    }

    /// Handles LSP notifications and processes resulting changes to state (if any) as appropriate.
    pub fn handle_notification(&mut self, not: lsp_server::Notification) -> anyhow::Result<()> {
        // Routes notification to appropriate handler (if any).
        let mut router = NotificationRouter::new(not, &mut self.memory);
        router
            .process::<lsp_types::notification::DidOpenTextDocument>(
                handlers::notification::handle_did_open_text_document,
            )?
            .process::<lsp_types::notification::DidChangeTextDocument>(
                handlers::notification::handle_did_change_text_document,
            )?
            .process::<lsp_types::notification::DidCloseTextDocument>(
                handlers::notification::handle_did_close_text_document,
            )?
            .finish();

        // Process memory changes (if any) made by notification handlers.
        self.process_changes()?;

        Ok(())
    }

    /// Handles LSP responses.
    fn handle_response(&mut self, resp: lsp_server::Response) -> anyhow::Result<()> {
        // Open `lib.rs` after project initialization.
        if let Some(resp_id) = utils::request_id_as_str(resp.id) {
            if resp_id.starts_with(INITIALIZE_PROJECT_ID_PREFIX) {
                if let Some(project_uri) = resp_id
                    .strip_prefix(INITIALIZE_PROJECT_ID_PREFIX)
                    .and_then(|suffix| lsp_types::Uri::from_str(suffix).ok())
                {
                    let lib_uri = utils::uri_to_url(&project_uri)
                        .ok()
                        .and_then(|url| url.join("lib.rs").ok())
                        .map(|url| utils::url_to_uri(&url));
                    if let Some(Ok(lib_uri)) = lib_uri {
                        let params = lsp_types::ShowDocumentParams {
                            uri: lib_uri.clone(),
                            external: None,
                            take_focus: Some(true),
                            selection: None,
                        };
                        let req = lsp_server::Request::new(
                            lsp_server::RequestId::from(format!(
                                "{SHOW_DOCUMENT_ID_PREFIX}{}",
                                lib_uri.as_str()
                            )),
                            lsp_types::request::ShowDocument::METHOD.to_owned(),
                            params,
                        );
                        self.send(req.into())?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Processes changes to state and triggers appropriate actions (if any).
    fn process_changes(&mut self) -> anyhow::Result<()> {
        // Retrieves document changes (if any).
        if let Some(changes) = self.memory.take_changes() {
            for id in changes {
                // Converts doc id to LSP URI.
                let doc_uri = lsp_types::Uri::from_str(&id);

                // Update analysis snapshot.
                if let Some(doc) = self.memory.get(&id) {
                    // Parse the ink! version.
                    let version_check = || {
                        doc_uri
                            .as_ref()
                            .ok()
                            .and_then(parse_ink_project_version)
                            .as_ref()
                            .map(InkProjectVersion::guess_version)
                            // FIXME: We default to v4 for now because v5 is super new,
                            // but this should be changed at some point.
                            .unwrap_or(Version::V4)
                    };
                    let lang_version = if self.version_check_fuel > 0 {
                        self.version_check_fuel -= 1;
                        version_check()
                    } else {
                        self.snapshots
                            .get(&id)
                            .map(|snapshot| snapshot.lang_version)
                            .unwrap_or_else(version_check)
                    };

                    self.snapshots.insert(
                        id,
                        Snapshot::new(
                            doc.content.clone(),
                            utils::position_encoding(&self.client_capabilities),
                            Some(doc.version),
                            lang_version,
                        ),
                    );
                } else {
                    self.snapshots.remove(&id);
                }

                // Publish diagnostics for each document with changes.
                if let Ok(uri) = doc_uri {
                    self.publish_diagnostics(&uri)?;
                }
            }
        }

        Ok(())
    }

    /// Sends diagnostics notifications to the client for changed (including new) documents.
    fn publish_diagnostics(&mut self, uri: &lsp_types::Uri) -> anyhow::Result<()> {
        // Composes and sends `PublishDiagnostics` notification for document with changes.
        use lsp_types::notification::Notification;
        let notification = lsp_server::Notification::new(
            lsp_types::notification::PublishDiagnostics::METHOD.to_owned(),
            actions::publish_diagnostics(uri, &self.snapshots)?,
        );
        self.send(notification.into())?;

        Ok(())
    }

    /// Handles command responses.
    fn process_command_response(
        &mut self,
        cmd: &str,
        resp: lsp_server::Response,
    ) -> anyhow::Result<()> {
        let req_data = if cmd == COMMAND_CREATE_PROJECT
            && utils::can_create_workspace_resources(&self.client_capabilities)
        {
            // Convert `createProject` response into a workspace edit
            // (only if the client supports the resource operations and document changes).
            resp.result
                .as_ref()
                .and_then(|value| {
                    serde_json::from_value::<CreateProjectResponse>(value.clone()).ok()
                })
                .map(|changes| {
                    let id = lsp_server::RequestId::from(format!(
                        "{INITIALIZE_PROJECT_ID_PREFIX}{}",
                        changes.uri.as_str()
                    ));
                    let params = lsp_types::ApplyWorkspaceEditParams {
                        label: Some("New ink! project".to_owned()),
                        edit: lsp_types::WorkspaceEdit {
                            document_changes: Some(lsp_types::DocumentChanges::from(changes)),
                            ..Default::default()
                        },
                    };
                    (id, params)
                })
        } else if cmd == COMMAND_MIGRATE_PROJECT {
            // Convert `migrateProject` response into a workspace edit.
            resp.result
                .as_ref()
                .and_then(|value| {
                    serde_json::from_value::<MigrateProjectResponse>(value.clone()).ok()
                })
                .map(|changes| {
                    let params = lsp_types::ApplyWorkspaceEditParams {
                        label: Some("Migrate to ink! 5.0".to_owned()),
                        edit: lsp_types::WorkspaceEdit {
                            changes: Some(changes.edits),
                            ..Default::default()
                        },
                    };
                    (
                        lsp_server::RequestId::from(format!(
                            "{MIGRATE_PROJECT_ID_PREFIX}{}",
                            changes.uri.as_str()
                        )),
                        params,
                    )
                })
        } else if cmd == COMMAND_EXTRACT_EVENT
            && utils::can_create_workspace_resources(&self.client_capabilities)
        {
            // Convert `extractEvent` response into a workspace edit
            // (only if the client supports the resource operations and document changes).
            resp.result
                .as_ref()
                .and_then(|value| {
                    serde_json::from_value::<ExtractEventResponse>(value.clone()).ok()
                })
                .map(|changes| {
                    let id = lsp_server::RequestId::from(format!(
                        "{EXTRACT_EVENT_ID_PREFIX}{}{}",
                        changes.uri.as_str(),
                        changes.name
                    ));
                    let params = lsp_types::ApplyWorkspaceEditParams {
                        label: Some("Extract ink! event into standalone package".to_owned()),
                        edit: lsp_types::WorkspaceEdit {
                            document_changes: Some(lsp_types::DocumentChanges::from(changes)),
                            ..Default::default()
                        },
                    };
                    (id, params)
                })
        } else {
            None
        };

        if let Some((req_id, params)) = req_data {
            // Return an empty response for commands that should be executed as workspace edits
            // (e.g. `createProject` and `migrateProject`).
            let mut empty_resp = resp.clone();
            empty_resp.result = Some(serde_json::Value::Null);
            self.send(empty_resp.into())?;

            // Apply workspace edit.
            let req = lsp_server::Request::new(
                req_id,
                lsp_types::request::ApplyWorkspaceEdit::METHOD.to_owned(),
                params,
            );
            self.send(req.into())?;
        } else {
            // Otherwise return response.
            self.send(resp.into())?;
        }

        Ok(())
    }

    /// Sends an LSP message to the client.
    fn send(&self, msg: lsp_server::Message) -> anyhow::Result<()> {
        self.sender
            .send(msg)
            .map_err(|error| anyhow::format_err!("Failed to send message: {error}"))
    }
}

/// Parses the ink! project version from the `Cargo.toml` file for a given `*.rs` file (if possible).
fn parse_ink_project_version(doc_uri: &lsp_types::Uri) -> Option<InkProjectVersion> {
    let doc_url = utils::uri_to_url(doc_uri).ok()?;
    let path = doc_url.to_file_path().ok()?;
    if path.extension().is_some_and(|ext| ext == "rs") {
        // Finds `Cargo.toml` for file (if any).
        let cargo_toml_path = utils::find_cargo_toml(path)?;
        let project_version = parse_ink_project_version_inner(&cargo_toml_path, false);

        // Handles workspace dependencies.
        let is_workspace_dependency = project_version.as_ref().is_some_and(|it| it.workspace);
        if is_workspace_dependency {
            // Finds `Cargo.toml` in workspace root directory (if any).
            let mut parent_dir = cargo_toml_path.clone();
            parent_dir.pop();
            if let Some(workspace_cargo_toml_path) = utils::find_cargo_toml(parent_dir) {
                let workspace_project_version =
                    parse_ink_project_version_inner(&workspace_cargo_toml_path, true);
                if workspace_project_version.is_some() {
                    return workspace_project_version;
                }
            }
        }
        return project_version;
    }
    return None;

    fn parse_ink_project_version_inner(
        cargo_toml_path: &PathBuf,
        workspace: bool,
    ) -> Option<InkProjectVersion> {
        if cargo_toml_path.is_file() {
            if let Ok(cargo_toml) = fs::read_to_string(cargo_toml_path) {
                if let Ok(package) = toml::from_str::<toml::Table>(&cargo_toml) {
                    let dependencies = if workspace {
                        if let Some(toml::Value::Table(workspace)) = package.get("workspace") {
                            workspace.get("dependencies")
                        } else {
                            None
                        }
                    } else {
                        package.get("dependencies")
                    };
                    if let Some(toml::Value::Table(deps)) = dependencies {
                        if let Some(ink_dep) = deps.get("ink") {
                            let (version, path, git, workspace) = match ink_dep {
                                toml::Value::String(ink_version) => {
                                    (Some(ink_version.to_owned()), None, None, false)
                                }
                                toml::Value::Table(ink_dep_info) => {
                                    let parse_dep_value = |key: &str| match ink_dep_info.get(key) {
                                        Some(toml::Value::String(it)) => Some(it.to_owned()),
                                        _ => None,
                                    };
                                    (
                                        parse_dep_value("version"),
                                        parse_dep_value("path"),
                                        parse_dep_value("git"),
                                        matches!(
                                            ink_dep_info.get("workspace"),
                                            Some(toml::Value::Boolean(true))
                                        ),
                                    )
                                }
                                _ => (None, None, None, false),
                            };

                            return (version.is_some()
                                || path.is_some()
                                || git.is_some()
                                || workspace)
                                .then_some(InkProjectVersion {
                                    version,
                                    path,
                                    git,
                                    workspace,
                                });
                        }
                    }
                }
            }
        }

        None
    }
}

/// Represents the ink! project version details as parsed from a `Cargo.toml` file.
#[derive(Debug)]
struct InkProjectVersion {
    version: Option<String>,
    path: Option<String>,
    git: Option<String>,
    workspace: bool,
}

impl InkProjectVersion {
    // Returns the best guess for the ink! project version.
    // FIXME: We default to v4 for now because v5 is super new, but this should be changed at some point.
    fn guess_version(&self) -> Version {
        self.version
            .as_ref()
            .map(|version| {
                if is_gte_v5_1_version_string(version) || version.trim().starts_with('*') {
                    Version::V5(MinorVersion::Latest)
                } else if is_v5_version_string(version) {
                    Version::V5(MinorVersion::V5_0)
                } else {
                    Version::V4
                }
            })
            .unwrap_or_else(|| {
                if self.path.is_some() || self.git.is_some() {
                    // Use latest version for git and path dependencies.
                    Version::V5(MinorVersion::Latest)
                } else {
                    Version::V4
                }
            })
    }
}

/// Returns true if the version string matches `version >= 5.x.x`.
///
/// Ref: <https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html>
///
/// NOTE: We intentionally match `>5.x.x` to v5, because no version greater than v5 currently exists.
fn is_v5_version_string(text: &str) -> bool {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(^|\b)(([\^~>=])|(>=))?(\s)*5\.").unwrap());
    RE.is_match(text)
}

/// Returns true if the version string matches `version >= 5.1.x`.
fn is_gte_v5_1_version_string(text: &str) -> bool {
    static RE: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"(^|\b)(([\^~>=])|(>=))?(\s)*5\.[123456789]").unwrap());
    RE.is_match(text)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::document_uri;
    use std::thread;
    use test_utils::simple_client_config;

    #[test]
    fn main_loop_and_dispatcher_works() {
        // Creates pair of in-memory connections to simulate an LSP client and server.
        let (server_connection, client_connection) = lsp_server::Connection::memory();

        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Runs the message dispatch loop on a separate thread (because `main_loop` function is blocking).
        thread::spawn(|| main_loop(server_connection, client_capabilities));

        // Creates test document URI.
        let uri = document_uri();

        // Verifies that document synchronization notifications (from client to server) trigger `PublishDiagnostics` notifications (from server to client).
        // Creates `DidOpenTextDocument` notification.
        use lsp_types::notification::Notification;
        let open_document_notification = lsp_server::Notification {
            method: lsp_types::notification::DidOpenTextDocument::METHOD.to_owned(),
            params: serde_json::to_value(lsp_types::DidOpenTextDocumentParams {
                text_document: lsp_types::TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "rust".to_owned(),
                    version: 0,
                    text: String::new(),
                },
            })
            .unwrap(),
        };
        // Sends `DidOpenTextDocument` notification from client to server.
        client_connection
            .sender
            .send(open_document_notification.into())
            .unwrap();
        // Confirms receipt of `PublishDiagnostics` notification by the client.
        let message = client_connection.receiver.recv().unwrap();
        let publish_diagnostics_notification = match message {
            lsp_server::Message::Notification(it) => Some(it),
            _ => None,
        }
        .unwrap();
        assert_eq!(
            publish_diagnostics_notification.method,
            lsp_types::notification::PublishDiagnostics::METHOD
        );

        // Verifies that LSP requests (from client to server) get appropriate LSP responses (from server to client).
        // Creates LSP completion request.
        let completion_request_id = lsp_server::RequestId::from(1);
        let completion_request = lsp_server::Request {
            id: completion_request_id.clone(),
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
        // Sends LSP completion request from client to server.
        client_connection
            .sender
            .send(completion_request.into())
            .unwrap();
        // Confirms receipt of LSP completion response by the client.
        let message = client_connection.receiver.recv().unwrap();
        let completion_response = match message {
            lsp_server::Message::Response(it) => Some(it),
            _ => None,
        }
        .unwrap();
        assert_eq!(completion_response.id, completion_request_id);
    }
}
