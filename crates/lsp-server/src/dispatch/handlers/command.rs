//! LSP command utilities.

use std::collections::HashMap;
use std::fs;

use ink_analyzer::{MinorVersion, Version};
use serde::{Deserialize, Serialize};

use crate::dispatch::Snapshots;
use crate::translator::{to_lsp, PositionTranslationContext};
use crate::utils;

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateProjectResponse {
    pub name: String,
    pub uri: lsp_types::Url,
    pub files: HashMap<lsp_types::Url, String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MigrateProjectResponse {
    pub uri: lsp_types::Url,
    pub edits: HashMap<lsp_types::Url, Vec<lsp_types::TextEdit>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExtractEventResponse {
    pub name: String,
    pub uri: lsp_types::Url,
    pub files: HashMap<lsp_types::Url, String>,
    pub edits: HashMap<lsp_types::Url, Vec<lsp_types::TextEdit>>,
}

/// Handles create project command request.
pub fn handle_create_project(
    name: &str,
    root: &lsp_types::Url,
) -> anyhow::Result<CreateProjectResponse> {
    let uris = root
        .clone()
        .join("lib.rs")
        .ok()
        .zip(root.join("Cargo.toml").ok());
    let Some((lib_uri, cargo_uri)) = uris else {
        return Err(anyhow::format_err!("Failed to create ink! project: {name}"));
    };

    let Ok(project) = ink_analyzer::new_project(name.to_owned(), Version::V5(MinorVersion::Latest))
    else {
        return Err(anyhow::format_err!(
            "Failed to create ink! project: {name}\n\
            ink! project names must begin with an alphabetic character, \
            and only contain alphanumeric characters, underscores and hyphens"
        ));
    };

    // Returns create project edits.
    Ok(CreateProjectResponse {
        name: name.to_owned(),
        uri: root.to_owned(),
        files: HashMap::from([
            (lib_uri, project.lib.plain),
            (cargo_uri, project.cargo.plain),
        ]),
    })
}

/// Handles migrate project command.
pub fn handle_migrate_project(
    uri: &lsp_types::Url,
    snapshots: &Snapshots,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<MigrateProjectResponse> {
    let id = uri.to_string();

    if !id.ends_with(".rs") {
        return Err(anyhow::format_err!("Failed to migrate ink! project."));
    }
    let Some(snapshot) = snapshots.get(&id) else {
        return Err(anyhow::format_err!("Failed to migrate ink! project."));
    };

    // Computes edits for contract's `*.rs` file.
    let code_edits = snapshot
        .analysis
        .migrate()
        .into_iter()
        .filter_map(|edit| {
            to_lsp::range(edit.range, &snapshot.context).map(|range| lsp_types::TextEdit {
                range,
                new_text: edit.text,
            })
        })
        .collect();

    // Computes edits for the `Cargo.toml` file.
    let Some(cargo_path) = uri.to_file_path().ok().and_then(utils::find_cargo_toml) else {
        return Err(anyhow::format_err!(
            "Failed to migrate ink! project.\nCouldn't locate `Cargo.toml` file."
        ));
    };
    let Ok(cargo_toml) = fs::read_to_string(&cargo_path) else {
        return Err(anyhow::format_err!(
            "Failed to migrate ink! project.\nCouldn't read `Cargo.toml` file."
        ));
    };
    let Ok(cargo_uri) = lsp_types::Url::from_file_path(cargo_path) else {
        return Err(anyhow::format_err!(
            "Failed to migrate ink! project.\nCouldn't convert `Cargo.toml` path into a URI."
        ));
    };
    let cargo_toml_updates = migrate_cargo_toml(&cargo_toml)?;
    let context =
        PositionTranslationContext::new(&cargo_toml, utils::position_encoding(client_capabilities));
    let Some(range) = to_lsp::range(
        ink_analyzer::TextRange::new(
            ink_analyzer::TextSize::new(0),
            ink_analyzer::TextSize::new(cargo_toml.len() as u32),
        ),
        &context,
    ) else {
        return Err(anyhow::format_err!(
            "Failed to migrate ink! project.\nCouldn't modify `Cargo.toml` file."
        ));
    };
    let cargo_edits = vec![lsp_types::TextEdit {
        range,
        new_text: cargo_toml_updates,
    }];

    // Returns edits.
    Ok(MigrateProjectResponse {
        uri: uri.to_owned(),
        edits: HashMap::from([(uri.clone(), code_edits), (cargo_uri, cargo_edits)]),
    })
}

/// Handles migrate project command.
pub fn handle_extract_event(
    uri: &lsp_types::Url,
    range: ink_analyzer::TextRange,
    snapshots: &Snapshots,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<ExtractEventResponse> {
    let id = uri.to_string();

    if !id.ends_with(".rs") {
        return Err(anyhow::format_err!("Failed to extract ink! event."));
    }

    // Retrieves snapshot and computes extraction.
    let Some((extraction, snapshot)) = snapshots
        .get(&id)
        .and_then(|snapshot| snapshot.analysis.extract(range).zip(Some(snapshot)))
    else {
        return Err(anyhow::format_err!("Failed to extract ink! event."));
    };

    // Finds path of contract's `Cargo.toml` file.
    let Some(cargo_path) = uri.to_file_path().ok().and_then(utils::find_cargo_toml) else {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\nCouldn't locate `Cargo.toml` file."
        ));
    };

    // Creates names and uris for new event package.
    // See `package_name` doc for difference btn `event_package_name` and `event_crate_name`.
    let (event_package_name, event_crate_name) = package_name(&extraction.name)?;
    let mut event_package_path = cargo_path.clone();
    event_package_path.pop();
    let mut event_package_dir = event_package_name.clone();
    event_package_path.push(&event_package_dir);
    let mut count = 0u8;
    while event_package_path.exists() && count < 10 {
        count += 1;
        event_package_path.pop();
        event_package_dir = format!("{event_package_name}{count}");
        event_package_path.push(&event_package_dir);
    }
    if event_package_path.exists() {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\n\
            Couldn't find a unique name for the new package directory."
        ));
    }
    // Creates event `Cargo.toml` uri.
    let mut event_cargo_path = event_package_path.clone();
    event_cargo_path.push("Cargo.toml");
    let Ok(event_cargo_uri) = lsp_types::Url::from_file_path(event_cargo_path) else {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\n\
            Couldn't convert event package `Cargo.toml` path into a URI."
        ));
    };
    // Creates event `lib.rs` uri.
    let mut event_lib_path = event_package_path.clone();
    event_lib_path.push("src");
    event_lib_path.push("lib.rs");
    let Ok(event_lib_uri) = lsp_types::Url::from_file_path(event_lib_path) else {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\n\
            Couldn't convert event package `lib.rs` path into a URI."
        ));
    };

    // Translates edits for contract's `lib.rs` file.
    let Some(import_insert_range) = to_lsp::range(
        ink_analyzer::TextRange::new(extraction.import_offset, extraction.import_offset),
        &snapshot.context,
    ) else {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\nCouldn't translate import insert offset."
        ));
    };
    let contract_edits = extraction
        .edits
        .into_iter()
        .filter_map(|edit| {
            to_lsp::range(edit.range, &snapshot.context).map(|range| lsp_types::TextEdit {
                range,
                new_text: edit.text,
            })
        })
        .chain(std::iter::once(lsp_types::TextEdit {
            range: import_insert_range,
            new_text: format!(
                "{}use {event_crate_name}::{};",
                extraction.import_indent, extraction.name
            ),
        }))
        .collect();

    // Computes edits for contract's `Cargo.toml` file.
    let Ok(contract_cargo_toml) = fs::read_to_string(&cargo_path) else {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\nCouldn't read `Cargo.toml` file."
        ));
    };
    let Ok(cargo_uri) = lsp_types::Url::from_file_path(cargo_path) else {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\nCouldn't convert `Cargo.toml` path into a URI."
        ));
    };
    let mut event_package_dep = toml_edit::InlineTable::new();
    event_package_dep.insert(
        "path",
        toml_edit::Value::from(event_package_dir.to_string()),
    );
    event_package_dep.insert("default-features", toml_edit::Value::from(false));
    let contract_cargo_toml_updates = add_dependency_to_cargo_toml(
        &contract_cargo_toml,
        &event_package_name,
        toml_edit::Item::Value(toml_edit::Value::InlineTable(event_package_dep)),
    )?;
    let context = PositionTranslationContext::new(
        &contract_cargo_toml,
        utils::position_encoding(client_capabilities),
    );
    let Some(range) = to_lsp::range(
        ink_analyzer::TextRange::new(
            ink_analyzer::TextSize::new(0),
            ink_analyzer::TextSize::new(contract_cargo_toml.len() as u32),
        ),
        &context,
    ) else {
        return Err(anyhow::format_err!(
            "Failed to extract ink! event.\nCouldn't modify `Cargo.toml` file."
        ));
    };
    let contract_cargo_edits = vec![lsp_types::TextEdit {
        range,
        new_text: contract_cargo_toml_updates,
    }];

    // Returns edits.
    let event_cargo_toml = CARGO_TOML_MINIMAL_V5.replace("my_package", &event_package_name);
    Ok(ExtractEventResponse {
        name: event_package_name,
        uri: uri.to_owned(),
        files: HashMap::from([
            (event_lib_uri, extraction.content),
            (event_cargo_uri, event_cargo_toml),
        ]),
        edits: HashMap::from([
            (uri.to_owned(), contract_edits),
            (cargo_uri, contract_cargo_edits),
        ]),
    })
}

const LATEST_INK_VERSION: &str = "5.1.1";

/// Migrates the contents of a `Cargo.toml` file from ink! 4.x to ink! 5.0.
fn migrate_cargo_toml(input: &str) -> anyhow::Result<String> {
    let Ok(mut doc) = input.parse::<toml_edit::DocumentMut>() else {
        return Err(anyhow::format_err!("Failed to parse `TOML` file."));
    };
    let Some(deps) = doc
        .get_mut("dependencies")
        .and_then(toml_edit::Item::as_table_mut)
    else {
        return Err(anyhow::format_err!("Failed to parse `TOML` file."));
    };

    // Upgrade `ink` dependency.
    let Some(ink_dep) = deps.get_mut("ink") else {
        return Err(anyhow::format_err!("Failed to parse `Cargo.toml` file."));
    };
    let new_ink_dep_value = match ink_dep {
        toml_edit::Item::Table(ink_dep_value) => {
            let mut new_ink_dep_value = ink_dep_value.clone();
            new_ink_dep_value.insert(
                "version",
                toml_edit::Item::Value(toml_edit::Value::from(LATEST_INK_VERSION)),
            );
            if !new_ink_dep_value.contains_key("default-features") {
                new_ink_dep_value.insert(
                    "default-features",
                    toml_edit::Item::Value(toml_edit::Value::from(false)),
                );
            }
            new_ink_dep_value.into_inline_table()
        }
        toml_edit::Item::Value(toml_edit::Value::InlineTable(ink_dep_value)) => {
            let mut new_ink_dep_value = ink_dep_value.clone();
            new_ink_dep_value.insert("version", toml_edit::Value::from(LATEST_INK_VERSION));
            if !new_ink_dep_value.contains_key("default-features") {
                new_ink_dep_value.insert("default-features", toml_edit::Value::from(false));
            }
            new_ink_dep_value
        }
        _ => {
            let mut new_ink_dep_value = toml_edit::InlineTable::new();
            new_ink_dep_value.insert("version", toml_edit::Value::from(LATEST_INK_VERSION));
            new_ink_dep_value.insert("default-features", toml_edit::Value::from(false));
            new_ink_dep_value
        }
    };
    *ink_dep = toml_edit::Item::Value(toml_edit::Value::InlineTable(new_ink_dep_value));

    // Remove `scale` and `scale-info` dependencies (if any).
    if let Some(scale_dep) = deps.get_mut("scale") {
        *scale_dep = toml_edit::Item::None;
    }
    if let Some(scale_info_dep) = deps.get_mut("scale-info") {
        *scale_info_dep = toml_edit::Item::None;
    }

    // Upgrade `ink_e2e` dev-dependency (if any).
    if let Some(dev_deps) = doc
        .get_mut("dev-dependencies")
        .and_then(toml_edit::Item::as_table_mut)
    {
        if let Some(ink_e2e_dep) = dev_deps.get_mut("ink_e2e") {
            *ink_e2e_dep = toml_edit::Item::Value(toml_edit::Value::from(LATEST_INK_VERSION));
        }
    }

    // Remove `scale/std` and `scale-info/std` features (if any).
    if let Some(features) = doc
        .get_mut("features")
        .and_then(toml_edit::Item::as_table_mut)
    {
        if let Some(std_features) = features
            .get_mut("std")
            .and_then(toml_edit::Item::as_array_mut)
        {
            std_features.retain(|feature| {
                !matches!(feature.as_str(), Some("scale/std" | "scale-info/std"))
            });
        }
    }

    Ok(doc.to_string())
}

/// Add a dependency to a `Cargo.toml` file.
fn add_dependency_to_cargo_toml(
    input: &str,
    name: &str,
    item: toml_edit::Item,
) -> anyhow::Result<String> {
    let Ok(mut doc) = input.parse::<toml_edit::DocumentMut>() else {
        return Err(anyhow::format_err!("Failed to parse `TOML` file."));
    };
    let Some(deps) = doc
        .get_mut("dependencies")
        .and_then(toml_edit::Item::as_table_mut)
    else {
        return Err(anyhow::format_err!("Failed to parse `TOML` file."));
    };

    // Adds dependency item.
    deps.insert(name, item);

    Ok(doc.to_string())
}

/// Converts the given item name into a valid Rust package name (if possible).
///
/// Returns a "package name" and "crate name", where "crate name" is the name used inside Rust files,
/// while the "package name" is a vanity friendly version of the crate name
/// (i.e. in which hyphens/`-` are used instead of underscores/`_` to separate words), that is used
/// in directory names and in `Cargo.toml`.
fn package_name(name: &str) -> anyhow::Result<(String, String)> {
    // Validates that name contains some valid characters for a package name.
    // Ref: <https://doc.rust-lang.org/cargo/reference/manifest.html#the-name-field>.
    if name.is_empty()
        || !name
            .chars()
            .any(|c| c.is_alphanumeric() || c == '_' || c == '-')
    {
        return Err(anyhow::format_err!(
            "Item name must contain at least one valid character for a package name."
        ));
    }

    // Validates that name can be converted into a valid ink! project name
    // (i.e. project names must additionally begin with an alphabetic character).
    // Ref: <https://github.com/paritytech/cargo-contract/blob/v3.2.0/crates/build/src/new.rs#L34-L52>.
    if !name.chars().next().is_some_and(char::is_alphabetic) {
        return Err(anyhow::format_err!(
            "Item name must start with an alphabetic character."
        ));
    }

    let mut buffer = Vec::new();
    let mut last_char_is_uppercase = false;
    let mut last_char_is_hyphen = false;
    for (idx, char) in name.chars().enumerate() {
        if idx > 0 && char.is_uppercase() && !last_char_is_uppercase && !last_char_is_hyphen {
            buffer.push('-');
        }
        if char.is_alphanumeric() {
            buffer.push(char);
            last_char_is_hyphen = false;
        } else if !last_char_is_hyphen {
            buffer.push('-');
            last_char_is_hyphen = true;
        }
        last_char_is_uppercase = char.is_uppercase();
    }

    let name = buffer
        .iter()
        .collect::<String>()
        .to_lowercase()
        .trim_end_matches('-')
        .to_owned();
    let crate_name = name.replace('-', "_");

    Ok((name, crate_name))
}

impl From<CreateProjectResponse> for lsp_types::DocumentChanges {
    fn from(value: CreateProjectResponse) -> Self {
        let doc_changes = value
            .files
            .into_iter()
            .flat_map(|(uri, content)| {
                vec![
                    lsp_types::DocumentChangeOperation::Op(lsp_types::ResourceOp::Create(
                        lsp_types::CreateFile {
                            uri: uri.clone(),
                            options: None,
                            annotation_id: None,
                        },
                    )),
                    lsp_types::DocumentChangeOperation::Edit(lsp_types::TextDocumentEdit {
                        text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                            uri,
                            version: None,
                        },
                        edits: vec![lsp_types::OneOf::Left(lsp_types::TextEdit {
                            range: lsp_types::Range::default(),
                            new_text: content,
                        })],
                    }),
                ]
            })
            .collect();
        lsp_types::DocumentChanges::Operations(doc_changes)
    }
}

impl From<ExtractEventResponse> for lsp_types::DocumentChanges {
    fn from(value: ExtractEventResponse) -> Self {
        let doc_changes = value
            .files
            .into_iter()
            .flat_map(|(uri, content)| {
                vec![
                    lsp_types::DocumentChangeOperation::Op(lsp_types::ResourceOp::Create(
                        lsp_types::CreateFile {
                            uri: uri.clone(),
                            options: None,
                            annotation_id: None,
                        },
                    )),
                    lsp_types::DocumentChangeOperation::Edit(lsp_types::TextDocumentEdit {
                        text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                            uri,
                            version: None,
                        },
                        edits: vec![lsp_types::OneOf::Left(lsp_types::TextEdit {
                            range: lsp_types::Range::default(),
                            new_text: content,
                        })],
                    }),
                ]
            })
            .chain(value.edits.into_iter().map(|(uri, edits)| {
                lsp_types::DocumentChangeOperation::Edit(lsp_types::TextDocumentEdit {
                    text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                        uri,
                        version: None,
                    },
                    edits: edits.into_iter().map(lsp_types::OneOf::Left).collect(),
                })
            }))
            .collect();
        lsp_types::DocumentChanges::Operations(doc_changes)
    }
}

pub const CARGO_TOML_MINIMAL_V5: &str = r#"[package]
name = "my_package"
version = "0.1.0"
authors = ["[your_name] <[your_email]>"]
edition = "2021"

[dependencies]
ink = { version = "5.1.1", default-features = false }

[features]
default = ["std"]
std = [
    "ink/std",
]
ink-as-dependency = []
e2e-tests = []"#;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn handle_create_project_works() {
        // Creates test project.
        let project_name = "hello_ink";
        let project_uri = lsp_types::Url::parse("file:///tmp/hello_ink/").unwrap();

        // Calls handler and verifies that the expected response is returned.
        let result = handle_create_project(project_name, &project_uri);
        assert!(result.is_ok());
        let resp = result.unwrap();
        // Verifies project metadata.
        assert_eq!(resp.name, project_name);
        assert_eq!(resp.uri, project_uri);
        // Verifies project files and their contents.
        let lib_uri = project_uri.clone().join("lib.rs").unwrap();
        let cargo_uri = project_uri.clone().join("Cargo.toml").unwrap();
        let lib_content = resp.files.get(&lib_uri).unwrap();
        let cargo_content = resp.files.get(&cargo_uri).unwrap();
        assert!(resp.files.contains_key(&lib_uri));
        assert!(resp.files.contains_key(&cargo_uri));
        assert!(lib_content.contains("#[ink::contract]\npub mod hello_ink {"));
        assert!(cargo_content.contains(r#"name = "hello_ink""#));
    }

    #[test]
    fn migrate_cargo_toml_works() {
        let input = r#"
[package]
name = "erc20"
version = "4.3.0"
authors = ["Parity Technologies <admin@parity.io>"]
edition = "2021"
publish = false

[dependencies]
ink = { version = "4.3", default-features = false }

scale = { package = "parity-scale-codec", version = "3", default-features = false, features = ["derive"] }
scale-info = { version = "2.5", default-features = false, features = ["derive"], optional = true }

[dev-dependencies]
ink_e2e = { version = "4.3" }

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
    "scale/std",
    "scale-info/std",
]
ink-as-dependency = []
e2e-tests = []

"#;
        let expected_output = r#"
[package]
name = "erc20"
version = "4.3.0"
authors = ["Parity Technologies <admin@parity.io>"]
edition = "2021"
publish = false

[dependencies]
ink = { version = "5.1.1", default-features = false }

[dev-dependencies]
ink_e2e = "5.1.1"

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
]
ink-as-dependency = []
e2e-tests = []

"#;
        let output = migrate_cargo_toml(input);
        assert!(output.is_ok());
        assert_eq!(output.unwrap(), expected_output);
    }

    #[test]
    fn add_dependency_to_cargo_toml_works() {
        let input = r#"
[package]
name = "erc20"
version = "4.3.0"
authors = ["Parity Technologies <admin@parity.io>"]
edition = "2021"
publish = false

[dependencies]
ink = { version = "5.1.1", default-features = false }

[dev-dependencies]
ink_e2e = "5.1.1"

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
]
ink-as-dependency = []
e2e-tests = []

"#;
        let expected_output = r#"
[package]
name = "erc20"
version = "4.3.0"
authors = ["Parity Technologies <admin@parity.io>"]
edition = "2021"
publish = false

[dependencies]
ink = { version = "5.1.1", default-features = false }
event = { path = "event", default-features = false }

[dev-dependencies]
ink_e2e = "5.1.1"

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
]
ink-as-dependency = []
e2e-tests = []

"#;
        let mut event_package_dep = toml_edit::InlineTable::new();
        event_package_dep.insert("path", toml_edit::Value::from("event"));
        event_package_dep.insert("default-features", toml_edit::Value::from(false));
        let output = add_dependency_to_cargo_toml(
            input,
            "event",
            toml_edit::Item::Value(toml_edit::Value::InlineTable(event_package_dep)),
        );
        assert!(output.is_ok());
        assert_eq!(output.unwrap(), expected_output);
    }

    #[test]
    fn package_name_works() {
        for (name, expected_result) in [
            // Works.
            ("hello", Some(("hello", "hello"))),
            ("Hello", Some(("hello", "hello"))),
            ("HELLO", Some(("hello", "hello"))),
            ("hello world", Some(("hello-world", "hello_world"))),
            ("hello_world", Some(("hello-world", "hello_world"))),
            ("hello-world", Some(("hello-world", "hello_world"))),
            ("Hello World", Some(("hello-world", "hello_world"))),
            ("HelloWorld", Some(("hello-world", "hello_world"))),
            ("Hello-World", Some(("hello-world", "hello_world"))),
            ("HELLO_WORLD", Some(("hello-world", "hello_world"))),
            ("Hello?World", Some(("hello-world", "hello_world"))),
            ("HELLO?WORLD", Some(("hello-world", "hello_world"))),
            ("HelloWorld?", Some(("hello-world", "hello_world"))),
            ("Hello💝World", Some(("hello-world", "hello_world"))),
            ("hello-world   ?=", Some(("hello-world", "hello_world"))),
            // Fails.
            ("", None), // Empty.
            // No valid characters.
            (" ", None),
            ("?,=", None),
            ("💝", None),
            // Doesn't start with alphabetic character.
            ("_hello", None),
            ("-hello", None),
            ("1hello", None),
        ] {
            assert_eq!(
                package_name(name)
                    .ok()
                    .as_ref()
                    .map(|(package_name, crate_name)| (package_name.as_str(), crate_name.as_str())),
                expected_result,
                "name: {}",
                name
            )
        }
    }
}
