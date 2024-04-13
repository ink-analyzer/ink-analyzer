//! LSP command utilities.

use std::collections::HashMap;
use std::fs;

use ink_analyzer::Version;
use serde::{Deserialize, Serialize};

use crate::dispatch::Snapshots;
use crate::translator::{to_lsp, PositionTranslationContext};
use crate::utils;

pub const CREATE_PROJECT: &str = "createProject";
pub const MIGRATE_PROJECT: &str = "migrateProject";

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateProjectResponse {
    pub name: String,
    pub uri: lsp_types::Url,
    pub files: HashMap<lsp_types::Url, String>,
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

    let Ok(project) = ink_analyzer::new_project(name.to_owned(), Version::V5) else {
        return Err(anyhow::format_err!(
            "Failed to create ink! project: {name}\n\
            ink! project names must begin with an alphabetic character, \
            and only contain alphanumeric characters, underscores and hyphens"
        ));
    };

    // Returns create project edits.
    Ok(CreateProjectResponse {
        name: name.to_owned(),
        uri: root.clone(),
        files: HashMap::from([
            (lib_uri, project.lib.plain),
            (cargo_uri, project.cargo.plain),
        ]),
    })
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MigrateProjectResponse {
    pub uri: lsp_types::Url,
    pub edits: HashMap<lsp_types::Url, Vec<lsp_types::TextEdit>>,
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

    // Computes edits for the Rust file.
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
        uri: uri.clone(),
        edits: HashMap::from([(uri.clone(), code_edits), (cargo_uri, cargo_edits)]),
    })
}

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
                toml_edit::Item::Value(toml_edit::Value::from("5.0.0")),
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
            new_ink_dep_value.insert("version", toml_edit::Value::from("5.0.0"));
            if !new_ink_dep_value.contains_key("default-features") {
                new_ink_dep_value.insert("default-features", toml_edit::Value::from(false));
            }
            new_ink_dep_value
        }
        _ => {
            let mut new_ink_dep_value = toml_edit::InlineTable::new();
            new_ink_dep_value.insert("version", toml_edit::Value::from("5.0.0"));
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
            *ink_e2e_dep = toml_edit::Item::Value(toml_edit::Value::from("5.0.0"));
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
ink = { version = "5.0.0", default-features = false }

[dev-dependencies]
ink_e2e = "5.0.0"

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
}
