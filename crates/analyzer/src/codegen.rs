//! Utilities for generate ink! project files.

use self::snippets::{CARGO_TOML_PLAIN, CARGO_TOML_SNIPPET, CONTRACT_PLAIN, CONTRACT_SNIPPET};
use crate::utils;

pub mod snippets;

/// Code stubs/snippets for creating an ink! project
/// (i.e. code stubs/snippets for `lib.rs` and `Cargo.toml`).
pub struct Project {
    /// The `lib.rs` content.
    pub lib: ProjectFile,
    /// The `Cargo.toml` content.
    pub cargo: ProjectFile,
}

/// Code stubs/snippets for creating a file in an ink! project
/// (e.g. `lib.rs` or `Cargo.toml` for an ink! contract).
pub struct ProjectFile {
    /// A plain text code stub.
    pub plain: String,
    /// A snippet (i.e. with tab stops and/or placeholders).
    pub snippet: Option<String>,
}

/// An ink! project error.
#[non_exhaustive]
pub enum Error {
    /// Invalid package name.
    ///
    /// Ref: <https://github.com/rust-lang/rfcs/blob/master/text/0940-hyphens-considered-harmful.md>.
    PackageName,
}

/// Returns code stubs/snippets for creating a new ink! project given a name.
pub fn new_project(name: String) -> Result<Project, Error> {
    // Validates that name is a valid Rust package name.
    // Ref: <https://github.com/rust-lang/rfcs/blob/master/text/0940-hyphens-considered-harmful.md>.
    if name
        .chars()
        .all(|it| it.is_alphabetic() || it == '_' || it == '-')
    {
        return Err(Error::PackageName);
    }

    // Generates `mod` and storage `struct` names for the contract.
    let module_name = name.replace('-', "_");
    let struct_name = utils::pascal_case(&module_name);

    // Returns project code stubs/snippets.
    Ok(Project {
        // Generates `lib.rs`.
        lib: ProjectFile {
            plain: CONTRACT_PLAIN
                .replace("my_contract", &module_name)
                .replace("MyContract", &struct_name),
            snippet: Some(
                CONTRACT_SNIPPET
                    .replace("my_contract", &module_name)
                    .replace("MyContract", &struct_name),
            ),
        },
        // Generates `Cargo.toml`.
        cargo: ProjectFile {
            plain: CARGO_TOML_PLAIN.replace("my_contract", &name),
            snippet: Some(CARGO_TOML_SNIPPET.replace("my_contract", &name)),
        },
    })
}
