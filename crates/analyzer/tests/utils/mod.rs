//! Utilities for ink! Semantic Analyzer integration tests.

use ink_analyzer::{MinorVersion, Version};

/// Determines the ink! language version from a test fixture's path.
///
/// This hack is used because we don't want to parse `Cargo.toml` in the semantic analyzer
/// (we leave that to the language server - which doesn't need this hack).
pub fn ink_version_from_path(path: &str) -> Version {
    if path.contains("v5") {
        Version::V5(MinorVersion::V5_0)
    } else {
        Version::Legacy
    }
}
