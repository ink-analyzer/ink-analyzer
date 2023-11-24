//! ink! environment IR.

use ra_ap_syntax::{ast, SyntaxKind, TextRange};

use crate::{InkArg, InkArgKind};

/// An ink! environment argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvironmentArg {
    /// The kind of the ink! environment.
    kind: EnvironmentArgKind,
    /// The ink! attribute argument for the environment.
    arg: InkArg,
}

impl EnvironmentArg {
    /// Returns true if the ink! argument can be converted into an ink! environment argument.
    pub fn can_cast(arg: &InkArg) -> bool {
        matches!(arg.kind(), InkArgKind::Env | InkArgKind::Environment)
    }

    /// Converts an ink! attribute argument into an inK! environment argument.
    pub fn cast(arg: InkArg) -> Option<Self> {
        Self::can_cast(&arg).then_some(Self {
            kind: if let Some(value) = arg.value() {
                match value.kind() {
                    SyntaxKind::PATH | SyntaxKind::PATH_EXPR => EnvironmentArgKind::Path,
                    _ => EnvironmentArgKind::Other,
                }
            } else {
                EnvironmentArgKind::Other
            },
            arg,
        })
    }

    /// Returns the ink! attribute argument for ink! environment.
    pub fn arg(&self) -> &InkArg {
        &self.arg
    }

    /// Returns true if the value is a path.
    pub fn is_path(&self) -> bool {
        self.kind == EnvironmentArgKind::Path
    }

    /// Converts the value if it's an integer literal (decimal or hexadecimal) into a `u32`.
    pub fn as_path_with_inaccurate_text_range(&self) -> Option<ast::Path> {
        self.arg.value()?.as_path_with_inaccurate_text_range()
    }

    /// Returns the text range of the ink! environment argument.
    pub fn text_range(&self) -> TextRange {
        self.arg.text_range()
    }
}

// The ink! environment argument kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum EnvironmentArgKind {
    Path,
    Other,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use test_utils::{quote_as_str, remove_whitespace};

    #[test]
    fn cast_arg_works() {
        for (code, expected_kind, expected_is_path, expected_path_value) in [
            // Paths are detected.
            (
                quote_as_str! {
                    #[ink(env=crate::Environment)]
                },
                EnvironmentArgKind::Path,
                true,
                Some("crate::Environment"),
            ),
            (
                quote_as_str! {
                    #[ink(environment=crate::Environment)]
                },
                EnvironmentArgKind::Path,
                true,
                Some("crate::Environment"),
            ),
            (
                quote_as_str! {
                    #[ink(env=external::Environment)]
                },
                EnvironmentArgKind::Path,
                true,
                Some("external::Environment"),
            ),
            (
                quote_as_str! {
                    #[ink(env=Environment)]
                },
                EnvironmentArgKind::Path,
                true,
                Some("Environment"),
            ),
            // Non-paths are not detected as paths.
            (
                quote_as_str! {
                    #[ink(env=1)]
                },
                EnvironmentArgKind::Other,
                false,
                None,
            ),
            (
                quote_as_str! {
                    #[ink(env="crate::Environment")]
                },
                EnvironmentArgKind::Other,
                false,
                None,
            ),
            (
                quote_as_str! {
                    #[ink(env=_)]
                },
                EnvironmentArgKind::Other,
                false,
                None,
            ),
            (
                quote_as_str! {
                    #[ink(env=struct)]
                },
                EnvironmentArgKind::Other,
                false,
                None,
            ),
        ] {
            // Parse ink! environment argument.
            let selector_arg = EnvironmentArg::cast(
                parse_first_ink_attribute(code)
                    .args()
                    .iter()
                    .find(|arg| matches!(arg.kind(), InkArgKind::Env | InkArgKind::Environment))
                    .unwrap()
                    .clone(),
            )
            .unwrap();

            assert_eq!(selector_arg.kind, expected_kind);

            assert_eq!(selector_arg.is_path(), expected_is_path);

            assert_eq!(
                selector_arg
                    .as_path_with_inaccurate_text_range()
                    .map(|path| remove_whitespace(path.to_string()))
                    .as_deref(),
                expected_path_value
            );
        }
    }
}
