//! Diagnostic errors and warnings based on ink! semantic rules.
use ra_ap_syntax::ast::Attr;
use ra_ap_syntax::{AstNode, SourceFile, SyntaxKind, TextRange};

use crate::ir::attrs::{Attribute, InkArgAttributeKind, InkAttributeKind, InkPathAttributeKind};

/// A diagnostic error or warning.
#[derive(Debug)]
pub struct Diagnostic {
    // TODO: Add diagnostic codes
    /// Error or warning message.
    pub message: String,
    /// Text range to highlight.
    pub range: TextRange,
    /// The severity level of the diagnostic.
    pub severity: Severity,
}

/// The severity level of the diagnostic.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    /// An diagnostic error.
    Error,
    /// A diagnostic warning.
    Warning,
}

/// Computes diagnostics for the source file.
pub fn diagnostics(file: &SourceFile) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Get all attributes
    let attrs = file.syntax().descendants().filter_map(Attr::cast);

    for attr in attrs {
        if let Attribute::Ink(ink_attr) = Attribute::from(attr) {
            let node = ink_attr.ast.syntax();
            match ink_attr.kind {
                // Validate ink! path attributes
                InkAttributeKind::Path(ink_path_kind) => {
                    if ink_path_kind == InkPathAttributeKind::Unknown {
                        results.push(Diagnostic {
                            message: format!(
                                "Unknown ink! path attribute: '{}'",
                                node.text().to_string()
                            ),
                            range: node.text_range(),
                            severity: Severity::Warning, // warning because it's possible ink-analyzer is just outdated
                        });
                    } else {
                        match ink_path_kind {
                            InkPathAttributeKind::Contract => {
                                let parent_kind = node.parent().unwrap().kind();
                                if parent_kind != SyntaxKind::MODULE {
                                    results.push(Diagnostic {
                                        message: format!(
                                            "This ink! attribute can only be applied to a mod"
                                        ),
                                        range: node.text_range(),
                                        severity: Severity::Error,
                                    });
                                }
                            }
                            _ => (), // TODO: Validate other ink! path attributes
                        }
                    }
                }
                // Validate ink! argument attributes
                InkAttributeKind::Arg(ink_arg_kind) => {
                    if ink_arg_kind == InkArgAttributeKind::Unknown {
                        results.push(Diagnostic {
                            message: format!(
                                "Unknown ink! argument attribute: '{}'",
                                node.text().to_string()
                            ),
                            range: node.text_range(),
                            severity: Severity::Warning, // warning because it's possible ink-analyzer is just outdated
                        });
                    } else {
                        // TODO: Validate ink! argument attributes
                    }
                }
            }
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(code: &str) -> SourceFile {
        SourceFile::parse(code).tree()
    }

    // tests for `#[ink::contract]` attributes
    mod contract {
        use super::*;

        #[test]
        fn ink_contract_attribute_on_mod_works() {
            let file = parse(
                r#"
                #[ink::contract]
                mod flipper {
                }
                "#,
            );

            let results = diagnostics(&file);
            assert!(results.is_empty());
        }

        #[test]
        fn ink_contract_attribute_with_args_on_mod_works() {
            let file = parse(
                r#"
                #[ink::contract(keep_attr="foo, bar")]
                mod flipper {
                    // #[foo]
                    // #[bar]
                }
                "#,
            );

            let results = diagnostics(&file);
            assert!(results.is_empty());
        }

        #[test]
        fn ink_contract_attribute_in_mod_body_fails() {
            let file = parse(
                r#"
                mod flipper {
                    #[ink::contract]
                }
                "#,
            );

            let results = diagnostics(&file);
            assert_eq!(1, results.len());
            assert_eq!(Severity::Error, results[0].severity);
        }

        #[test]
        fn ink_contract_attribute_on_fn_fails() {
            let file = parse(
                r#"
                #[ink::contract]
                fn flipper() {
                }
                "#,
            );

            let results = diagnostics(&file);
            assert_eq!(1, results.len());
            assert_eq!(Severity::Error, results[0].severity);
        }

        #[test]
        fn ink_contract_attribute_on_struct_fails() {
            let file = parse(
                r#"
                #[ink::contract]
                struct Flipper {
                }
                "#,
            );

            let results = diagnostics(&file);
            assert_eq!(1, results.len());
            assert_eq!(Severity::Error, results[0].severity);
        }
    }

    // tests for unknown ink! attributes
    mod unknown {
        use super::*;

        #[test]
        fn ink_unknown_path_attribute_fails() {
            let file = parse(
                r#"
                #[ink::xyz]
                mod flipper {
                }
                "#,
            );

            let results = diagnostics(&file);
            assert_eq!(1, results.len());
            assert_eq!(Severity::Warning, results[0].severity);
        }

        #[test]
        fn ink_unknown_multi_path_attribute_fails() {
            let file = parse(
                r#"
                #[ink::abc::xyz]
                mod flipper {
                }
                "#,
            );

            let results = diagnostics(&file);
            assert_eq!(1, results.len());
            assert_eq!(Severity::Warning, results[0].severity);
        }

        #[test]
        fn ink_unknown_arg_attribute_fails() {
            let file = parse(
                r#"
                #[ink(xyz)]
                struct Flipper {
                }
                "#,
            );

            let results = diagnostics(&file);
            assert_eq!(1, results.len());
            assert_eq!(Severity::Warning, results[0].severity);
        }
    }
}
