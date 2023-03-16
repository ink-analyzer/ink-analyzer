//! Diagnostic errors and warnings based on ink! semantic rules.
use ra_ap_syntax::ast::Attr;
use ra_ap_syntax::{AstNode, SourceFile, SyntaxKind, TextRange};

use crate::ir::attrs::{Attribute, InkAttributeKind, InkMacroAttributeKind};

/// A diagnostic error or warning.
#[derive(Debug)]
pub struct Diagnostic {
    /// Error or warning message.
    pub message: String,
    /// Text range to highlight.
    pub range: TextRange,
    /// The severity level of the diagnostic.
    pub severity: Severity,
}

/// The severity level of the diagnostic.
#[derive(Debug, Copy, Clone)]
pub enum Severity {
    /// An diagnostic error.
    Error,
    /// A diagnostic warning.
    Warning,
}

/// Computes diagnostics for the source file.
pub fn diagnostics(file: &SourceFile) -> Vec<Diagnostic> {
    let mut diagnostic_errors: Vec<Diagnostic> = Vec::new();

    // Get all attributes
    let attrs = file.syntax().descendants().filter_map(Attr::cast);

    for attr in attrs {
        if let Attribute::Ink(ink_attr) = Attribute::from(attr) {
            let node = ink_attr.ast.syntax();
            match ink_attr.kind {
                // Validate ink! macro attributes
                InkAttributeKind::Macro(ink_macro_kind) => {
                    if ink_macro_kind == InkMacroAttributeKind::Unknown {
                        diagnostic_errors.push(Diagnostic {
                            message: format!("Unknown ink! attribute"),
                            range: node.text_range(),
                            severity: Severity::Warning, // warning because it's possible ink-analyzer is just outdated
                        });
                    } else {
                        match ink_macro_kind {
                            InkMacroAttributeKind::Contract => {
                                let parent_kind = node.parent().unwrap().kind();
                                if parent_kind != SyntaxKind::MODULE {
                                    diagnostic_errors.push(Diagnostic {
                                        message: format!(
                                            "This ink! attribute can only be applied to a mod"
                                        ),
                                        range: node.text_range(),
                                        severity: Severity::Error,
                                    });
                                }
                            }
                            _ => (), // TODO: Validate other ink! macro attributes
                        }
                    }
                }
                // Validate ink! macro attributes
                InkAttributeKind::Arg(_) => {
                    // TODO: Validate ink! argument attributes
                }
                // Handle generic unknown ink! attributes
                _ => {
                    diagnostic_errors.push(Diagnostic {
                        message: format!("Unknown ink! attribute"),
                        range: node.text_range(),
                        severity: Severity::Warning, // warning because it's possible ink-analyzer is just outdated
                    });
                }
            }
        }
    }

    diagnostic_errors
}
