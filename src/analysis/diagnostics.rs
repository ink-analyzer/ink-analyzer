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
}

/// Computes diagnostics for the source file.
pub fn diagnostics(file: &SourceFile) -> Vec<Diagnostic> {
    let mut diagnostic_errors: Vec<Diagnostic> = Vec::new();

    // Get all attributes
    let attrs = file.syntax().descendants().filter_map(Attr::cast);

    for attr in attrs {
        if let Attribute::Ink(ink_attr) = Attribute::from(attr) {
            match ink_attr.kind {
                InkAttributeKind::Macro(ink_macro_kind) => {
                    // Validate ink! macro attributes
                    match ink_macro_kind {
                        InkMacroAttributeKind::Contract => {
                            let node = ink_attr.ast.syntax();
                            let parent_kind = node.parent().unwrap().kind();

                            if parent_kind != SyntaxKind::MODULE {
                                diagnostic_errors.push(Diagnostic {
                                    message: format!("This attribute can only be applied to a mod"),
                                    range: node.text_range(),
                                });
                            }
                        }
                        _ => (), // TODO: Validate other ink! macro attributes
                    }
                }
                InkAttributeKind::Arg(_) => {
                    // TODO: Validate ink! argument attributes
                }
                _ => (),
            }
        }
    }

    diagnostic_errors
}
