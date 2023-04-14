//! Utilities for ink! diagnostics.

use ink_analyzer_ir::ast::{AstToken, Ident};
use ink_analyzer_ir::{FromSyntax, IRItem, InkArgKind, InkAttributeKind, InkPathKind};

use crate::{Diagnostic, Severity};

/// Pushes a new diagnostic into the current list of diagnostics.
pub fn push_diagnostic(current: &mut Vec<Diagnostic>, value: Diagnostic) {
    current.push(value)
}

/// Appends new diagnostics to the current list of diagnostics.
pub fn append_diagnostics(current: &mut Vec<Diagnostic>, updates: &mut Vec<Diagnostic>) {
    current.append(updates)
}

/// Returns an error diagnostic for every instance of `__ink_` prefixed identifier found.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/idents_lint.rs#L20>
pub fn ensure_no_ink_identifiers<T: FromSyntax>(item: &T) -> Vec<Diagnostic> {
    item.syntax()
        .descendants_with_tokens()
        .filter_map(|elem| {
            elem.into_token().and_then(Ident::cast).and_then(|ident| {
                ident
                    .to_string()
                    .starts_with("__ink_")
                    .then_some(Diagnostic {
                        message: format!(
                            "Invalid identifier starting with __ink_: {}",
                            ident.text()
                        ),
                        range: ident.syntax().text_range(),
                        severity: Severity::Error,
                    })
            })
        })
        .collect()
}

/// Returns a warning diagnostic for every unknown ink attribute found.
///
/// Handles both ink! path attributes (i.e ink! attribute macros e.g `#[ink::xyz]`) and ink! argument attributes (e.g `#[ink(xyz)]`).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L876-L1024>.
pub fn ensure_no_unknown_ink_attributes<T: FromSyntax>(item: &T) -> Vec<Diagnostic> {
    item.ink_attrs_descendants()
        .into_iter()
        .filter_map(|attr| {
            matches!(
                attr.kind(),
                InkAttributeKind::Path(InkPathKind::Unknown)
                    | InkAttributeKind::Arg(InkArgKind::Unknown)
            )
            .then_some(Diagnostic {
                message: format!("Unknown ink! attribute: '{}'", attr.syntax()),
                range: attr.syntax().text_range(),
                severity: Severity::Warning, // warning because it's possible analyzer is just outdated
            })
        })
        .collect()
}

/// Runs generic diagnostics that apply to all IR items.
/// (e.g `ensure_no_unknown_ink_attributes` and `ensure_no_ink_identifiers`).
pub fn run_generic_diagnostics<T: FromSyntax>(item: &T) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Ensure no `__ink_` prefixed identifiers, See `ensure_no_ink_identifiers` doc.
    append_diagnostics(&mut results, &mut ensure_no_ink_identifiers(item));

    // Ensure no invalid ink! attributes, See `ensure_no_invalid_ink_attributes` doc.
    append_diagnostics(&mut results, &mut ensure_no_unknown_ink_attributes(item));

    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::InkFile;

    #[test]
    fn identifiers_not_prefixed_with_ink_works() {
        let file = InkFile::parse(
            r#"
            #[ink::contract]
            mod flipper {
                #[ink(storage)]
                struct Flipper {
                    value: bool,
                }

                impl Flipper {
                    #[ink(constructor)]
                    pub fn new(init_value: bool) -> Self {
                        Self { value: init_value }
                    }
                }
            }
            "#,
        );

        let results = ensure_no_ink_identifiers(&file);
        assert!(results.is_empty());
    }

    #[test]
    fn identifiers_prefixed_with_ink_fails() {
        let file = InkFile::parse(
            r#"
            #[ink::contract]
            mod __ink_example {
                #[ink(storage)]
                struct __ink_Example {
                    value: bool,
                }

                impl __ink_Example {
                    #[ink(constructor)]
                    pub fn __ink_new(__ink_init_value: bool) -> Self {
                        Self { value: __ink_init_value }
                    }
                }
            }
            "#,
        );

        let results = ensure_no_ink_identifiers(&file);
        // There are 6 occurrences of __ink_ prefixed identifiers in the code snippet.
        assert_eq!(results.len(), 6);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .into_iter()
                .filter(|item| item.severity == Severity::Error)
                .collect::<Vec<Diagnostic>>()
                .len(),
            6
        );
    }

    #[test]
    fn unknown_ink_path_attribute_fails() {
        let file = InkFile::parse(
            r#"
                #[ink::xyz]
                mod flipper {
                }
                "#,
        );

        let results = ensure_no_unknown_ink_attributes(&file);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Warning);
    }

    #[test]
    fn unknown_ink_multi_segment_path_attribute_fails() {
        let file = InkFile::parse(
            r#"
                #[ink::abc::xyz]
                mod flipper {
                }
                "#,
        );

        let results = ensure_no_unknown_ink_attributes(&file);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Warning);
    }

    #[test]
    fn unknown_ink_arg_attribute_fails() {
        let file = InkFile::parse(
            r#"
                #[ink(xyz)]
                struct Flipper {
                }
                "#,
        );

        let results = ensure_no_unknown_ink_attributes(&file);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Warning);
    }
}
