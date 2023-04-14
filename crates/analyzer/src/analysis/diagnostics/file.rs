//! ink! file level diagnostics.

use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::{FromSyntax, InkFile};

use super::utils;
use crate::{Diagnostic, Severity};

/// Runs ink! file level diagnostics.
pub fn diagnostics(file: &InkFile) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Runs generic diagnostics `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(file));

    // Ensure exactly one ink! contract, See `ensure_contract_quantity`.
    if let Some(diagnostic) = ensure_contract_quantity(file) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    results
}

/// Ensures that an ink! contract is not missing and that there are not multiple ink! contract definitions.
///
/// Multiple ink! contract definitions in a single file generate conflicting metadata definitions.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/codegen/src/generator/metadata.rs#L51>.
fn ensure_contract_quantity(file: &InkFile) -> Option<Diagnostic> {
    let num_contracts = file.contracts().len();
    if num_contracts == 0 {
        return Some(Diagnostic {
            message: "Missing ink! contract definition".to_string(),
            range: file.syntax().text_range(),
            severity: Severity::Error,
        });
    } else if num_contracts > 1 {
        let mut text_range = file.contracts()[1].syntax().text_range();
        if num_contracts > 2 {
            text_range = TextRange::new(
                text_range.start(),
                file.contracts().last().unwrap().syntax().text_range().end(),
            );
        }
        return Some(Diagnostic {
            message: "Only one ink! contract per file is currently supported.".to_string(),
            range: text_range,
            severity: Severity::Warning,
        });
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::InkFile;

    #[test]
    fn one_contract_definition_works() {
        let file = InkFile::parse(
            r#"
            #[ink::contract]
            mod flipper {
            }
            "#,
        );

        let result = ensure_contract_quantity(&file);
        assert!(result.is_none());
    }

    #[test]
    fn no_contract_definitions_fails() {
        let file = InkFile::parse("");

        let result = ensure_contract_quantity(&file);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn multiple_contract_definitions_fails() {
        // Tests snippets with btn 2 and 5 contract definitions.
        for idx in 2..=5 {
            // Creates code snippets with multiple contract definitions
            let code = (1..=idx)
                .map(|i| {
                    format!(
                        r#"
                    #[ink::contract]
                    mod flipper{} {{
                    }}
                    "#,
                        i
                    )
                })
                .collect::<Vec<String>>()
                .join("");

            let file = InkFile::parse(&code);

            let result = ensure_contract_quantity(&file);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Warning);
        }
    }
}
