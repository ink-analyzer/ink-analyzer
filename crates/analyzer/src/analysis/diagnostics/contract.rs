//! ink! contract diagnostics.

use ink_analyzer_ir::{
    Contract, FromInkAttribute, FromSyntax, IRItem, InkAttributeKind, InkPathKind,
};

use super::utils;
use crate::{Diagnostic, Severity};

/// Runs all ink! contract diagnostics.
pub fn diagnostics(contract: &Contract) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Runs generic diagnostics `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(contract));

    // Ensure ink! contract is an inline module, See `ensure_inline_module`.
    if let Some(diagnostic) = ensure_inline_module(contract) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure no nested ink! contracts, See `ensure_no_nested_contracts`.
    utils::append_diagnostics(&mut results, &mut ensure_no_nested_contracts(contract));

    results
}

/// Ensures ink! contract attribute is applied to an inline module.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L301-L309>.
fn ensure_inline_module(contract: &Contract) -> Option<Diagnostic> {
    let ink_attr = contract.ink_attr();
    let mut error = None;

    if let Some(module) = contract.module() {
        if module.item_list().is_none() {
            error = Some(format!(
                "The content of the mod annotated by `{}` should be defined inline.",
                ink_attr.syntax()
            ));
        }
    } else {
        error = Some(format!(
            "`{}` can only be applied to an inline mod",
            ink_attr.syntax()
        ));
    }

    error.map(|message| Diagnostic {
        message,
        range: ink_attr.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensures that an ink! contract doesn't include other nested contract definitions.
///
/// Multiple ink! contract definitions in a single file generate conflicting metadata definitions.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/codegen/src/generator/metadata.rs#L51>.
fn ensure_no_nested_contracts(contract: &Contract) -> Vec<Diagnostic> {
    contract
        .ink_attrs_descendants()
        .iter()
        .filter_map(|attr| {
            matches!(attr.kind(), InkAttributeKind::Path(InkPathKind::Contract)).then_some(
                Diagnostic {
                    message: format!(
                        "Nested ink! contracts aren't currently supported: {}",
                        attr.syntax()
                    ),
                    range: attr.syntax().text_range(),
                    severity: Severity::Error,
                },
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::InkFile;

    fn parse_first_contract(code: &str) -> Contract {
        InkFile::parse(code).contracts().to_owned()[0].to_owned()
    }

    #[test]
    fn contract_attribute_on_inline_mod_works() {
        let contract = parse_first_contract(
            r#"
                #[ink::contract]
                mod flipper {
                }
                "#,
        );

        let result = ensure_inline_module(&contract);
        assert!(result.is_none());
    }

    #[test]
    fn contract_attribute_with_args_on_inline_mod_works() {
        let contract = parse_first_contract(
            r#"
                #[ink::contract(keep_attr="foo, bar")]
                mod flipper {
                    // #[foo]
                    // #[bar]
                }
                "#,
        );

        let result = ensure_inline_module(&contract);
        assert!(result.is_none());
    }

    #[test]
    fn contract_attribute_on_out_of_line_mod_fails() {
        let contract = parse_first_contract(
            r#"
                #[ink::contract]
                mod flipper;
                "#,
        );

        let result = ensure_inline_module(&contract);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn contract_attribute_on_anything_other_than_mod_fails() {
        let results: Vec<Diagnostic> = (vec![
            r#"
            #[ink::contract]
            fn flipper() {
            }
            "#,
            r#"
            #[ink::contract]
            struct Flipper;
            "#,
            r#"
            #[ink::contract]
            enum Flipper {
                This,
                That
            }
            "#,
        ])
        .into_iter()
        .map(parse_first_contract)
        .filter_map(|contract| ensure_inline_module(&contract))
        .collect();

        assert_eq!(results.len(), 3);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .into_iter()
                .filter(|item| item.severity == Severity::Error)
                .collect::<Vec<Diagnostic>>()
                .len(),
            3
        );
    }

    #[test]
    fn contract_attribute_in_mod_body_fails() {
        let contract = parse_first_contract(
            r#"
                mod flipper {
                    #[ink::contract]
                }
                "#,
        );

        let result = ensure_inline_module(&contract);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn nested_contract_definition_fails() {
        let contract = parse_first_contract(
            r#"
            #[ink::contract]
            mod flipper {
                #[ink::contract]
                mod flipper2 {
                }
            }
            "#,
        );

        let results = ensure_no_nested_contracts(&contract);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
    }
}
