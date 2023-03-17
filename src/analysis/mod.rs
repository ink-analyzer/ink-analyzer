//! Types and abstractions for performing semantic analysis of ink! smart contract code.
use ra_ap_syntax::SourceFile;

use crate::analysis::diagnostics::Diagnostic;

pub mod diagnostics;

/// Analysis is the main entry point for asking for semantic information about source code.
#[derive(Debug)]
pub struct Analysis;

impl Analysis {
    /// Gets the syntax tree of the smart contract code.
    pub fn parse(&self, code: &str) -> SourceFile {
        SourceFile::parse(code).tree()
    }

    /// Computes diagnostics for the smart contract code.
    pub fn diagnostics(&self, code: &str) -> Vec<Diagnostic> {
        diagnostics::diagnostics(&self.parse(code))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::diagnostics::Severity;

    #[test]
    fn ink_contract_attribute_on_mod_works() {
        let code = r#"
        #[ink::contract]
        mod flipper {
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn ink_contract_attribute_with_args_on_mod_works() {
        let code = r#"
        #[ink::contract(keep_attr="foo, bar")]
        mod flipper {
            // #[foo]
            // #[bar]
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn ink_contract_attribute_in_mod_body_fails() {
        let code = r#"
        mod flipper {
            #[ink::contract]
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert_eq!(1, diagnostics.len());
        assert_eq!(Severity::Error, diagnostics[0].severity);
    }

    #[test]
    fn ink_contract_attribute_on_fn_fails() {
        let code = r#"
        #[ink::contract]
        fn flipper() {
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert_eq!(1, diagnostics.len());
        assert_eq!(Severity::Error, diagnostics[0].severity);
    }

    #[test]
    fn ink_contract_attribute_on_struct_fails() {
        let code = r#"
        #[ink::contract]
        struct Flipper {
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert_eq!(1, diagnostics.len());
        assert_eq!(Severity::Error, diagnostics[0].severity);
    }

    #[test]
    fn ink_unknown_path_attribute_fails() {
        let code = r#"
        #[ink::xyz]
        mod flipper {
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert_eq!(1, diagnostics.len());
        assert_eq!(Severity::Warning, diagnostics[0].severity);
    }

    #[test]
    fn ink_unknown_multi_path_attribute_fails() {
        let code = r#"
        #[ink::abc::xyz]
        mod flipper {
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert_eq!(1, diagnostics.len());
        assert_eq!(Severity::Warning, diagnostics[0].severity);
    }

    #[test]
    fn ink_unknown_arg_attribute_fails() {
        let code = r#"
        #[ink(xyz)]
        struct Flipper {
        }
        "#;

        let diagnostics = Analysis.diagnostics(code);
        assert_eq!(1, diagnostics.len());
        assert_eq!(Severity::Warning, diagnostics[0].severity);
    }
}
