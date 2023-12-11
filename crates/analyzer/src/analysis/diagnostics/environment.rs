//! ink! environment config diagnostics.

use ink_analyzer_ir::ast::HasName;
use ink_analyzer_ir::meta::MetaValue;
use ink_analyzer_ir::{Environment, HasInkEnvironment};

use super::utils;
use crate::codegen::snippets::{ENVIRONMENT_IMPL_PLAIN, ENVIRONMENT_IMPL_SNIPPET};
use crate::{resolution, Action, ActionKind, Diagnostic, Severity, TextEdit};

const INK_ENV_QUALIFIERS: [&str; 2] = ["ink::env", "ink_env"];

/// Runs all ink! environment diagnostics.
pub fn diagnostics<T>(results: &mut Vec<Diagnostic>, item: &T)
where
    T: HasInkEnvironment,
{
    // Ensures that ink! environment argument value can be resolved, see `ensure_resolvable` doc.
    if let Some(diagnostic) = ensure_resolvable(item) {
        results.push(diagnostic);
    }

    // Ensures that ink! environment item satisfies the required trait bounds,
    // see `ensure_impl_environment` doc.
    if let Some(diagnostic) = ensure_impl_environment(item) {
        results.push(diagnostic);
    }
}

// Ensures that the ink! environment argument value can be resolved to an ADT item (i.e. struct, enum or union).
fn ensure_resolvable<T>(item: &T) -> Option<Diagnostic>
where
    T: HasInkEnvironment,
{
    // Only continue if there's a `path` environment arg.
    let env_arg = item.env_arg()?;
    let env_path = env_arg.as_path_with_inaccurate_text_range()?;
    match item.environment() {
        // Handles no resolved environment.
        None => {
            // Environment argument name.
            let arg_name = env_arg.arg().meta().name().to_string();

            // Determines text range for the argument value.
            let range = env_arg
                .arg()
                .value()
                .map(MetaValue::text_range)
                .unwrap_or(env_arg.text_range());

            let is_default_env = resolution::is_external_crate_item(
                "DefaultEnvironment",
                &env_path,
                &INK_ENV_QUALIFIERS,
                item.syntax(),
            );
            (!is_default_env).then_some(Diagnostic {
                message: format!(
                    "`{arg_name}` argument value should be a path to a custom type \
                    that implements the `ink::env::Environment` trait."
                ),
                range,
                severity: Severity::Error,
                quickfixes: resolution::resolve_or_find_adt_by_external_trait_impl(
                    &env_path,
                    item.syntax(),
                    "Environment",
                    &INK_ENV_QUALIFIERS,
                )
                .as_ref()
                .and_then(resolution::item_path)
                .map(|candidate_path| {
                    // Suggests a resolved path.
                    vec![Action {
                        label: format!(
                            "Replace `{arg_name}` argument value with `{candidate_path}`"
                        ),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::replace_with_snippet(
                            format!("{arg_name} = {candidate_path}"),
                            env_arg.text_range(),
                            Some(format!("{arg_name} = ${{1:{candidate_path}}}")),
                        )],
                    }]
                }),
            })
        }
        // Ignores resolved environment config.
        Some(_) => None,
    }
}

// Ensures that the ink! environment ADT item (i.e. struct, enum or union) implements the `ink::env::Environment` trait.
fn ensure_impl_environment<T>(item: &T) -> Option<Diagnostic>
where
    T: HasInkEnvironment,
{
    // Only continue if there's a named environment ADT.
    let adt = item.environment().as_ref().map(Environment::adt).cloned()?;
    let name = adt.name()?.to_string();

    utils::ensure_external_trait_impl(
        &adt,
        (
            "Environment",
            &INK_ENV_QUALIFIERS,
            &item.syntax().ancestors().last()?,
        ),
        "Environment values must implement the `ink::env::Environment` trait".to_string(),
        format!("Add `ink::env::Environment` implementation for {name}"),
        ENVIRONMENT_IMPL_PLAIN.replace("MyEnvironment", &name),
        Some(ENVIRONMENT_IMPL_SNIPPET.replace("MyEnvironment", &name)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{parse_first_ink_entity_of_type, verify_actions};
    use ink_analyzer_ir::{Contract, InkE2ETest};
    use quote::quote;
    use test_utils::{
        quote_as_pretty_string, quote_as_string, TestResultAction, TestResultTextRange,
    };

    // Custom environment definition.
    macro_rules! custom_env {
        () => {
            quote! {
                #[derive(Clone)]
                pub struct MyEnvironment;

                impl ink::env::Environment for MyEnvironment {
                    const MAX_EVENT_TOPICS: usize = 3;
                    type AccountId = [u8; 16];
                    type Balance = u128;
                    type Hash = [u8; 32];
                    type Timestamp = u64;
                    type BlockNumber = u32;
                    type ChainExtension = ::ink::env::NoChainExtension;
                }
            }
        };
    }

    // Contract definition.
    macro_rules! contract_item {
        () => {
            quote! {
                mod my_contract {
                    // --snip--
                }
            }
        };
    }

    // Contract macro name.
    macro_rules! contract_macro_name {
        () => {
            quote! { ink::contract }
        };
    }

    // E2E test definition.
    macro_rules! e2e_test_item {
        () => {
            quote! {
                async fn it_works(mut client: ::ink_e2e::Client<C,E>) ->
                    std::result::Result<T, Box<dyn std::error::Error>> {
                    // --snip--
                }
            }
        };
    }

    // E2E test macro name.
    macro_rules! e2e_test_macro_name {
        () => {
            quote! { ink_e2e::test }
        };
    }

    macro_rules! valid_envs {
        () => {
            [
                // No environment argument (i.e. default).
                (
                    quote! {},
                    quote! {},
                    contract_macro_name!(),
                    contract_item!(),
                ),
                (
                    quote! {},
                    quote! {},
                    e2e_test_macro_name!(),
                    e2e_test_item!(),
                ),
                // Explicit default environment (for re-export).
                (
                    quote! {},
                    quote! { (env = ink::env::DefaultEnvironment) },
                    contract_macro_name!(),
                    contract_item!(),
                ),
                (
                    quote! {},
                    quote! { (environment = ink::env::DefaultEnvironment) },
                    e2e_test_macro_name!(),
                    e2e_test_item!(),
                ),
                // Explicit default environment (from `ink_env` crate).
                (
                    quote! {},
                    quote! { (env = ink_env::DefaultEnvironment) },
                    contract_macro_name!(),
                    contract_item!(),
                ),
                (
                    quote! {},
                    quote! { (environment = ink_env::DefaultEnvironment) },
                    e2e_test_macro_name!(),
                    e2e_test_item!(),
                ),
                // Custom environment.
                (
                    custom_env!(),
                    quote! { (env = crate::MyEnvironment) },
                    contract_macro_name!(),
                    contract_item!(),
                ),
                (
                    custom_env!(),
                    quote! { (environment = crate::MyEnvironment) },
                    e2e_test_macro_name!(),
                    e2e_test_item!(),
                ),
            ]
        };
    }

    macro_rules! run_diagnostic {
        ($call: ident, $source: ident, $macro_name: ident) => {
            if $macro_name.to_string().contains("contract") {
                let item: Contract = parse_first_ink_entity_of_type(&$source);
                $call(&item)
            } else {
                let item: InkE2ETest = parse_first_ink_entity_of_type(&$source);
                $call(&item)
            }
        };
    }

    #[test]
    fn resolvable_and_default_env_works() {
        for (env, env_arg, macro_name, item) in valid_envs!() {
            let code = quote_as_string! {
                #[#macro_name #env_arg]
                #item

                #env
            };

            let result = run_diagnostic!(ensure_resolvable, code, macro_name);
            assert!(result.is_none(), "item: {code}");
        }
    }

    #[test]
    fn unresolvable_env_fails() {
        for (env, env_arg, macro_name, item, expected_quickfixes) in [
            // Wrong path to existing environment.
            (
                custom_env!(),
                quote! { (env = self::MyEnvironment) },
                contract_macro_name!(),
                contract_item!(),
                vec![TestResultAction {
                    label: "Replace `env`",
                    edits: vec![TestResultTextRange {
                        text: "env = crate::MyEnvironment",
                        start_pat: Some("<-env = self::MyEnvironment"),
                        end_pat: Some("env = self::MyEnvironment"),
                    }],
                }],
            ),
            (
                custom_env!(),
                quote! { (environment = super::MyEnvironment) },
                e2e_test_macro_name!(),
                e2e_test_item!(),
                vec![TestResultAction {
                    label: "Replace `environment`",
                    edits: vec![TestResultTextRange {
                        text: "environment = crate::MyEnvironment",
                        start_pat: Some("<-environment = super::MyEnvironment"),
                        end_pat: Some("environment = super::MyEnvironment"),
                    }],
                }],
            ),
            // Non-existent environment (with no local environment definition).
            (
                quote! {},
                quote! { (env = crate::MyEnvironment) },
                contract_macro_name!(),
                contract_item!(),
                vec![],
            ),
            (
                quote! {},
                quote! { (environment = crate::MyEnvironment) },
                e2e_test_macro_name!(),
                e2e_test_item!(),
                vec![],
            ),
            // Non-existent environment (with local custom environment definition).
            (
                custom_env!(),
                quote! { (env = crate::NoEnvironment) },
                contract_macro_name!(),
                contract_item!(),
                vec![TestResultAction {
                    label: "Replace `env`",
                    edits: vec![TestResultTextRange {
                        text: "env = crate::MyEnvironment",
                        start_pat: Some("<-env = crate::NoEnvironment"),
                        end_pat: Some("env = crate::NoEnvironment"),
                    }],
                }],
            ),
            (
                custom_env!(),
                quote! { (environment = crate::NoEnvironment) },
                e2e_test_macro_name!(),
                e2e_test_item!(),
                vec![TestResultAction {
                    label: "Replace `environment`",
                    edits: vec![TestResultTextRange {
                        text: "environment = crate::MyEnvironment",
                        start_pat: Some("<-environment = crate::NoEnvironment"),
                        end_pat: Some("environment = crate::NoEnvironment"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[#macro_name #env_arg]
                #item

                #env
            };

            let result = run_diagnostic!(ensure_resolvable, code, macro_name);

            // Verifies diagnostics.
            assert!(result.is_some(), "item: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "item: {code}"
            );
            // Verifies quickfixes.
            let empty = Vec::new();
            let quickfixes = result
                .as_ref()
                .unwrap()
                .quickfixes
                .as_ref()
                .unwrap_or(&empty);
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn env_impl_environment_and_default_works() {
        for (env, env_arg, macro_name, item) in valid_envs!() {
            let code = quote_as_string! {
                #[#macro_name #env_arg]
                #item

                #env
            };

            let result = run_diagnostic!(ensure_impl_environment, code, macro_name);
            assert!(result.is_none(), "item: {code}");
        }
    }

    #[test]
    fn env_no_impl_environment_fails() {
        for (env, env_arg, macro_name, item, expected_quickfixes) in [
            (
                quote! {
                    #[derive(Clone)]
                    pub struct MyEnvironment;
                },
                quote! { (env = crate::MyEnvironment) },
                contract_macro_name!(),
                contract_item!(),
                vec![TestResultAction {
                    label: "Add `ink::env::Environment`",
                    edits: vec![TestResultTextRange {
                        text: "impl ink::env::Environment for ",
                        start_pat: Some("pub struct MyEnvironment;"),
                        end_pat: Some("pub struct MyEnvironment;"),
                    }],
                }],
            ),
            (
                quote! {
                    #[derive(Clone)]
                    pub struct MyEnvironment;
                },
                quote! { (environment = crate::MyEnvironment) },
                e2e_test_macro_name!(),
                e2e_test_item!(),
                vec![TestResultAction {
                    label: "Add `ink::env::Environment`",
                    edits: vec![TestResultTextRange {
                        text: "impl ink::env::Environment for ",
                        start_pat: Some("pub struct MyEnvironment;"),
                        end_pat: Some("pub struct MyEnvironment;"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[#macro_name #env_arg]
                #item

                #env
            };

            let result = run_diagnostic!(ensure_impl_environment, code, macro_name);

            // Verifies diagnostics.
            assert!(result.is_some(), "item: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "item: {code}"
            );
            // Verifies quickfixes.
            let empty = Vec::new();
            let quickfixes = result
                .as_ref()
                .unwrap()
                .quickfixes
                .as_ref()
                .unwrap_or(&empty);
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }
}
