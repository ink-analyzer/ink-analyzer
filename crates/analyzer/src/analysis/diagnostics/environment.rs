//! ink! environment config diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasName};
use ink_analyzer_ir::{ast, HasInkEnvironment, InkEntity};
use itertools::Itertools;
use std::iter;

use crate::{Action, ActionKind, Diagnostic, Severity, TextEdit};

/// Runs all ink! environment diagnostics.
pub fn diagnostics<T>(results: &mut Vec<Diagnostic>, item: &T)
where
    T: InkEntity + HasInkEnvironment,
{
    // Ensures that ink! environment argument value can be resolved, see `ensure_resolvable` doc.
    if let Some(diagnostic) = ensure_resolvable(item) {
        results.push(diagnostic);
    }
}

// Ensures that the ink! environment argument can be resolved to an ADT item
// (i.e. struct, enum or union).
fn ensure_resolvable<T>(item: &T) -> Option<Diagnostic>
where
    T: InkEntity + HasInkEnvironment,
{
    // Only continue if there's a `path` environment arg and no environment was resolved.
    let env_arg = item.env_arg()?;
    let env_path = env_arg.as_path_with_inaccurate_text_range()?;
    match item.environment() {
        // Handles no environment.
        None => {
            let arg_name = env_arg.arg().meta().name().to_string();
            let is_target = |name: &str, path: &ast::Path| {
                path.segment()
                    .as_ref()
                    .and_then(ast::PathSegment::name_ref)
                    .map_or(false, |name_ref| name_ref.to_string() == name)
            };
            let has_ink_env_qualifier = |path: &ast::Path| {
                path.qualifier().map_or(false, |qualifier| {
                    let qualifier = qualifier.to_string().replace(' ', "");
                    let env_qualifiers: Vec<_> = ["ink_env", "ink::env"]
                        .into_iter()
                        .flat_map(|qualifier| [format!("::{qualifier}"), String::from(qualifier)])
                        .collect();
                    env_qualifiers.contains(&qualifier)
                })
            };
            let extract_type_path = |item_type: &ast::Type| match item_type {
                ast::Type::PathType(path_type) => path_type.path(),
                _ => None,
            };

            // Finds a struct, enum or union with the target name.
            let find_adt_by_name = |target_name: &ast::NameRef| {
                item.syntax().ancestors().last().and_then(|root_node| {
                    root_node
                        .descendants()
                        .filter(|it| ast::Adt::can_cast(it.kind()))
                        .find_map(|node| {
                            ast::Adt::cast(node).filter(|item| {
                                item.name().map_or(false, |item_name| {
                                    item_name.text() == target_name.text()
                                })
                            })
                        })
                })
            };

            let is_default_env = is_target("DefaultEnvironment", &env_path)
                && (has_ink_env_qualifier(&env_path) || env_path.qualifier().is_none());
            (!is_default_env).then_some(Diagnostic {
                message: format!(
                    "`{arg_name}` argument value should be a path to a custom type \
                that implements the `ink_env::Environment` trait."
                ),
                range: env_arg.text_range(),
                severity: Severity::Error,
                quickfixes: env_path
                    .segment()
                    .as_ref()
                    .and_then(ast::PathSegment::name_ref)
                    .as_ref()
                    // Finds a struct, enum or union with the target name.
                    .and_then(find_adt_by_name)
                    // Otherwise finds an implementation of `inv::env::Environment` (if any)
                    // to suggest as the `env` value.
                    .or(item
                        .syntax()
                        .ancestors()
                        .last()
                        .and_then(|root_node| {
                            // Finds a implementations of `inv::env::Environment`
                            // and returns the custom type name.
                            root_node
                                .descendants()
                                .filter(|it| ast::Impl::can_cast(it.kind()))
                                .find_map(|node| {
                                    ast::Impl::cast(node)
                                        .filter(|item| {
                                            item.trait_()
                                                .as_ref()
                                                .and_then(extract_type_path)
                                                .map_or(false, |path| {
                                                    is_target("Environment", &path)
                                                        && (has_ink_env_qualifier(&path)
                                                            || path.qualifier().is_none())
                                                })
                                        })
                                        .as_ref()
                                        .and_then(ast::Impl::self_ty)
                                        .as_ref()
                                        .and_then(extract_type_path)
                                        .as_ref()
                                        .and_then(ast::Path::segment)
                                        .as_ref()
                                        .and_then(ast::PathSegment::name_ref)
                                })
                        })
                        .as_ref()
                        .and_then(find_adt_by_name))
                    .and_then(|candidate| {
                        // Suggests a resolved path based one on the candidate's name.
                        candidate.name().map(|name| {
                            let candidate_path = iter::once(String::from("crate"))
                                .chain(candidate.syntax().ancestors().filter_map(|node| {
                                    ast::Module::cast(node)
                                        .as_ref()
                                        .and_then(HasName::name)
                                        .as_ref()
                                        .map(ToString::to_string)
                                }))
                                .chain(iter::once(name.to_string()))
                                .join("::");

                            vec![Action {
                                label: format!(
                                    "Replace `{arg_name}` argument value with `{candidate_path}`"
                                ),
                                kind: ActionKind::QuickFix,
                                range: env_arg.text_range(),
                                edits: vec![TextEdit::replace(
                                    format!("{arg_name} = {candidate_path}"),
                                    env_arg.text_range(),
                                )],
                            }]
                        })
                    }),
            })
        }
        // Ignores resolved environment config.
        Some(_) => None,
    }
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
        // Default environment path.
        let default_env_path_re_export = quote! { ink::env::DefaultEnvironment };
        let default_env_path_from_ink_env = quote! { ink_env::DefaultEnvironment };

        // Custom environment path.
        let custom_env_path = quote! { crate::MyEnvironment };

        for (env, env_arg, macro_name, item) in [
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
                quote! { (env = #default_env_path_re_export) },
                contract_macro_name!(),
                contract_item!(),
            ),
            (
                quote! {},
                quote! { (environment = #default_env_path_re_export) },
                e2e_test_macro_name!(),
                e2e_test_item!(),
            ),
            // Explicit default environment (from `ink_env` crate).
            (
                quote! {},
                quote! { (env = #default_env_path_from_ink_env) },
                contract_macro_name!(),
                contract_item!(),
            ),
            (
                quote! {},
                quote! { (environment = #default_env_path_from_ink_env) },
                e2e_test_macro_name!(),
                e2e_test_item!(),
            ),
            // Custom environment.
            (
                custom_env!(),
                quote! { (env = #custom_env_path) },
                contract_macro_name!(),
                contract_item!(),
            ),
            (
                custom_env!(),
                quote! { (environment = #custom_env_path) },
                e2e_test_macro_name!(),
                e2e_test_item!(),
            ),
        ] {
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
            // Non-existent environment (with no local custom environment definition).
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
}
