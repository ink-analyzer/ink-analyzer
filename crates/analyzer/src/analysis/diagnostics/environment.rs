//! ink! environment config diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasName};
use ink_analyzer_ir::meta::MetaValue;
use ink_analyzer_ir::syntax::SyntaxNode;
use ink_analyzer_ir::{ast, Environment, HasInkEnvironment};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::iter;

use crate::analysis::utils as analysis_utils;
use crate::codegen::snippets::{ENVIRONMENT_IMPL_PLAIN, ENVIRONMENT_IMPL_SNIPPET};
use crate::{Action, ActionKind, Diagnostic, Severity, TextEdit};

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

// Ensures that the ink! environment argument value can be resolved to an ADT item
// (i.e. struct, enum or union).
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

            let is_default_env = is_ink_env_target("DefaultEnvironment", &env_path, item.syntax());
            (!is_default_env).then_some(Diagnostic {
                message: format!(
                    "`{arg_name}` argument value should be a path to a custom type \
                    that implements the `ink::env::Environment` trait."
                ),
                range,
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
                    .or(
                        // Finds an `inv::env::Environment` implementation.
                        find_ink_env_impl(item, None)
                            // Returns the custom type name for `inv::env::Environment` implementation.
                            .as_ref()
                            .and_then(ast::Impl::self_ty)
                            .as_ref()
                            .and_then(path_from_type)
                            .as_ref()
                            .and_then(ast::Path::segment)
                            .as_ref()
                            .and_then(ast::PathSegment::name_ref)
                            .as_ref()
                            .and_then(find_adt_by_name),
                    )
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
                                range,
                                edits: vec![TextEdit::replace_with_snippet(
                                    format!("{arg_name} = {candidate_path}"),
                                    env_arg.text_range(),
                                    Some(format!("{arg_name} = ${{1:{candidate_path}}}")),
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

// Ensures that the ink! environment ADT item (i.e. struct, enum or union) implements
// the `ink::env::Environment` trait.
fn ensure_impl_environment<T>(item: &T) -> Option<Diagnostic>
where
    T: HasInkEnvironment,
{
    // Only continue if there's a named environment ADT.
    let adt = item.environment().as_ref().map(Environment::adt).cloned()?;
    let name = adt.name()?;

    // Finds environment implementation (if any).
    match find_ink_env_impl(item, Some(&name.to_string())) {
        // Handles no environment implementation.
        None => {
            let item = match adt.clone() {
                ast::Adt::Enum(it) => ast::Item::Enum(it),
                ast::Adt::Struct(it) => ast::Item::Struct(it),
                ast::Adt::Union(it) => ast::Item::Union(it),
            };
            let range = analysis_utils::ast_item_declaration_range(&item)
                .unwrap_or(adt.syntax().text_range());
            let indent_option = analysis_utils::item_indenting(adt.syntax());
            let env_impl_plain = ENVIRONMENT_IMPL_PLAIN.replace("MyEnvironment", &name.to_string());
            let env_impl_snippet =
                ENVIRONMENT_IMPL_SNIPPET.replace("MyEnvironment", &name.to_string());

            Some(Diagnostic {
                message: "Environment values must implement the `ink::env::Environment` trait"
                    .to_string(),
                range,
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: format!("Add `ink::env::Environment` implementation for {name}"),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::insert_with_snippet(
                        indent_option
                            .as_ref()
                            .map(|indent| analysis_utils::apply_indenting(&env_impl_plain, indent))
                            .unwrap_or(env_impl_plain),
                        adt.syntax().text_range().end(),
                        Some(
                            indent_option
                                .as_ref()
                                .map(|indent| {
                                    analysis_utils::apply_indenting(&env_impl_snippet, indent)
                                })
                                .unwrap_or(env_impl_snippet),
                        ),
                    )],
                }]),
            })
        }
        // Ignores resolved environment implementation.
        Some(_) => None,
    }
}

// Finds first `inv::env::Environment` implementation (optionally for a target name).
fn find_ink_env_impl<T>(item: &T, name_option: Option<&str>) -> Option<ast::Impl>
where
    T: HasInkEnvironment,
{
    item.syntax().ancestors().last().and_then(|root_node| {
        root_node
            .descendants()
            .filter(|it| ast::Impl::can_cast(it.kind()))
            .find_map(|node| {
                ast::Impl::cast(node).filter(|impl_item| {
                    let is_env_impl = impl_item
                        .trait_()
                        .as_ref()
                        .and_then(path_from_type)
                        .map_or(false, |path| {
                            is_ink_env_target("Environment", &path, impl_item.syntax())
                        });
                    let is_target_name = name_option
                        .zip(impl_item.self_ty().as_ref().and_then(path_from_type))
                        .map_or(false, |(name, path)| is_path_target(name, &path));

                    is_env_impl && (name_option.is_none() || is_target_name)
                })
            })
    })
}

// Checks that an item referenced by `name` from the `ink::env` (or `ink_env`) crate (or any of its aliases) is the target of `path`.
fn is_ink_env_target(name: &str, path: &ast::Path, ref_node: &SyntaxNode) -> bool {
    let name_is_path_target = is_path_target(name, path);

    // Checks if path's qualifier matches one of the specified qualifiers.
    // Matches exactly when strict is true, otherwise adds fully qualified variants
    // See `make_qualifiers_exhaustive` doc above.
    let path_has_qualifier = |qualifiers: &[&str], strict: bool| {
        path.qualifier().map_or(false, |qualifier| {
            let qualifier = qualifier.to_string().replace(' ', "");
            if strict {
                qualifiers.contains(&qualifier.as_str())
            } else {
                make_qualifiers_exhaustive(qualifiers).contains(&qualifier)
            }
        })
    };

    let name_qualifiers = ["ink::env", "ink_env"];

    // Checks `name` or any of its aliases is the target of `path` (including scope considerations).
    (name_is_path_target && path_has_qualifier(&name_qualifiers, false))
        || ink_analyzer_ir::resolve_current_module(ref_node).map_or(false, |root_node| {
            let (use_paths, mut use_aliases) = ink_env_uses_and_aliases_in_scope(&root_node);

            // Reverse alias map to `use_path -> alias`.
            use_aliases = use_aliases
                .into_iter()
                .map(|(alias, use_path)| (use_path, alias))
                .collect();

            // Checks whether an item with given qualifiers is in scope.
            let is_item_in_scope = |item_name: &str, qualifiers: &[&str]| {
                make_qualifiers_exhaustive(qualifiers)
                    .into_iter()
                    .flat_map(|prefix| [format!("{prefix}::{item_name}"), format!("{prefix}::*")])
                    .any(|use_path| use_paths.contains(&use_path))
            };

            macro_rules! item_aliases {
                ($name: expr, $qualifiers: expr) => {
                    make_qualifiers_exhaustive($qualifiers)
                        .into_iter()
                        .flat_map(|prefix| [format!("{prefix}::{}", $name), format!("{prefix}::*")])
                        .filter_map(|use_path| use_aliases.get(&use_path))
                };
            }

            // Checks for scope and aliased name.
            let unqualified_target_name_in_scope =
                || path.qualifier().is_none() && is_item_in_scope(name, &name_qualifiers);
            let target_is_name_alias = || {
                path.qualifier().is_none()
                    && item_aliases!(name, &name_qualifiers)
                        .any(|alias| is_path_target(alias, path))
            };
            let sub_qualifier_or_alias_in_scope = || {
                name_qualifiers.iter().any(|qualifier| {
                    let qualifiers: Vec<_> = qualifier.split("::").collect();
                    let n_qualifiers = qualifiers.len();
                    (0..n_qualifiers).any(|idx| {
                        let anchor_name = qualifiers[idx];
                        let pre_anchor_qualifier = qualifiers[0..idx].join("::");
                        let post_anchor_qualifier = if idx < n_qualifiers {
                            let post_anchor = qualifiers[idx + 1..].join("::");
                            (!post_anchor.is_empty()).then_some(post_anchor)
                        } else {
                            None
                        };

                        let is_top_qualifier = idx == 0;
                        let anchor_and_post_qualifier = |alias: &str| {
                            format!(
                                "{alias}{}{}",
                                if post_anchor_qualifier
                                    .as_ref()
                                    .map_or(false, |it| !it.is_empty())
                                {
                                    "::"
                                } else {
                                    ""
                                },
                                post_anchor_qualifier.as_deref().unwrap_or("")
                            )
                        };
                        let anchor_qualifier_in_scope = || {
                            path_has_qualifier(
                                &[anchor_and_post_qualifier(anchor_name).as_str()],
                                !is_top_qualifier,
                            ) && (is_top_qualifier
                                || is_item_in_scope(anchor_name, &[pre_anchor_qualifier.as_str()]))
                        };
                        let anchor_alias_qualifier_in_scope = || {
                            let anchor_aliases: Vec<_> = if is_top_qualifier {
                                make_qualifiers_exhaustive(&[anchor_name])
                                    .into_iter()
                                    .filter_map(|use_path| use_aliases.get(&use_path))
                                    .map(|alias| anchor_and_post_qualifier(alias))
                                    .collect()
                            } else {
                                item_aliases!(anchor_name, &[pre_anchor_qualifier.as_str()])
                                    .map(|alias| anchor_and_post_qualifier(alias))
                                    .collect()
                            };

                            path_has_qualifier(
                                &anchor_aliases
                                    .iter()
                                    .map(|it| it.as_str())
                                    .collect::<Vec<_>>(),
                                !is_top_qualifier,
                            )
                        };

                        anchor_qualifier_in_scope() || anchor_alias_qualifier_in_scope()
                    })
                })
            };

            (name_is_path_target
                && (unqualified_target_name_in_scope() || sub_qualifier_or_alias_in_scope()))
                || target_is_name_alias()
        })
}

// Checks if an item named `name` is the target of the `path`.
fn is_path_target(name: &str, path: &ast::Path) -> bool {
    path.segment()
        .as_ref()
        .and_then(ast::PathSegment::name_ref)
        .map_or(false, |name_ref| name_ref.to_string() == name)
}

// Converts a type to a path (if possible).
fn path_from_type(item_type: &ast::Type) -> Option<ast::Path> {
    match item_type {
        ast::Type::PathType(path_type) => path_type.path(),
        _ => None,
    }
}

// Add fully qualified variants to paths (e.g. [`ink`] becomes [`ink`, `::ink`]).
fn make_qualifiers_exhaustive(paths: &[&str]) -> Vec<String> {
    paths
        .iter()
        .flat_map(|qualifier| [format!("::{qualifier}"), String::from(*qualifier)])
        .collect()
}

// Collects `ink` and `ink_env` use paths and aliases in the current scope.
fn ink_env_uses_and_aliases_in_scope(
    root_node: &SyntaxNode,
) -> (HashSet<String>, HashMap<String, String>) {
    let (mut use_paths, mut use_aliases) =
        ink_analyzer_ir::simple_use_paths_and_aliases_in_scope(root_node);

    while let Some(use_path_str) = use_paths.iter().find_map(|use_path| {
        (!path_str_is_ink_or_has_ink_qualifier(use_path)).then_some(use_path.to_string())
    }) {
        // Removes path.
        use_paths.remove(&use_path_str);

        // Resolves path if it points to an `ink` or `ink_crate` use declaration.
        let result = resolve_non_ink_path(&use_path_str, root_node);
        use_paths.extend(result.0);
        use_aliases.extend(result.1);
    }

    while let Some((alias, use_path_str)) = use_aliases.iter().find_map(|(alias, use_path)| {
        (!path_str_is_ink_or_has_ink_qualifier(use_path))
            .then_some((alias.to_string(), use_path.to_string()))
    }) {
        // Removes alias.
        use_aliases.remove(&alias);

        // Resolves path if it points to an `ink` or `ink_crate` use declaration.
        if let Some(target) = ink_analyzer_ir::path_from_str(&use_path_str)
            .as_ref()
            .and_then(ast::Path::segment)
        {
            let result = resolve_non_ink_path(&use_path_str, root_node);
            if let Some(resolved_use_path) =
                result.0.iter().next().or(result.1.get(&target.to_string()))
            {
                use_aliases.insert(alias, resolved_use_path.to_string());
            }
        }
    }

    fn path_str_is_ink_or_has_ink_qualifier(path_str: &str) -> bool {
        make_qualifiers_exhaustive(&["ink::", "ink_env::"])
            .iter()
            .any(|qualifier| path_str.starts_with(qualifier))
            || make_qualifiers_exhaustive(&["ink", "ink_env"])
                .iter()
                .any(|qualifier| path_str == qualifier)
    }

    fn resolve_non_ink_path(
        use_path_str: &str,
        ref_node: &SyntaxNode,
    ) -> (HashSet<String>, HashMap<String, String>) {
        let mut use_paths = HashSet::new();
        let mut use_aliases = HashMap::new();
        let use_path_option = ink_analyzer_ir::path_from_str(use_path_str).and_then(|path| {
            path.qualifier().and_then(|qualifier| {
                let target_name_option = path
                    .segment()
                    .as_ref()
                    .and_then(ast::PathSegment::name_ref)
                    .as_ref()
                    .map(ToString::to_string)
                    .or((path.to_string().replace(' ', "")
                        == format!("{}::*", qualifier.to_string().replace(' ', "")))
                    .then_some(String::from("*")));

                target_name_option.zip(ink_analyzer_ir::resolve_qualifier(
                    &qualifier,
                    ref_node,
                    path.segment().as_ref(),
                ))
            })
        });
        if let Some((target_name, qualifier_ref_node)) = use_path_option {
            if target_name == "*" {
                let result = ink_env_uses_and_aliases_in_scope(&qualifier_ref_node);
                use_paths.extend(result.0);
                use_aliases.extend(result.1);
            } else {
                for path in ink_analyzer_ir::resolve_item_path_from_use_scope!(
                    target_name,
                    &qualifier_ref_node
                ) {
                    let resolved_path_str = path.to_string().replace(' ', "");
                    if path_str_is_ink_or_has_ink_qualifier(&resolved_path_str) {
                        if resolved_path_str.ends_with(&format!("::{target_name}")) {
                            use_paths.insert(resolved_path_str.clone());
                        } else {
                            use_aliases.insert(target_name.clone(), resolved_path_str);
                        }
                    } else {
                        let result = resolve_non_ink_path(&resolved_path_str, &qualifier_ref_node);
                        use_paths.extend(result.0);
                        use_aliases.extend(result.1);
                    }
                }
            }
        }

        (use_paths, use_aliases)
    }

    (use_paths, use_aliases)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{
        parse_first_ast_node_of_type, parse_first_ink_entity_of_type, verify_actions,
    };
    use ink_analyzer_ir::ast::SourceFile;
    use ink_analyzer_ir::{Contract, InkE2ETest, InkEntity, InkFile};
    use quote::quote;
    use test_utils::{
        quote_as_pretty_string, quote_as_str, quote_as_string, TestResultAction,
        TestResultTextRange,
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
            // Wrong path to existing environment.
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

    #[test]
    fn valid_ink_env_target_works() {
        let ref_name = quote! { ref_node };
        for (code, path_str) in [
            // Simple paths.
            ("", "ink::env::Environment"),
            ("", "::ink::env::Environment"),
            ("", "ink_env::Environment"),
            ("", "::ink_env::Environment"),
            // Scoped paths.
            ("use ink::env::Environment;", "Environment"),
            ("use ink::env::*", "Environment;"),
            ("use ink::{env::Environment, primitives};", "Environment"),
            (
                "use ink::{env::{Environment, DefaultEnvironment}, primitives};",
                "Environment",
            ),
            ("use ink::env", "env::Environment;"),
            ("use ink::{env, primitives};", "env::Environment"),
            (
                quote_as_str! {
                    use ink::env::Environment;

                    mod #ref_name {
                        use super::Environment;
                    }
                },
                "Environment",
            ),
            (
                quote_as_str! {
                    use ink::env::Environment;

                    mod #ref_name {
                        use super::*;
                    }
                },
                "Environment",
            ),
            // Aliased paths.
            (
                "use ink::env::Environment as ChainEnvironment;",
                "ChainEnvironment",
            ),
            (
                "use ink::{env::Environment as ChainEnvironment, primitives};",
                "ChainEnvironment",
            ),
            ("use ink::env as chain_env;", "chain_env::Environment"),
            (
                "use ink::{env as chain_env, primitives};",
                "chain_env::Environment",
            ),
            ("use ink as ink_lang;", "ink_lang::env::Environment"),
            ("use ink_env as ink_lang_env;", "ink_lang_env::Environment"),
            (
                quote_as_str! {
                    use ink::env::Environment as ChainEnvironment;

                    mod #ref_name {
                        use super::ChainEnvironment;
                    }
                },
                "ChainEnvironment",
            ),
            (
                quote_as_str! {
                    use ink::env::Environment as ChainEnvironment;

                    mod #ref_name {
                        use super::*;
                    }
                },
                "ChainEnvironment",
            ),
            (
                quote_as_str! {
                    use ink::env::Environment as InkEnvironment;

                    mod #ref_name {
                        use super::InkEnvironment as ChainEnvironment;
                    }
                },
                "ChainEnvironment",
            ),
        ] {
            let file = InkFile::parse(code);
            let path: ast::Path = parse_first_ast_node_of_type(path_str);
            let ref_module_option = SourceFile::parse(code)
                .tree()
                .syntax()
                .descendants()
                .find_map(|node| {
                    ast::Module::cast(node).filter(|item| {
                        item.name()
                            .map_or(false, |name| name.to_string() == ref_name.to_string())
                    })
                });

            assert!(
                is_ink_env_target(
                    "Environment",
                    &path,
                    ref_module_option
                        .as_ref()
                        .map(AstNode::syntax)
                        .unwrap_or(file.syntax())
                ),
                "code: {code} | path: {path_str}"
            );
        }
    }
}
