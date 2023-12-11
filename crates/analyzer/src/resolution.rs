//! Utilities for item resolution.

use ink_analyzer_ir::ast;
use ink_analyzer_ir::ast::AstNode;
use ink_analyzer_ir::syntax::SyntaxNode;
use std::collections::{HashMap, HashSet};

/// Converts a type to a path (if possible).
pub fn path_from_type(item_type: &ast::Type) -> Option<ast::Path> {
    match item_type {
        ast::Type::PathType(path_type) => path_type.path(),
        _ => None,
    }
}

/// Finds first external trait implementation (optionally given an implementation name).
pub fn external_trait_impl(
    trait_name: &str,
    crate_qualifiers: &[&str],
    ref_node: &SyntaxNode,
    impl_name_option: Option<&str>,
) -> Option<ast::Impl> {
    ref_node
        .descendants()
        .filter(|it| ast::Impl::can_cast(it.kind()))
        .find_map(|node| {
            ast::Impl::cast(node).filter(|impl_item| {
                let is_trait_impl =
                    impl_item
                        .trait_()
                        .as_ref()
                        .and_then(path_from_type)
                        .map_or(false, |path| {
                            is_external_crate_item(
                                trait_name,
                                &path,
                                crate_qualifiers,
                                impl_item.syntax(),
                            )
                        });
                let is_target_name = impl_name_option
                    .zip(impl_item.self_ty().as_ref().and_then(path_from_type))
                    .map_or(false, |(impl_name, path)| is_path_target(impl_name, &path));

                is_trait_impl && (impl_name_option.is_none() || is_target_name)
            })
        })
}

/// Checks that the `path` resolves to the item `name` with one of the crate `qualifiers` (or any of their aliases).
pub fn is_external_crate_item(
    name: &str,
    path: &ast::Path,
    qualifiers: &[&str],
    ref_node: &SyntaxNode,
) -> bool {
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
                exhaustive_qualifiers(qualifiers).contains(&qualifier)
            }
        })
    };

    // Checks `name` or any of its aliases is the target of `path` (including scope considerations).
    (name_is_path_target && path_has_qualifier(qualifiers, false))
        || ink_analyzer_ir::resolve_current_module(ref_node).map_or(false, |root_node| {
            let crates: Vec<_> = qualifiers
                .iter()
                .map(|qualifier| qualifier.split("::").next().unwrap_or(qualifier))
                .collect();
            let (use_paths, mut use_aliases) =
                external_crate_uses_and_aliases_in_scope(&crates, &root_node);

            // Reverse alias map to `use_path -> alias`.
            use_aliases = use_aliases
                .into_iter()
                .map(|(alias, use_path)| (use_path, alias))
                .collect();

            // Checks whether an item with given qualifiers is in scope.
            let is_item_in_scope = |item_name: &str, qualifiers: &[&str]| {
                exhaustive_qualifiers(qualifiers)
                    .into_iter()
                    .flat_map(|prefix| [format!("{prefix}::{item_name}"), format!("{prefix}::*")])
                    .any(|use_path| use_paths.contains(&use_path))
            };

            macro_rules! item_aliases {
                ($name: expr, $qualifiers: expr) => {
                    exhaustive_qualifiers($qualifiers)
                        .into_iter()
                        .flat_map(|prefix| [format!("{prefix}::{}", $name), format!("{prefix}::*")])
                        .filter_map(|use_path| use_aliases.get(&use_path))
                };
            }

            // Checks for scope and aliased name.
            let unqualified_target_name_in_scope =
                || path.qualifier().is_none() && is_item_in_scope(name, qualifiers);
            let target_is_name_alias = || {
                path.qualifier().is_none()
                    && item_aliases!(name, qualifiers).any(|alias| is_path_target(alias, path))
            };
            let sub_qualifier_or_alias_in_scope = || {
                qualifiers.iter().any(|qualifier| {
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
                                exhaustive_qualifiers(&[anchor_name])
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

// Add fully qualified variants to paths (e.g. [`ink`] becomes [`ink`, `::ink`]).
fn exhaustive_qualifiers(paths: &[&str]) -> Vec<String> {
    paths
        .iter()
        .flat_map(|qualifier| [format!("::{qualifier}"), String::from(*qualifier)])
        .collect()
}

// Checks if the path references an item in one of the crates.
fn is_crate_item_path(path: &str, crates: &[&str]) -> bool {
    exhaustive_qualifiers(crates)
        .iter()
        .any(|qualifier| path == qualifier || path.starts_with(&format!("{qualifier}::")))
}

// Collects crate use paths and aliases in the current scope.
fn external_crate_uses_and_aliases_in_scope(
    crates: &[&str],
    ref_node: &SyntaxNode,
) -> (HashSet<String>, HashMap<String, String>) {
    let (mut use_paths, mut use_aliases) =
        ink_analyzer_ir::simple_use_paths_and_aliases_in_scope(ref_node);

    while let Some(use_path_str) = use_paths.iter().find_map(|use_path| {
        (!is_crate_item_path(use_path, crates)).then_some(use_path.to_string())
    }) {
        // Removes path.
        use_paths.remove(&use_path_str);

        // Resolves path if it points to a use declaration for one of the specified external crates.
        let result = match_path_to_external_crate_in_scope(&use_path_str, crates, ref_node);
        use_paths.extend(result.0);
        use_aliases.extend(result.1);
    }

    while let Some((alias, use_path_str)) = use_aliases.iter().find_map(|(alias, use_path)| {
        (!is_crate_item_path(use_path, crates)).then_some((alias.to_string(), use_path.to_string()))
    }) {
        // Removes alias.
        use_aliases.remove(&alias);

        // Resolves path if it points to a use declaration for one of the specified external crates.
        if let Some(target) = ink_analyzer_ir::path_from_str(&use_path_str)
            .as_ref()
            .and_then(ast::Path::segment)
        {
            let result = match_path_to_external_crate_in_scope(&use_path_str, crates, ref_node);
            if let Some(resolved_use_path) =
                result.0.iter().next().or(result.1.get(&target.to_string()))
            {
                use_aliases.insert(alias, resolved_use_path.to_string());
            }
        }
    }

    (use_paths, use_aliases)
}

// Matches a path to an external crate item (if possible) using use declarations and aliases in the current scope.
fn match_path_to_external_crate_in_scope(
    path: &str,
    crates: &[&str],
    ref_node: &SyntaxNode,
) -> (HashSet<String>, HashMap<String, String>) {
    let mut use_paths = HashSet::new();
    let mut use_aliases = HashMap::new();
    let use_path_option = ink_analyzer_ir::path_from_str(path).and_then(|path| {
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
            let result = external_crate_uses_and_aliases_in_scope(crates, &qualifier_ref_node);
            use_paths.extend(result.0);
            use_aliases.extend(result.1);
        } else {
            for path in
                ink_analyzer_ir::resolve_item_path_from_use_scope!(target_name, &qualifier_ref_node)
            {
                let resolved_path_str = path.to_string().replace(' ', "");
                if is_crate_item_path(&resolved_path_str, crates) {
                    if resolved_path_str.ends_with(&format!("::{target_name}")) {
                        use_paths.insert(resolved_path_str.clone());
                    } else {
                        use_aliases.insert(target_name.clone(), resolved_path_str);
                    }
                } else {
                    let result = match_path_to_external_crate_in_scope(
                        &resolved_path_str,
                        crates,
                        &qualifier_ref_node,
                    );
                    use_paths.extend(result.0);
                    use_aliases.extend(result.1);
                }
            }
        }
    }

    (use_paths, use_aliases)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::parse_first_ast_node_of_type;
    use ink_analyzer_ir::ast::{HasName, SourceFile};
    use ink_analyzer_ir::{InkEntity, InkFile};
    use quote::quote;
    use test_utils::quote_as_str;

    #[test]
    fn external_crate_item_path_resolution_works() {
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
                is_external_crate_item(
                    "Environment",
                    &path,
                    &["ink::env", "ink_env"],
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