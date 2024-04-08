//! ink! 5.0 host function related migrations.

use ink_analyzer_ir::ast;
use ink_analyzer_ir::ast::AstNode;
use ink_analyzer_ir::syntax::SyntaxNode;
use ink_analyzer_ir::{InkEntity, InkFile};

use super::common;
use super::traversal::{walk_call, walk_method_call, Visitor};
use crate::TextEdit;

/// Computes text edits for ink! 5.0 host function related migrations.
pub fn migrate(results: &mut Vec<TextEdit>, file: &InkFile) {
    for fn_item in file.syntax().descendants().filter_map(ast::Fn::cast) {
        if let Some(body) = fn_item.body() {
            let mut visitor = BodyVisitor::new();
            visitor.visit_block(&body);
            results.extend(visitor.results);
        }
    }
}

struct BodyVisitor {
    results: Vec<TextEdit>,
}

impl BodyVisitor {
    fn new() -> Self {
        Self {
            results: Vec::new(),
        }
    }
}

impl Visitor for BodyVisitor {
    fn visit_call(&mut self, expr: &ast::CallExpr) {
        // Represents a `build_call::<..>(..)`, `build_create::<..>(..)`,
        // `instantiate_contract::<..>(..)` or `invoke_contract::<..>(..)` call.
        enum HostFnWrapper {
            BuildCall(ast::Path),
            BuildCreate(ast::Path),
            InstantiateContract(ast::Path),
            InvokeContract(ast::Path),
        }

        let Some(call_path) = expr.expr().and_then(|call_expr| match call_expr {
            ast::Expr::PathExpr(path) => path.path(),
            _ => None,
        }) else {
            walk_call(self, expr);
            return;
        };

        // Handles `Self::env().instantiate_contract::<..>(..)` and `Self::env().invoke_contract::<..>(..)` calls.
        let is_self_type_env_call = {
            call_path
                .qualifier()
                .zip(
                    call_path
                        .segment()
                        .as_ref()
                        .and_then(ast::PathSegment::name_ref),
                )
                .is_some_and(|(qualifier, name)| {
                    qualifier.to_string() == "Self" && name.to_string() == "env"
                })
        };
        if is_self_type_env_call {
            chained_env_host_fn_call(&mut self.results, expr.syntax());
            return;
        }

        let host_fn_wrapper_path = if common::is_call_path(
            &call_path,
            "build_call",
            &["ink_env::call", "ink::env::call"],
            expr.syntax(),
        ) {
            Some(HostFnWrapper::BuildCall(call_path))
        } else if common::is_call_path(
            &call_path,
            "build_create",
            &["ink_env::call", "ink::env::call"],
            expr.syntax(),
        ) {
            Some(HostFnWrapper::BuildCreate(call_path))
        } else if common::is_call_path(
            &call_path,
            "instantiate_contract",
            &["ink_env", "ink::env"],
            expr.syntax(),
        ) {
            Some(HostFnWrapper::InstantiateContract(call_path))
        } else if common::is_call_path(
            &call_path,
            "invoke_contract",
            &["ink_env", "ink::env"],
            expr.syntax(),
        ) {
            Some(HostFnWrapper::InvokeContract(call_path))
        } else {
            None
        };
        let Some(host_fn_wrapper_path) = host_fn_wrapper_path else {
            walk_call(self, expr);
            return;
        };

        // Migrate host function wrapper APIs.
        match host_fn_wrapper_path {
            // Add `.instantiate_v1(..)` to `build_create::<..>(..)`.
            HostFnWrapper::BuildCreate(_) => {
                self.results.push(TextEdit::insert(
                    ".instantiate_v1()".to_owned(),
                    expr.syntax().text_range().end(),
                ));
            }
            // Replace `.call(..)` with `.call_v1(..)`.
            HostFnWrapper::BuildCall(_) => {
                let Some((_, host_fn_name_ref)) = chained_host_fn_call(
                    expr.syntax(),
                    "call",
                    &[
                        "call_type",
                        "call_flags",
                        "returns",
                        "exec_input",
                        "delegate",
                        "gas_limit",
                        "transferred_value",
                        "code_hash",
                    ],
                ) else {
                    return;
                };
                self.results.push(TextEdit::replace(
                    "call_v1".to_owned(),
                    host_fn_name_ref.syntax().text_range(),
                ));
            }
            // Replace `instantiate_contract::<..>(..)` with `instantiate_contract_v1::<..>(..)` or
            // `invoke_contract::<..>(..)` with `invoke_contract::<..>(..)`.
            HostFnWrapper::InstantiateContract(ref path)
            | HostFnWrapper::InvokeContract(ref path) => {
                let Some(name_ref) = path.segment().as_ref().and_then(ast::PathSegment::name_ref)
                else {
                    return;
                };
                self.results.push(TextEdit::replace(
                    format!("{name_ref}_v1"),
                    name_ref.syntax().text_range(),
                ));
            }
        }

        fn chained_host_fn_call(
            ref_node: &SyntaxNode,
            name: &str,
            allowed_intermediates: &[&str],
        ) -> Option<(ast::MethodCallExpr, ast::NameRef)> {
            ref_node
                .parent()
                .and_then(ast::MethodCallExpr::cast)
                .and_then(|method_call| {
                    method_call.name_ref().and_then(|method_name_ref| {
                        let method_name = method_name_ref.to_string();
                        if method_name == name {
                            Some((method_call, method_name_ref))
                        } else if allowed_intermediates.contains(&method_name.as_str()) {
                            chained_host_fn_call(method_call.syntax(), name, allowed_intermediates)
                        } else {
                            None
                        }
                    })
                })
        }
    }

    fn visit_method_call(&mut self, expr: &ast::MethodCallExpr) {
        let is_self_env_call = expr
            .receiver()
            .and_then(|receiver| match receiver {
                ast::Expr::PathExpr(path) => Some(path),
                _ => None,
            })
            .is_some_and(|receiver| {
                receiver.to_string() == "self"
                    && expr
                        .name_ref()
                        .is_some_and(|name| name.to_string() == "env")
            });
        if !is_self_env_call {
            walk_method_call(self, expr);
        }
        chained_env_host_fn_call(&mut self.results, expr.syntax());
    }
}

/// Computes text edits for `Self::env()` and `self.env()` host function related migrations.
fn chained_env_host_fn_call(results: &mut Vec<TextEdit>, ref_node: &SyntaxNode) {
    let Some(chained_v1_method_name) = ref_node
        .parent()
        .and_then(ast::MethodCallExpr::cast)
        .as_ref()
        .and_then(ast::MethodCallExpr::name_ref)
        .filter(|method_name| {
            matches!(
                method_name.to_string().as_str(),
                "instantiate_contract" | "invoke_contract"
            )
        })
    else {
        return;
    };
    results.push(TextEdit::replace(
        format!("{chained_v1_method_name}_v1"),
        chained_v1_method_name.syntax().text_range(),
    ));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::text_edits_from_fixtures;
    use quote::quote;
    use test_utils::quote_as_pretty_string;

    #[test]
    fn migrate_works() {
        for (code, expected_results) in [
            // build_call::<..>().call(..)
            (
                quote! { use ink::env::call::build_call; },
                quote! {
                    let result = build_call::<DefaultEnvironment>()
                        .call(AccountId::from([0x42; 32]))
                        .gas_limit(5000)
                        .transferred_value(10)
                        .exec_input(
                            ExecutionInput::new(Selector::new([0xDE, 0xAD, 0xBE, 0xEF]))
                                .push_arg(42u8)
                                .push_arg(true)
                                .push_arg(&[0x10u8; 32]),
                        )
                        .returns::<()>()
                        .invoke();
                },
                vec![("call_v1", Some("<-call("), Some(".call"))],
            ),
            (
                quote! { use ink_env::call::build_call; },
                quote! {
                    let result = build_call::<DefaultEnvironment>()
                        .call(AccountId::from([0x42; 32]))
                        .gas_limit(5000)
                        .transferred_value(10)
                        .exec_input(
                            ExecutionInput::new(Selector::new([0xDE, 0xAD, 0xBE, 0xEF]))
                                .push_arg(42u8)
                                .push_arg(true)
                                .push_arg(&[0x10u8; 32]),
                        )
                        .returns::<()>()
                        .invoke();
                },
                vec![("call_v1", Some("<-call("), Some(".call"))],
            ),
            (
                quote! {},
                quote! {
                    let result = ink::env::call::build_call::<DefaultEnvironment>()
                        .call(AccountId::from([0x42; 32]))
                        .gas_limit(5000)
                        .transferred_value(10)
                        .exec_input(
                            ExecutionInput::new(Selector::new([0xDE, 0xAD, 0xBE, 0xEF]))
                                .push_arg(42u8)
                                .push_arg(true)
                                .push_arg(&[0x10u8; 32]),
                        )
                        .returns::<()>()
                        .invoke();
                },
                vec![("call_v1", Some("<-call("), Some(".call"))],
            ),
            (
                quote! { use ink::env::call; },
                quote! {
                    let result = call::build_call::<DefaultEnvironment>()
                        .call(AccountId::from([0x42; 32]))
                        .gas_limit(5000)
                        .transferred_value(10)
                        .exec_input(
                            ExecutionInput::new(Selector::new([0xDE, 0xAD, 0xBE, 0xEF]))
                                .push_arg(42u8)
                                .push_arg(true)
                                .push_arg(&[0x10u8; 32]),
                        )
                        .returns::<()>()
                        .invoke();
                },
                vec![("call_v1", Some("<-call("), Some(".call"))],
            ),
            // build_create::<..>()
            (
                quote! { use ink::env::call::build_create; },
                quote! {
                    let my_contract: MyContractRef = build_create::<MyContractRef>()
                        .code_hash(Hash::from([0x42; 32]))
                        .gas_limit(4000)
                        .endowment(25)
                        .exec_input(
                            ExecutionInput::new(
                                Selector::new(ink::selector_bytes!("my_constructor"))
                            )
                            .push_arg(42)
                            .push_arg(true)
                            .push_arg(&[0x10u8; 32]),
                        )
                        .salt_bytes(&[0xDE, 0xAD, 0xBE, 0xEF])
                        .returns::<MyContractRef>()
                        .instantiate();
                },
                vec![(
                    ".instantiate_v1()",
                    Some("<MyContractRef>()"),
                    Some("<MyContractRef>()"),
                )],
            ),
            // instantiate_contract::<..>()
            (
                quote! { use ink::env::instantiate_contract; },
                quote! {
                    instantiate_contract::<E, ContractRef, Args, Salt, R>(params);
                },
                vec![(
                    "instantiate_contract_v1",
                    Some("<-instantiate_contract->"),
                    Some("instantiate_contract->"),
                )],
            ),
            // invoke_contract::<..>()
            (
                quote! { use ink::env::invoke_contract; },
                quote! {
                    invoke_contract::<E, Args, R>(params);
                },
                vec![(
                    "invoke_contract_v1",
                    Some("<-invoke_contract->"),
                    Some("invoke_contract->"),
                )],
            ),
            // self.env().instantiate_contract::<..>()
            (
                quote! {},
                quote! {
                    self.env()
                        .instantiate_contract(&create_params)
                },
                vec![(
                    "instantiate_contract_v1",
                    Some("<-instantiate_contract->"),
                    Some("instantiate_contract->"),
                )],
            ),
            // self.env().invoke_contract::<..>()
            (
                quote! {},
                quote! {
                    self.env()
                        .invoke_contract(&call_params)
                },
                vec![(
                    "invoke_contract_v1",
                    Some("<-invoke_contract->"),
                    Some("invoke_contract->"),
                )],
            ),
            // Self::env().instantiate_contract::<..>()
            (
                quote! {},
                quote! {
                    Self::env()
                        .instantiate_contract(&create_params)
                },
                vec![(
                    "instantiate_contract_v1",
                    Some("<-instantiate_contract->"),
                    Some("instantiate_contract->"),
                )],
            ),
            // Self::env().invoke_contract::<..>()
            (
                quote! {},
                quote! {
                    Self::env()
                        .invoke_contract(&call_params)
                },
                vec![(
                    "invoke_contract_v1",
                    Some("<-invoke_contract->"),
                    Some("invoke_contract->"),
                )],
            ),
        ]
        .into_iter()
        .flat_map(|(imports, code, expected_results)| {
            [
                (
                    quote_as_pretty_string! {
                        #imports

                        fn my_fn() {
                            #code
                        }
                    },
                    expected_results.clone(),
                ),
                (
                    quote_as_pretty_string! {
                        #imports

                        fn my_fn() {
                            let closure = || {
                                #code
                            };
                            closure();
                        }
                    },
                    expected_results,
                ),
            ]
        }) {
            let mut results = Vec::new();
            let file = InkFile::parse(&code);
            migrate(&mut results, &file);

            assert_eq!(results, text_edits_from_fixtures(&code, expected_results));
        }
    }
}
