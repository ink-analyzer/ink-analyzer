//! ink! 5.0 host function related migrations.

use ink_analyzer_ir::ast;
use ink_analyzer_ir::ast::{AstNode, CallExpr};
use ink_analyzer_ir::syntax::SyntaxNode;
use ink_analyzer_ir::{InkEntity, InkFile};

use super::common;
use super::traversal::{walk_call, Visitor};
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
    fn visit_call(&mut self, expr: &CallExpr) {
        // Represents a `build_call::<..>(..)` or `build_create::<..>(..)` call.
        enum HostFnBuilder {
            Call(ast::Path),
            Instantiate(ast::Path),
        }

        let call_path = expr
            .expr()
            .and_then(|call_expr| match call_expr {
                ast::Expr::PathExpr(path) => path.path(),
                _ => None,
            })
            .and_then(|path| {
                if common::is_call_path(
                    &path,
                    "build_call",
                    &["ink_env::call", "ink::env::call"],
                    expr.syntax(),
                ) {
                    Some(HostFnBuilder::Call(path))
                } else if common::is_call_path(
                    &path,
                    "build_create",
                    &["ink_env::call", "ink::env::call"],
                    expr.syntax(),
                ) {
                    Some(HostFnBuilder::Instantiate(path))
                } else {
                    None
                }
            });
        let Some(call_path) = call_path else {
            walk_call(self, expr);
            return;
        };

        // Replace `.call(..)` with `.call_v1(..)` or `.instantiate(..)` with `.instantiate_v1(..)`.
        let (name, allowed_intermediates) = match call_path {
            HostFnBuilder::Call(_) => (
                "call",
                vec![
                    "call_type",
                    "call_flags",
                    "returns",
                    "exec_input",
                    "call",
                    "delegate",
                    "gas_limit",
                    "transferred_value",
                    "code_hash",
                ],
            ),
            HostFnBuilder::Instantiate(_) => (
                "instantiate",
                vec![
                    "code_hash",
                    "gas_limit",
                    "endowment",
                    "exec_input",
                    "salt_bytes",
                    "returns",
                ],
            ),
        };
        let Some((_, host_fn_name_ref)) =
            chained_host_fn_call(expr.syntax(), name, &allowed_intermediates)
        else {
            return;
        };
        self.results.push(TextEdit::replace(
            format!("{name}_v1"),
            host_fn_name_ref.syntax().text_range(),
        ));

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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::text_edits_from_fixtures;
    use quote::quote;
    use test_utils::quote_as_pretty_string;

    #[test]
    fn fn_body_works() {
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
            // build_create::<..>()..instantiate(..)
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
                vec![("instantiate_v1", Some("<-instantiate"), Some("instantiate"))],
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
