//! ink! 5.0 e2e test migration.

use std::collections::HashSet;
use std::fmt;

use ink_analyzer_ir::ast::{HasArgList, HasDocComments, HasName};
use ink_analyzer_ir::syntax::{AstNode, AstToken, SyntaxKind, TextRange};
use ink_analyzer_ir::{ast, Contract, InkE2ETest, InkEntity, InkFile, IsInkFn};
use itertools::Itertools;

use super::common;
use super::traversal::{walk_call, walk_method_call, Visitor};
use crate::analysis::utils;
use crate::{resolution, TextEdit};

/// Computes text edits for migrating ink! e2e tests to ink! 5.0.
pub fn migrate(results: &mut Vec<TextEdit>, file: &InkFile) {
    for e2e_test in file
        .e2e_tests()
        .iter()
        .chain(file.contracts().iter().flat_map(Contract::e2e_tests))
    {
        // Migrate e2e test attribute.
        attribute(results, e2e_test);

        // Migrate e2e test `fn` signature.
        fn_sig(results, e2e_test);

        // Migrate e2e test `fn` body.
        fn_body(results, e2e_test);

        // Remove deprecated imports.
        deprecated_imports(results, e2e_test);
    }
}

/// Computes text edits for migrating ink! e2e test attributes to ink! 5.0.
fn attribute(results: &mut Vec<TextEdit>, e2e_test: &InkE2ETest) {
    if e2e_test.environment_arg().is_none()
        && (e2e_test.additional_contracts_arg().is_some()
            || e2e_test.additional_contracts_arg().is_some())
    {
        // Remove all attribute arguments if the ink! e2e attribute has either an
        // `additional_contracts` and/or `keep_attr` argument but no `environment` argument.
        if let Some(meta) = e2e_test.ink_attr().and_then(|attr| attr.ast().token_tree()) {
            results.push(TextEdit::delete(meta.syntax().text_range()));
        }
    } else {
        // Remove `additional_contracts` argument (if any).
        if let Some(arg) = e2e_test.additional_contracts_arg() {
            let range = utils::ink_arg_and_delimiter_removal_range(&arg, None);
            results.push(TextEdit::delete(range));
        }

        // Remove `keep_attr` argument (if any).
        if let Some(arg) = e2e_test.keep_attr_arg() {
            let range = utils::ink_arg_and_delimiter_removal_range(&arg, None);
            results.push(TextEdit::delete(range));
        }
    }
}

/// Computes text edits for migrating ink! e2e test function signatures to ink! 5.0.
fn fn_sig(results: &mut Vec<TextEdit>, e2e_test: &InkE2ETest) {
    if let Some(fn_item) = e2e_test.fn_item() {
        // Edit range for `fn` parameter list.
        let range = fn_item
            .param_list()
            .map(|param_list| param_list.syntax().text_range())
            .or_else(|| {
                fn_item.name().map(|name| {
                    // Default to inserting at end of function name (if any).
                    let name_end = name.syntax().text_range().end();
                    TextRange::new(name_end, name_end)
                })
            });
        if let Some(range) = range {
            results.push(TextEdit::replace(
                "<Client: E2EBackend>(mut client: Client)".to_owned(),
                range,
            ));
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum E2ETrait {
    Contracts,
    Chain,
    #[allow(dead_code)]
    E2E,
}
impl fmt::Display for E2ETrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ink_e2e::{:?}Backend", self)
    }
}

/// Computes text edits for migrating ink! e2e test function bodies to ink! 5.0.
fn fn_body(results: &mut Vec<TextEdit>, e2e_test: &InkE2ETest) {
    let Some(body) = e2e_test.fn_item().and_then(ast::Fn::body) else {
        return;
    };

    let mut visitor = BodyVisitor::new();
    visitor.visit_block(&body);

    results.extend(visitor.results);

    // Add required trait imports (if any).
    if visitor.required_traits.is_empty() {
        return;
    }
    let mut insert_offset_and_indent = None;
    let parent_item = ink_analyzer_ir::parent_ast_item(e2e_test.syntax());
    if parent_item.is_none() {
        // Insert trait imports in file root if e2e test `fn` has no parent item.
        if let Some(file_root) = e2e_test
            .syntax()
            .ancestors()
            .last()
            .and_then(ast::SourceFile::cast)
        {
            let insert_offset = file_root
                .doc_comments()
                .last()
                .as_ref()
                .map(|comment| comment.syntax().text_range().end())
                .unwrap_or_else(|| file_root.syntax().text_range().start());
            insert_offset_and_indent = Some((insert_offset, String::new()));
        }
    } else {
        // Alternatively, insert trait imports in parent `mod` item of e2e test `fn` (if any).
        if let Some(ast::Item::Module(mod_item)) = parent_item {
            insert_offset_and_indent = mod_item
                .item_list()
                .as_ref()
                .map(utils::item_insert_offset_start)
                .map(|insert_offset| {
                    (
                        insert_offset,
                        utils::item_children_indenting(mod_item.syntax()),
                    )
                });
        };

        if insert_offset_and_indent.is_none() {
            // Fallback to inserting trait imports inside the e2e test `fn`.
            if let Some(fn_item) = e2e_test.fn_item() {
                insert_offset_and_indent = fn_item
                    .body()
                    .as_ref()
                    .and_then(ast::BlockExpr::stmt_list)
                    .as_ref()
                    .and_then(ast::StmtList::l_curly_token)
                    .map(|token| token.text_range().end())
                    .map(|insert_offset| {
                        (
                            insert_offset,
                            utils::item_children_indenting(fn_item.syntax()),
                        )
                    });
            }
        }
    }

    if let Some((insert_offset, indent)) = insert_offset_and_indent {
        for e2e_trait in visitor.required_traits {
            results.push(TextEdit::insert(
                format!("\n{indent}use {e2e_trait};"),
                insert_offset,
            ));
        }
    }
}

/// Computes text edits for removing use declarations of ink! e2e items deprecated in ink! 5.0.
fn deprecated_imports(results: &mut Vec<TextEdit>, e2e_test: &InkE2ETest) {
    if let Some(fn_item) = e2e_test.fn_item() {
        let parent_module = ink_analyzer_ir::parent_ast_item(fn_item.syntax())
            .and_then(|item| match item {
                ast::Item::Module(mod_item) => Some(mod_item.syntax().clone()),
                _ => None,
            })
            .or_else(|| {
                e2e_test
                    .syntax()
                    .ancestors()
                    .last()
                    .filter(|node| ast::SourceFile::can_cast(node.kind()))
            });
        if let Some(parent_module) = parent_module {
            for use_item in parent_module.descendants().filter_map(ast::Use::cast) {
                if let Some(use_tree) = use_item.use_tree() {
                    let (to_remove, contains_valid_items) = deprecated_imports_inner(&use_tree);
                    if !to_remove.is_empty() {
                        if contains_valid_items {
                            // Remove only the deprecated use tree items.
                            results.extend(to_remove.into_iter().map(TextEdit::delete))
                        } else {
                            // Remove the parent use item if all use tree items are deprecated.
                            results.push(TextEdit::delete(utils::node_and_delimiter_range(
                                use_item.syntax(),
                                SyntaxKind::COMMA,
                            )));
                        }
                    }
                }
            }
        }
    }

    // Returns the text ranges of imported items to remove, and a flag indicating whether
    // any other valid imported items where found for the use tree.
    fn deprecated_imports_inner(use_tree: &ast::UseTree) -> (Vec<TextRange>, bool) {
        if let Some(path) = use_tree.path() {
            let is_deprecated_item_path = path.segment().is_some_and(|segment| {
                matches!(
                    segment.to_string().as_str(),
                    "build_message" | "MessageBuilder"
                )
            });
            if is_deprecated_item_path
                && path
                    .qualifier()
                    .is_some_and(|qualifier| qualifier.to_string() == "ink_e2e")
            {
                // Handles a simple import (e.g. `use ink_e2e::build_message;`).
                return (
                    vec![utils::node_and_delimiter_range(
                        use_tree.syntax(),
                        SyntaxKind::COMMA,
                    )],
                    false,
                );
            } else if path.to_string() == "ink_e2e" {
                // Handles nested imports (e.g. `use ink_e2e::{build_message, MessageBuilder};`).
                let mut to_remove = Vec::new();
                let mut n_valid = 0;
                if let Some(use_tree_list) = use_tree.use_tree_list() {
                    for child_use_tree in use_tree_list.use_trees() {
                        let is_deprecated_item_path = child_use_tree.path().is_some_and(|path| {
                            matches!(
                                path.to_string().as_str(),
                                "build_message" | "MessageBuilder"
                            )
                        });
                        if is_deprecated_item_path {
                            to_remove.push(utils::node_and_delimiter_range(
                                child_use_tree.syntax(),
                                SyntaxKind::COMMA,
                            ));
                        } else {
                            n_valid += 1;
                        }
                    }
                }

                if !to_remove.is_empty() {
                    return if n_valid == 0 {
                        // Remove the parent use tree if all use tree list items are deprecated.
                        (
                            vec![utils::node_and_delimiter_range(
                                use_tree.syntax(),
                                SyntaxKind::COMMA,
                            )],
                            false,
                        )
                    } else {
                        // Remove only the deprecated use tree list items.
                        (to_remove, true)
                    };
                }
            }
        } else if let Some(use_tree_list) = use_tree.use_tree_list() {
            // Handles "one" style imports (e.g. `use {ink_e2e::build_message, ..};`).
            let mut to_remove = Vec::new();
            let mut contains_valid_items = false;
            for use_tree in use_tree_list.use_trees() {
                let (to_remove_local, contains_valid_items_local) =
                    deprecated_imports_inner(&use_tree);
                to_remove.extend(to_remove_local);
                contains_valid_items |= contains_valid_items_local;
            }

            if !to_remove.is_empty() {
                return if contains_valid_items {
                    // Remove only the deprecated use tree list items.
                    (to_remove, true)
                } else {
                    // Remove the parent use tree if all use tree list items are deprecated.
                    (
                        vec![utils::node_and_delimiter_range(
                            use_tree.syntax(),
                            SyntaxKind::COMMA,
                        )],
                        false,
                    )
                };
            }
        }

        // No deprecated items to remove.
        (Vec::new(), true)
    }
}

struct BodyVisitor {
    results: Vec<TextEdit>,
    required_traits: HashSet<E2ETrait>,
}

impl BodyVisitor {
    fn new() -> Self {
        Self {
            results: Vec::new(),
            required_traits: HashSet::new(),
        }
    }
}

impl Visitor for BodyVisitor {
    fn visit_call(&mut self, expr: &ast::CallExpr) {
        let Some(message_builder_call) = MessageBuilderCall::parse(expr) else {
            walk_call(self, expr);
            return;
        };

        // Replace `build_message::<..>(..)` or `MessageBuilder<..>::from_account_id(..)` with `create_call_builder<..>(..)`.
        let (call_path, generic_args_list) = match message_builder_call {
            MessageBuilderCall::BuildMessageFn(ref path) => {
                let generic_args_list = message_builder_call
                    .generic_args()
                    .as_ref()
                    .map(ToString::to_string);
                (path, generic_args_list)
            }
            MessageBuilderCall::MessageBuilderFromAccountId(ref path) => {
                let generic_args_list = message_builder_call
                    .generic_args()
                    .as_ref()
                    .map(ast::GenericArgList::generic_args)
                    .and_then(|generic_args| {
                        generic_args
                            .last()
                            .map(|last_generic_arg| format!("::<{}>", last_generic_arg))
                    });
                (path, generic_args_list)
            }
        };
        self.results.push(TextEdit::replace(
            format!(
                "ink_e2e::create_call_builder{}",
                generic_args_list.as_deref().unwrap_or_default()
            ),
            call_path.syntax().text_range(),
        ));

        // Replace `.call(|contract| contract.message(..))` with `.message(..)`.
        let Some(chained_call_method) = expr.syntax().parent().and_then(ast::MethodCallExpr::cast)
        else {
            return;
        };
        let Some(chained_call_method_name) = chained_call_method
            .name_ref()
            .filter(|method_name| method_name.to_string() == "call")
        else {
            return;
        };
        let Some(chained_call_method_args) = chained_call_method
            .arg_list()
            .as_ref()
            .map(ast::ArgList::args)
        else {
            return;
        };
        let Some((ast::Expr::ClosureExpr(callback),)) = chained_call_method_args.collect_tuple()
        else {
            return;
        };
        let Some(callback_body) = callback.body() else {
            return;
        };
        let Some(contract_call) = last_method_call(&callback_body) else {
            return;
        };
        let Some(contract_call_name) = contract_call.name_ref() else {
            return;
        };
        self.results.push(TextEdit::replace(
            format!(
                "{}{}",
                contract_call_name,
                contract_call
                    .arg_list()
                    .as_ref()
                    .map(ToString::to_string)
                    .unwrap_or_else(|| "()".to_owned())
            ),
            TextRange::new(
                chained_call_method_name.syntax().text_range().start(),
                chained_call_method.syntax().text_range().end(),
            ),
        ));

        fn last_method_call(expr: &ast::Expr) -> Option<ast::MethodCallExpr> {
            match expr {
                ast::Expr::MethodCallExpr(method_call) => Some(method_call.clone()),
                ast::Expr::BlockExpr(body) => {
                    // Only handles tail expr or returned expr.
                    let returned_expr = || {
                        body.stmt_list()
                            .as_ref()
                            .map(ast::StmtList::statements)
                            .and_then(|mut stmts| stmts.next())
                            .and_then(|stmt| match stmt {
                                ast::Stmt::ExprStmt(expr) => expr.expr(),
                                _ => None,
                            })
                            .and_then(|expr| match expr {
                                ast::Expr::ReturnExpr(return_expr) => return_expr.expr(),
                                _ => None,
                            })
                    };
                    body.tail_expr()
                        .or_else(returned_expr)
                        .as_ref()
                        .and_then(last_method_call)
                }
                ast::Expr::ReturnExpr(return_expr) => {
                    return_expr.expr().as_ref().and_then(last_method_call)
                }
                _ => None,
            }
        }
    }

    fn visit_method_call(&mut self, expr: &ast::MethodCallExpr) {
        let is_client_receiver = expr
            .receiver()
            .and_then(|receiver| match receiver {
                ast::Expr::PathExpr(path) => Some(path),
                _ => None,
            })
            .is_some_and(|receiver| receiver.to_string() == "client");
        if !is_client_receiver {
            walk_method_call(self, expr);
        }

        let Some(method_name) = expr.name_ref() else {
            walk_method_call(self, expr);
            return;
        };
        let Some(arg_list) = expr.arg_list() else {
            return;
        };
        let args = arg_list.args();

        match method_name.to_string().as_str() {
            name @ ("instantiate" | "instantiate_dry_run") => {
                if let Some((contract_name, signer, constructor, value, storage_deposit_limit)) =
                    args.collect_tuple()
                {
                    self.results.push(TextEdit::replace(
                        format!(
                            "({contract_name}, {signer}, {constructor}){}{}{}",
                            value_call(&value).as_deref().unwrap_or_default(),
                            storage_deposit_limit_call(&storage_deposit_limit)
                                .as_deref()
                                .unwrap_or_default(),
                            if name == "instantiate_dry_run" {
                                ".dry_run()"
                            } else {
                                ".submit()"
                            }
                        ),
                        arg_list.syntax().text_range(),
                    ));

                    if name == "instantiate_dry_run" {
                        unwrap_dry_run_result(&mut self.results, expr);
                    }

                    self.required_traits.insert(E2ETrait::Contracts);
                }
            }
            name @ ("call" | "call_dry_run") => {
                if let Some((signer, message, value, storage_deposit_limit)) = args.collect_tuple()
                {
                    self.results.push(TextEdit::replace(
                        format!(
                            "({signer}, {message}){}{}{}",
                            value_call(&value).as_deref().unwrap_or_default(),
                            storage_deposit_limit_call(&storage_deposit_limit)
                                .as_deref()
                                .unwrap_or_default(),
                            if name == "call_dry_run" {
                                ".dry_run()"
                            } else {
                                ".submit()"
                            }
                        ),
                        arg_list.syntax().text_range(),
                    ));

                    if name == "call_dry_run" {
                        unwrap_dry_run_result(&mut self.results, expr);
                    }

                    self.required_traits.insert(E2ETrait::Contracts);
                }
            }
            "upload" => {
                if let Some((contract_name, signer, storage_deposit_limit)) = args.collect_tuple() {
                    self.results.push(TextEdit::replace(
                        format!(
                            "({contract_name}, {signer}){}.submit()",
                            storage_deposit_limit_call(&storage_deposit_limit)
                                .as_deref()
                                .unwrap_or_default(),
                        ),
                        arg_list.syntax().text_range(),
                    ));

                    self.required_traits.insert(E2ETrait::Contracts);
                }
            }
            "create_and_fund_account" => {
                if args.count() == 2 {
                    self.required_traits.insert(E2ETrait::Chain);
                }
            }
            "balance" => {
                if args.count() == 1 {
                    self.results.push(TextEdit::replace(
                        "free_balance".to_owned(),
                        method_name.syntax().text_range(),
                    ));

                    self.required_traits.insert(E2ETrait::Chain);
                }
            }
            "runtime_call" => {
                if args.count() == 4 {
                    self.required_traits.insert(E2ETrait::Chain);
                }
            }
            _ => (),
        }

        fn value_call(expr: &ast::Expr) -> Option<String> {
            let mut is_zero = false;
            if let ast::Expr::Literal(ref lit) = expr {
                if let ast::LiteralKind::IntNumber(int) = lit.kind() {
                    if let Ok(value) = int.value() {
                        is_zero = value == 0;
                    }
                }
            }
            (!is_zero).then_some(format!(".value({expr})"))
        }

        fn storage_deposit_limit_call(expr: &ast::Expr) -> Option<String> {
            let mut is_none = false;
            if let ast::Expr::PathExpr(ref path) = expr {
                is_none = matches!(path.to_string().as_str(), "None" | "Option::None");
            }
            (!is_none).then(|| {
                let mut storage_deposit_limit_value = None;
                if let ast::Expr::CallExpr(ref call) = expr {
                    if let Some(ast::Expr::PathExpr(path)) = call.expr() {
                        if matches!(path.to_string().as_str(), "Some" | "Option::Some") {
                            storage_deposit_limit_value = call
                                .arg_list()
                                .as_ref()
                                .map(ast::ArgList::args)
                                .and_then(|mut args| args.next())
                                .as_ref()
                                .map(ToString::to_string);
                        }
                    }
                }
                format!(
                    ".storage_deposit_limit({})",
                    storage_deposit_limit_value.unwrap_or_else(|| { expr.to_string() })
                )
            })
        }

        fn unwrap_dry_run_result(results: &mut Vec<TextEdit>, expr: &ast::MethodCallExpr) {
            if let Some(await_expr) = expr.syntax().parent().and_then(ast::AwaitExpr::cast) {
                results.push(TextEdit::insert(
                    "?".to_owned(),
                    await_expr.syntax().text_range().end(),
                ));
            }
        }
    }
}

/// Represents a `build_message::<..>(..)` or `MessageBuilder<..>::from_account_id(..)` call.
enum MessageBuilderCall {
    BuildMessageFn(ast::Path),
    MessageBuilderFromAccountId(ast::Path),
}

impl MessageBuilderCall {
    fn parse(expr: &ast::CallExpr) -> Option<Self> {
        let is_message_builder_from_account_id_call_path = |path: &ast::Path| {
            path.qualifier()
                .zip(path.segment())
                .is_some_and(|(qualifier, segment)| {
                    if segment.to_string() == "from_account_id" {
                        let type_path = common::last_segment_generic_args(&qualifier).and_then(
                            |generic_args| common::simplify_path(&qualifier, &generic_args),
                        );
                        resolution::is_external_crate_item(
                            "MessageBuilder",
                            type_path.as_ref().unwrap_or(&qualifier),
                            &["ink_e2e"],
                            expr.syntax(),
                        )
                    } else {
                        false
                    }
                })
        };

        expr.expr()
            .and_then(|call_expr| match call_expr {
                ast::Expr::PathExpr(path) => path.path(),
                _ => None,
            })
            .and_then(|path| {
                if common::is_call_path(&path, "build_message", &["ink_e2e"], expr.syntax()) {
                    Some(MessageBuilderCall::BuildMessageFn(path))
                } else if is_message_builder_from_account_id_call_path(&path) {
                    Some(MessageBuilderCall::MessageBuilderFromAccountId(path))
                } else {
                    None
                }
            })
    }

    fn generic_args(&self) -> Option<ast::GenericArgList> {
        match self {
            MessageBuilderCall::BuildMessageFn(path) => common::last_segment_generic_args(path),
            MessageBuilderCall::MessageBuilderFromAccountId(path) => path
                .qualifier()
                .as_ref()
                .and_then(common::last_segment_generic_args),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{parse_first_ink_entity_of_type, text_edits_from_fixtures};
    use quote::quote;
    use test_utils::quote_as_pretty_string;

    #[test]
    fn attribute_works() {
        for (code, expected_results) in [
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(environment = ink::env::DefaultEnvironment)]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(additional_contracts = "adder/Cargo.toml flipper/Cargo.toml")]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![("", Some("#[ink_e2e::test"), Some("<-]"))],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(keep_attr = "foo,bar")]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![("", Some("#[ink_e2e::test"), Some("<-]"))],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(
                        additional_contracts = "adder/Cargo.toml flipper/Cargo.toml",
                        keep_attr = "foo,bar"
                    )]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![("", Some("#[ink_e2e::test"), Some("<-]"))],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(
                        additional_contracts = "adder/Cargo.toml flipper/Cargo.toml",
                        environment = ink::env::DefaultEnvironment,
                        keep_attr = "foo,bar"
                    )]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![
                    (
                        "",
                        Some("<-additional_contracts"),
                        Some(r#""adder/Cargo.toml flipper/Cargo.toml","#),
                    ),
                    ("", Some("<-keep_attr"), Some("\"foo,bar\"\n")),
                ],
            ),
        ] {
            let mut results = Vec::new();
            let e2e_test = parse_first_ink_entity_of_type::<InkE2ETest>(&code);
            attribute(&mut results, &e2e_test);

            assert_eq!(results, text_edits_from_fixtures(&code, expected_results));
        }
    }

    #[test]
    fn fn_sig_works() {
        let code = quote_as_pretty_string! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
        };

        let mut results = Vec::new();
        let e2e_test = parse_first_ink_entity_of_type::<InkE2ETest>(&code);
        fn_sig(&mut results, &e2e_test);

        let expected_results = vec![(
            "<Client: E2EBackend>(mut client: Client)",
            Some("<-(mut client: ink_e2e::Client<C, E>)"),
            Some("(mut client: ink_e2e::Client<C, E>)"),
        )];
        assert_eq!(results, text_edits_from_fixtures(&code, expected_results));
    }

    #[test]
    fn fn_body_works() {
        for (imports, code, expected_results) in [
            // instantiate
            (
                quote! {},
                quote! {
                    let contract_acc_id = client
                        .instantiate("erc20", &ink_e2e::alice(), constructor, 0, None)
                        .await
                        .expect("instantiate failed")
                        .account_id;
                },
                vec![
                    (
                        r#"("erc20", &ink_e2e::alice(), constructor).submit()"#,
                        Some(r#"<-("erc20", &ink_e2e::alice(), constructor, 0, None)"#),
                        Some(r#"("erc20", &ink_e2e::alice(), constructor, 0, None)"#),
                    ),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            (
                quote! {},
                quote! {
                    let contract_acc_id = client
                        .instantiate("erc20", &ink_e2e::alice(), constructor, 1, Some(1))
                        .await
                        .expect("instantiate failed")
                        .account_id;
                },
                vec![
                    (
                        r#"("erc20", &ink_e2e::alice(), constructor).value(1).storage_deposit_limit(1).submit()"#,
                        Some(r#"<-("erc20", &ink_e2e::alice(), constructor, 1, Some(1))"#),
                        Some(r#"("erc20", &ink_e2e::alice(), constructor, 1, Some(1))"#),
                    ),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            // instantiate_dry_run
            (
                quote! {},
                quote! {
                    let contract_acc_id = client
                        .instantiate_dry_run("erc20", &ink_e2e::alice(), constructor, 0, None)
                        .await
                        .account_id;
                },
                vec![
                    (
                        r#"("erc20", &ink_e2e::alice(), constructor).dry_run()"#,
                        Some(r#"<-("erc20", &ink_e2e::alice(), constructor, 0, None)"#),
                        Some(r#"("erc20", &ink_e2e::alice(), constructor, 0, None)"#),
                    ),
                    ("?", Some("await"), Some("await")),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            (
                quote! {},
                quote! {
                    let contract_acc_id = client
                        .instantiate_dry_run("erc20", &ink_e2e::alice(), constructor, 1, Some(1))
                        .await
                        .account_id;
                },
                vec![
                    (
                        r#"("erc20", &ink_e2e::alice(), constructor).value(1).storage_deposit_limit(1).dry_run()"#,
                        Some(r#"<-("erc20", &ink_e2e::alice(), constructor, 1, Some(1))"#),
                        Some(r#"("erc20", &ink_e2e::alice(), constructor, 1, Some(1))"#),
                    ),
                    ("?", Some("await"), Some("await")),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            // call
            (
                quote! {},
                quote! {
                    let transfer_res = client
                        .call(&ink_e2e::alice(), transfer, 0, None)
                        .await
                        .expect("transfer failed");
                },
                vec![
                    (
                        "(&ink_e2e::alice(), transfer).submit()",
                        Some("<-(&ink_e2e::alice(), transfer, 0, None)"),
                        Some("(&ink_e2e::alice(), transfer, 0, None)"),
                    ),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            (
                quote! {},
                quote! {
                    let transfer_res = client
                        .call(&ink_e2e::alice(), transfer, 1, Some(1))
                        .await
                        .expect("transfer failed");
                },
                vec![
                    (
                        "(&ink_e2e::alice(), transfer).value(1).storage_deposit_limit(1).submit()",
                        Some("<-(&ink_e2e::alice(), transfer, 1, Some(1))"),
                        Some("(&ink_e2e::alice(), transfer, 1, Some(1))"),
                    ),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            // call_dry_run
            (
                quote! {},
                quote! {
                    let transfer_res = client
                        .call_dry_run(&ink_e2e::alice(), transfer, 0, None)
                        .await;
                },
                vec![
                    (
                        "(&ink_e2e::alice(), transfer).dry_run()",
                        Some("<-(&ink_e2e::alice(), transfer, 0, None)"),
                        Some("(&ink_e2e::alice(), transfer, 0, None)"),
                    ),
                    ("?", Some("await"), Some("await")),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            (
                quote! {},
                quote! {
                    let transfer_res = client
                        .call_dry_run(&ink_e2e::alice(), transfer, 1, Some(1))
                        .await;
                },
                vec![
                    (
                        "(&ink_e2e::alice(), transfer).value(1).storage_deposit_limit(1).dry_run()",
                        Some("<-(&ink_e2e::alice(), transfer, 1, Some(1))"),
                        Some("(&ink_e2e::alice(), transfer, 1, Some(1))"),
                    ),
                    ("?", Some("await"), Some("await")),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            // upload
            (
                quote! {},
                quote! {
                    let contract = client
                        .upload("erc20", &ink_e2e::alice(), None)
                        .await
                        .expect("upload failed");
                },
                vec![
                    (
                        r#"("erc20", &ink_e2e::alice()).submit()"#,
                        Some(r#"<-("erc20", &ink_e2e::alice(), None)"#),
                        Some(r#"("erc20", &ink_e2e::alice(), None)"#),
                    ),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            (
                quote! {},
                quote! {
                    let contract = client
                        .upload("erc20", &ink_e2e::alice(), Some(1))
                        .await
                        .expect("upload failed");
                },
                vec![
                    (
                        r#"("erc20", &ink_e2e::alice()).storage_deposit_limit(1).submit()"#,
                        Some(r#"<-("erc20", &ink_e2e::alice(), Some(1))"#),
                        Some(r#"("erc20", &ink_e2e::alice(), Some(1))"#),
                    ),
                    ("\nuse ink_e2e::ContractsBackend;", Some(""), Some("")),
                ],
            ),
            // create_and_fund_account
            (
                quote! {},
                quote! {
                    let origin = client
                        .create_and_fund_account(&ink_e2e::alice(), 10_000_000_000_000)
                        .await;
                },
                vec![("\nuse ink_e2e::ChainBackend;", Some(""), Some(""))],
            ),
            // balance
            (
                quote! {},
                quote! {
                    let balance = client
                        .balance(contract_acc_id)
                        .await
                        .expect("Failed to get account balance");
                },
                vec![
                    ("free_balance", Some("<-balance("), Some(".balance")),
                    ("\nuse ink_e2e::ChainBackend;", Some(""), Some("")),
                ],
            ),
            // runtime_call
            (
                quote! {},
                quote! {
                    client
                    .runtime_call(&ink_e2e::alice(), "Balances", "transfer", call_data)
                    .await
                    .expect("runtime call failed");
                },
                vec![("\nuse ink_e2e::ChainBackend;", Some(""), Some(""))],
            ),
            // build_message(..).call(..)
            (
                quote! { use ink_e2e::build_message; },
                quote! {
                    let total_supply_msg = build_message::<Erc20Ref>(contract_acc_id.clone())
                        .call(|erc20| erc20.total_supply());
                },
                vec![
                    (
                        "ink_e2e::create_call_builder::<Erc20Ref>",
                        Some("<-build_message::<Erc20Ref>"),
                        Some("build_message::<Erc20Ref>"),
                    ),
                    (
                        "total_supply()",
                        Some("<-call(|erc20| erc20.total_supply())"),
                        Some("call(|erc20| erc20.total_supply())"),
                    ),
                ],
            ),
            (
                quote! { use ink_e2e::build_message; },
                quote! {
                    let transfer = build_message::<Erc20Ref>(contract_acc_id.clone())
                        .call(|erc20| erc20.transfer(bob_account.clone(), transfer_to_bob));
                },
                vec![
                    (
                        "ink_e2e::create_call_builder::<Erc20Ref>",
                        Some("<-build_message::<Erc20Ref>"),
                        Some("build_message::<Erc20Ref>"),
                    ),
                    (
                        "transfer(bob_account.clone(), transfer_to_bob)",
                        Some(
                            "<-call(|erc20| erc20.transfer(bob_account.clone(), transfer_to_bob))",
                        ),
                        Some("call(|erc20| erc20.transfer(bob_account.clone(), transfer_to_bob))"),
                    ),
                ],
            ),
            (
                quote! {},
                quote! {
                    let total_supply_msg = ink_e2e::build_message::<Erc20Ref>(contract_acc_id.clone())
                        .call(|erc20| erc20.total_supply());
                },
                vec![
                    (
                        "ink_e2e::create_call_builder::<Erc20Ref>",
                        Some("<-ink_e2e::build_message::<Erc20Ref>"),
                        Some("ink_e2e::build_message::<Erc20Ref>"),
                    ),
                    (
                        "total_supply()",
                        Some("<-call(|erc20| erc20.total_supply())"),
                        Some("call(|erc20| erc20.total_supply())"),
                    ),
                ],
            ),
            // MessageBuilder::from_account_id(..).call(..)
            (
                quote! { use ink_e2e::MessageBuilder; },
                quote! {
                    let message =
                        MessageBuilder::<crate::EnvironmentWithManyTopics, TopicsRef>::from_account_id(
                            contract_acc_id,
                        )
                        .call(|caller| caller.trigger());
                },
                vec![
                    (
                        "ink_e2e::create_call_builder::<TopicsRef>",
                        Some("<-MessageBuilder::"),
                        Some("from_account_id"),
                    ),
                    (
                        "trigger()",
                        Some("<-call(|caller| caller.trigger())"),
                        Some("call(|caller| caller.trigger())"),
                    ),
                ],
            ),
            (
                quote! {},
                quote! {
                    let message =
                        ink_e2e::MessageBuilder::<crate::EnvironmentWithManyTopics, TopicsRef>::from_account_id(
                            contract_acc_id,
                        )
                        .call(|caller| caller.trigger());
                },
                vec![
                    (
                        "ink_e2e::create_call_builder::<TopicsRef>",
                        Some("<-ink_e2e::MessageBuilder::"),
                        Some("from_account_id"),
                    ),
                    (
                        "trigger()",
                        Some("<-call(|caller| caller.trigger())"),
                        Some("call(|caller| caller.trigger())"),
                    ),
                ],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #imports

                type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                #[ink_e2e::test]
                async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {
                    #code

                    Ok(())
                }
            };

            let mut results = Vec::new();
            let e2e_test = parse_first_ink_entity_of_type::<InkE2ETest>(&code);
            fn_body(&mut results, &e2e_test);

            assert_eq!(results, text_edits_from_fixtures(&code, expected_results));
        }
    }

    #[test]
    fn deprecated_imports_works() {
        for (code, expected_results) in [
            (quote! {}, vec![]),
            (
                quote! { use ink_e2e::build_message; },
                vec![(
                    "",
                    Some("<-use ink_e2e::build_message;"),
                    Some("use ink_e2e::build_message;"),
                )],
            ),
            (
                quote! { use ink_e2e::MessageBuilder; },
                vec![(
                    "",
                    Some("<-use ink_e2e::MessageBuilder;"),
                    Some("use ink_e2e::MessageBuilder;"),
                )],
            ),
            (
                quote! {
                    use ink_e2e::build_message;
                    use ink_e2e::MessageBuilder;
                },
                vec![
                    (
                        "",
                        Some("<-use ink_e2e::build_message;"),
                        Some("use ink_e2e::build_message;"),
                    ),
                    (
                        "",
                        Some("<-use ink_e2e::MessageBuilder;"),
                        Some("use ink_e2e::MessageBuilder;"),
                    ),
                ],
            ),
            (
                quote! { use ink_e2e::{build_message, MessageBuilder}; },
                vec![(
                    "",
                    Some("<-use ink_e2e::{build_message, MessageBuilder};"),
                    Some("use ink_e2e::{build_message, MessageBuilder};"),
                )],
            ),
            (
                quote! {
                    use ink_e2e::build_message;
                    use ink_e2e::Client;
                    use ink_e2e::MessageBuilder;
                },
                vec![
                    (
                        "",
                        Some("<-use ink_e2e::build_message;"),
                        Some("use ink_e2e::build_message;"),
                    ),
                    (
                        "",
                        Some("<-use ink_e2e::MessageBuilder;"),
                        Some("use ink_e2e::MessageBuilder;"),
                    ),
                ],
            ),
            (
                quote! { use ink_e2e::{build_message, Client, MessageBuilder}; },
                vec![
                    ("", Some("<-build_message,"), Some("build_message,")),
                    ("", Some("<-, MessageBuilder"), Some("MessageBuilder")),
                ],
            ),
            (
                quote! { use ink_e2e::{build_message, Client, Keypair, MessageBuilder}; },
                vec![
                    ("", Some("<-build_message,"), Some("build_message,")),
                    ("", Some("<-, MessageBuilder"), Some("MessageBuilder")),
                ],
            ),
            (
                quote! {
                    use {
                        ink_e2e::build_message,
                        ink::env::Environment,
                    };
                },
                vec![(
                    "",
                    Some("<-ink_e2e::build_message,"),
                    Some("ink_e2e::build_message,"),
                )],
            ),
            (
                quote! {
                    use {
                        ink_e2e::build_message,
                        ink_e2e::MessageBuilder,
                    };
                },
                vec![("", Some("<-use {"), Some("};"))],
            ),
            (
                quote! {
                    use ink_e2e::Client;
                    use ink_e2e::Keypair;
                },
                vec![],
            ),
            (
                quote! {
                    use ink_e2e::{Client, Keypair};
                },
                vec![],
            ),
            (
                quote! {
                    use ink_e2e::*;
                },
                vec![],
            ),
        ]
        .into_iter()
        .flat_map(|(imports, expected_results)| {
            [
                (
                    quote_as_pretty_string! {
                        #[cfg(all(test, feature = "e2e-tests"))]
                        mod e2e_tests {
                            #imports

                            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                            #[ink_e2e::test]
                            async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) ->
                                E2EResult<()> {
                                Ok(())
                            }
                        }
                    },
                    expected_results.clone(),
                ),
                (
                    quote_as_pretty_string! {
                        #imports

                        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                        #[ink_e2e::test]
                        async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {
                            Ok(())
                        }
                    },
                    expected_results,
                ),
            ]
        }) {
            let mut results = Vec::new();
            let e2e_test = parse_first_ink_entity_of_type::<InkE2ETest>(&code);
            deprecated_imports(&mut results, &e2e_test);

            assert_eq!(results, text_edits_from_fixtures(&code, expected_results));
        }
    }
}
