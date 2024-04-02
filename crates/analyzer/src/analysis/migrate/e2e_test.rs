//! ink! 5.0 e2e test migration.

use std::collections::HashSet;
use std::fmt;

use ink_analyzer_ir::ast::{HasArgList, HasDocComments, HasLoopBody, HasName, RangeItem};
use ink_analyzer_ir::syntax::{AstNode, AstToken, TextRange};
use ink_analyzer_ir::{ast, Contract, InkE2ETest, InkEntity, InkFile, IsInkFn};
use itertools::Itertools;

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
            .or(fn_item.name().map(|name| {
                // Default to inserting at end of function name (if any).
                let name_end = name.syntax().text_range().end();
                TextRange::new(name_end, name_end)
            }));
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

    let mut required_traits = HashSet::new();
    process_block_expr(results, &mut required_traits, &body);

    // Add required trait imports (if any).
    if required_traits.is_empty() {
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
        for e2e_trait in required_traits {
            results.push(TextEdit::insert(
                format!("\n{indent}use {e2e_trait};"),
                insert_offset,
            ));
        }
    }
}

fn process_block_expr(
    results: &mut Vec<TextEdit>,
    required_traits: &mut HashSet<E2ETrait>,
    block_expr: &ast::BlockExpr,
) {
    let Some(stmt_list) = block_expr.stmt_list() else {
        return;
    };
    for stmt in stmt_list.statements() {
        let mut top_expr = match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => expr_stmt.expr(),
            ast::Stmt::LetStmt(let_stmt) => let_stmt.initializer(),
            ast::Stmt::Item(_) => None,
        };

        let mut prev_sub_exprs = Vec::new();
        loop {
            let mut prev_sub_exprs_iter = prev_sub_exprs.into_iter();
            let mut curr_sub_exprs = Vec::new();

            while let Some(expr) = top_expr.take().or(prev_sub_exprs_iter.next()) {
                process_expr(results, required_traits, &mut curr_sub_exprs, &expr);
            }

            if curr_sub_exprs.is_empty() {
                // Processing nested expressions is complete.
                break;
            }
            prev_sub_exprs = curr_sub_exprs;
        }
    }
}

fn process_expr(
    results: &mut Vec<TextEdit>,
    required_traits: &mut HashSet<E2ETrait>,
    sub_exprs: &mut Vec<ast::Expr>,
    expr: &ast::Expr,
) {
    macro_rules! collect_sub_expr {
        ($expr: expr, $sub_exprs: expr) => {
            if let Some(expr) = $expr.expr() {
                $sub_exprs.push(expr);
            }
        };
    }

    match expr {
        // Handle method and call migrations.
        ast::Expr::MethodCallExpr(expr) => {
            process_method_call(results, required_traits, sub_exprs, expr);
        }
        ast::Expr::CallExpr(expr) => {
            process_call(results, sub_exprs, expr);
        }
        // Handle nested expressions.
        ast::Expr::ArrayExpr(expr) => match expr.kind() {
            ast::ArrayExprKind::ElementList(elems) => {
                sub_exprs.extend(elems);
            }
            ast::ArrayExprKind::Repeat { initializer, .. } => {
                if let Some(init) = initializer {
                    sub_exprs.push(init);
                }
            }
        },
        ast::Expr::BinExpr(expr) => {
            if let Some(lhs) = expr.lhs() {
                sub_exprs.push(lhs);
            }
            if let Some(rhs) = expr.rhs() {
                sub_exprs.push(rhs);
            }
        }
        ast::Expr::BlockExpr(expr) => {
            process_block_expr(results, required_traits, expr);
        }
        ast::Expr::ClosureExpr(expr) => {
            if let Some(body) = expr.body() {
                sub_exprs.push(body);
            }
        }
        ast::Expr::ForExpr(expr) => {
            if let Some(body) = expr.loop_body() {
                process_block_expr(results, required_traits, &body);
            }
        }
        ast::Expr::FormatArgsExpr(expr) => {
            if let Some(template) = expr.template() {
                sub_exprs.push(template);
            }
            sub_exprs.extend(expr.args().filter_map(|arg| arg.expr()));
        }
        ast::Expr::IfExpr(expr) => {
            process_if_expr(results, required_traits, expr);
        }
        ast::Expr::IndexExpr(expr) => {
            if let Some(base) = expr.base() {
                sub_exprs.push(base);
            }
            if let Some(index) = expr.index() {
                sub_exprs.push(index);
            }
        }
        ast::Expr::LoopExpr(expr) => {
            if let Some(body) = expr.loop_body() {
                process_block_expr(results, required_traits, &body);
            }
        }
        ast::Expr::MatchExpr(expr) => {
            if let Some(scrutinee) = expr.expr() {
                sub_exprs.push(scrutinee);
            }
            if let Some(arms) = expr.match_arm_list().as_ref().map(ast::MatchArmList::arms) {
                sub_exprs.extend(arms.filter_map(|arm| arm.expr()));
            }
        }
        ast::Expr::RangeExpr(expr) => {
            if let Some(start) = expr.start() {
                sub_exprs.push(start);
            }
            if let Some(end) = expr.end() {
                sub_exprs.push(end);
            }
        }
        ast::Expr::RecordExpr(expr) => {
            if let Some(field_list) = expr.record_expr_field_list() {
                sub_exprs.extend(field_list.fields().filter_map(|field| field.expr()));

                if let Some(spread) = field_list.spread() {
                    sub_exprs.push(spread);
                }
            }
        }
        ast::Expr::TupleExpr(expr) => {
            sub_exprs.extend(expr.fields());
        }
        ast::Expr::WhileExpr(expr) => {
            if let Some(body) = expr.loop_body() {
                process_block_expr(results, required_traits, &body);
            }
        }
        // Handle simple expression wrappers.
        ast::Expr::AwaitExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::BreakExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::CastExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::FieldExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::LetExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::ParenExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::PrefixExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::RefExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::ReturnExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::TryExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::YieldExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        ast::Expr::YeetExpr(expr) => collect_sub_expr!(expr, sub_exprs),
        // Handle terminal expressions.
        _ => (),
    };
}

fn process_method_call(
    results: &mut Vec<TextEdit>,
    required_traits: &mut HashSet<E2ETrait>,
    sub_exprs: &mut Vec<ast::Expr>,
    expr: &ast::MethodCallExpr,
) {
    let mut process_nested_exprs = || {
        if let Some(receiver) = expr.receiver() {
            sub_exprs.push(receiver);
        }
        if let Some(args) = expr.arg_list().as_ref().map(ast::ArgList::args) {
            sub_exprs.extend(args);
        }
    };

    let is_client_receiver = expr
        .receiver()
        .and_then(|receiver| match receiver {
            ast::Expr::PathExpr(path) => Some(path),
            _ => None,
        })
        .is_some_and(|receiver| receiver.to_string() == "client");
    if !is_client_receiver {
        process_nested_exprs();
    }

    let Some(method_name) = expr.name_ref() else {
        process_nested_exprs();
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
                results.push(TextEdit::replace(
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
                    unwrap_dry_run_result(results, expr);
                }

                required_traits.insert(E2ETrait::Contracts);
            }
        }
        name @ ("call" | "call_dry_run") => {
            if let Some((signer, message, value, storage_deposit_limit)) = args.collect_tuple() {
                results.push(TextEdit::replace(
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
                    unwrap_dry_run_result(results, expr);
                }

                required_traits.insert(E2ETrait::Contracts);
            }
        }
        "upload" => {
            if let Some((contract_name, signer, storage_deposit_limit)) = args.collect_tuple() {
                results.push(TextEdit::replace(
                    format!(
                        "({contract_name}, {signer}){}.submit()",
                        storage_deposit_limit_call(&storage_deposit_limit)
                            .as_deref()
                            .unwrap_or_default(),
                    ),
                    arg_list.syntax().text_range(),
                ));

                required_traits.insert(E2ETrait::Contracts);
            }
        }
        "create_and_fund_account" => {
            if args.count() == 2 {
                required_traits.insert(E2ETrait::Chain);
            }
        }
        "balance" => {
            if args.count() == 1 {
                results.push(TextEdit::replace(
                    "free_balance".to_owned(),
                    method_name.syntax().text_range(),
                ));

                required_traits.insert(E2ETrait::Chain);
            }
        }
        "runtime_call" => {
            if args.count() == 4 {
                required_traits.insert(E2ETrait::Chain);
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

fn process_call(results: &mut Vec<TextEdit>, sub_exprs: &mut Vec<ast::Expr>, expr: &ast::CallExpr) {
    let mut process_nested_exprs = || {
        if let Some(call) = expr.expr() {
            sub_exprs.push(call);
        }
        if let Some(args) = expr.arg_list().as_ref().map(ast::ArgList::args) {
            sub_exprs.extend(args);
        }
    };

    let Some(message_builder_call) = MessageBuilderCall::parse(expr) else {
        process_nested_exprs();
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
    results.push(TextEdit::replace(
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
    let Some((ast::Expr::ClosureExpr(callback),)) = chained_call_method_args.collect_tuple() else {
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
    results.push(TextEdit::replace(
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

fn process_if_expr(
    results: &mut Vec<TextEdit>,
    required_traits: &mut HashSet<E2ETrait>,
    if_expr: &ast::IfExpr,
) {
    if let Some(expr) = if_expr.then_branch() {
        process_block_expr(results, required_traits, &expr);
    }
    if let Some(branch) = if_expr.else_branch() {
        match branch {
            ast::ElseBranch::Block(expr) => process_block_expr(results, required_traits, &expr),
            ast::ElseBranch::IfExpr(expr) => process_if_expr(results, required_traits, &expr),
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
        let simplify_path = |path: &ast::Path, generic_args: &ast::GenericArgList| {
            let simple_path = path.to_string().replace(&generic_args.to_string(), "");
            ink_analyzer_ir::path_from_str(&simple_path)
        };
        let is_build_message_call_path = |path: &ast::Path| {
            let fn_path = build_message_generic_args(path)
                .and_then(|generic_args| simplify_path(path, &generic_args));
            resolution::is_external_crate_item(
                "build_message",
                fn_path.as_ref().unwrap_or(path),
                &["ink_e2e"],
                expr.syntax(),
            )
        };
        let is_message_builder_from_account_id_call_path = |path: &ast::Path| {
            let is_from_account_id_call = path
                .segment()
                .is_some_and(|segment| segment.to_string() == "from_account_id");
            if is_from_account_id_call {
                if let Some(qualifier) = path.qualifier() {
                    let type_path = message_builder_generic_args(&qualifier)
                        .and_then(|generic_args| simplify_path(&qualifier, &generic_args));
                    return resolution::is_external_crate_item(
                        "MessageBuilder",
                        type_path.as_ref().unwrap_or(&qualifier),
                        &["ink_e2e"],
                        expr.syntax(),
                    );
                }
            }
            false
        };

        expr.expr()
            .and_then(|call_expr| match call_expr {
                ast::Expr::PathExpr(path) => path.path(),
                _ => None,
            })
            .and_then(|path| {
                if is_build_message_call_path(&path) {
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
            MessageBuilderCall::BuildMessageFn(path) => build_message_generic_args(path),
            MessageBuilderCall::MessageBuilderFromAccountId(path) => {
                message_builder_generic_args(path)
            }
        }
    }
}

fn build_message_generic_args(path: &ast::Path) -> Option<ast::GenericArgList> {
    path.segment()
        .as_ref()
        .and_then(ast::PathSegment::generic_arg_list)
}

fn message_builder_generic_args(path: &ast::Path) -> Option<ast::GenericArgList> {
    path.segments().find_map(|segment| {
        if segment.to_string().starts_with("MessageBuilder::") {
            segment.generic_arg_list()
        } else {
            None
        }
    })
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
}
