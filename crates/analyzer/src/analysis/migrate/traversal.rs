//! Utilities for traversing items.

use ink_analyzer_ir::ast;
use ink_analyzer_ir::ast::{HasArgList, HasLoopBody, RangeItem};

macro_rules! visit_sub_expr {
    ($visitor: ident, $expr: expr) => {
        if let Some(expr) = $expr.expr() {
            $visitor.visit_expr(&expr);
        }
    };
}

macro_rules! visit_sub_expr_opt {
    ($visitor: ident, $expr_opt: expr) => {
        if let Some(expr) = $expr_opt {
            $visitor.visit_expr(&expr);
        }
    };
}

macro_rules! visit_sub_exprs_iter {
    ($visitor: ident, $exprs: expr) => {
        for expr in $exprs {
            $visitor.visit_expr(&expr);
        }
    };
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &ast::Expr) {
    match expr {
        // Handle complex expressions.
        ast::Expr::ArrayExpr(expr) => match expr.kind() {
            ast::ArrayExprKind::ElementList(elems) => {
                visit_sub_exprs_iter!(visitor, elems);
            }
            ast::ArrayExprKind::Repeat { initializer, .. } => {
                visit_sub_expr_opt!(visitor, initializer);
            }
        },
        ast::Expr::BinExpr(expr) => {
            visit_sub_expr_opt!(visitor, expr.lhs());
            visit_sub_expr_opt!(visitor, expr.rhs());
        }
        ast::Expr::BlockExpr(expr) => {
            visitor.visit_block(expr);
        }
        ast::Expr::CallExpr(expr) => {
            visitor.visit_call(expr);
        }
        ast::Expr::ClosureExpr(expr) => {
            visit_sub_expr_opt!(visitor, expr.body());
        }
        ast::Expr::ForExpr(expr) => {
            if let Some(body) = expr.loop_body() {
                visitor.visit_block(&body);
            }
        }
        ast::Expr::FormatArgsExpr(expr) => {
            visit_sub_expr_opt!(visitor, expr.template());
            let args = expr.args().filter_map(|arg| arg.expr());
            visit_sub_exprs_iter!(visitor, args);
        }
        ast::Expr::IfExpr(expr) => {
            visitor.visit_if_expr(expr);
        }
        ast::Expr::IndexExpr(expr) => {
            visit_sub_expr_opt!(visitor, expr.base());
            visit_sub_expr_opt!(visitor, expr.index());
        }
        ast::Expr::LoopExpr(expr) => {
            if let Some(body) = expr.loop_body() {
                visitor.visit_block(&body);
            }
        }
        ast::Expr::MatchExpr(expr) => {
            visit_sub_expr!(visitor, expr);
            if let Some(arms) = expr.match_arm_list().as_ref().map(ast::MatchArmList::arms) {
                let arms_exprs = arms.filter_map(|arm| arm.expr());
                visit_sub_exprs_iter!(visitor, arms_exprs);
            }
        }
        ast::Expr::MethodCallExpr(expr) => {
            visitor.visit_method_call(expr);
        }
        ast::Expr::RangeExpr(expr) => {
            visit_sub_expr_opt!(visitor, expr.start());
            visit_sub_expr_opt!(visitor, expr.end());
        }
        ast::Expr::RecordExpr(expr) => {
            if let Some(field_list) = expr.record_expr_field_list() {
                let fields = field_list.fields().filter_map(|field| field.expr());
                visit_sub_exprs_iter!(visitor, fields);

                visit_sub_expr_opt!(visitor, field_list.spread());
            }
        }
        ast::Expr::TupleExpr(expr) => {
            visit_sub_exprs_iter!(visitor, expr.fields());
        }
        ast::Expr::WhileExpr(expr) => {
            if let Some(body) = expr.loop_body() {
                visitor.visit_block(&body);
            }
        }
        // Handle simple expression wrappers.
        ast::Expr::AwaitExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::BreakExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::CastExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::FieldExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::LetExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::ParenExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::PrefixExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::RefExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::ReturnExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::TryExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::YieldExpr(expr) => visit_sub_expr!(visitor, expr),
        ast::Expr::YeetExpr(expr) => visit_sub_expr!(visitor, expr),
        // Handle terminal expressions.
        _ => (),
    };
}

pub fn walk_block<V: Visitor>(visitor: &mut V, block_expr: &ast::BlockExpr) {
    let Some(stmt_list) = block_expr.stmt_list() else {
        return;
    };
    for stmt in stmt_list.statements() {
        let expr = match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => expr_stmt.expr(),
            ast::Stmt::LetStmt(let_stmt) => let_stmt.initializer(),
            ast::Stmt::Item(_) => None,
        };

        if let Some(expr) = expr {
            visitor.visit_expr(&expr);
        }
    }
}

pub fn walk_call<V: Visitor>(visitor: &mut V, expr: &ast::CallExpr) {
    if let Some(call) = expr.expr() {
        visitor.visit_expr(&call);
    }
    if let Some(args) = expr.arg_list().as_ref().map(ast::ArgList::args) {
        visit_sub_exprs_iter!(visitor, args);
    }
}

pub fn walk_if_expr<V: Visitor>(visitor: &mut V, if_expr: &ast::IfExpr) {
    if let Some(expr) = if_expr.then_branch() {
        visitor.visit_block(&expr);
    }
    if let Some(branch) = if_expr.else_branch() {
        match branch {
            ast::ElseBranch::Block(expr) => visitor.visit_block(&expr),
            ast::ElseBranch::IfExpr(expr) => visitor.visit_if_expr(&expr),
        }
    }
}

pub fn walk_method_call<V: Visitor>(visitor: &mut V, expr: &ast::MethodCallExpr) {
    if let Some(receiver) = expr.receiver() {
        visitor.visit_expr(&receiver);
    }
    if let Some(args) = expr.arg_list().as_ref().map(ast::ArgList::args) {
        visit_sub_exprs_iter!(visitor, args);
    }
}

pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &ast::Expr) {
        walk_expr(self, expr);
    }

    fn visit_block(&mut self, expr: &ast::BlockExpr) {
        walk_block(self, expr);
    }

    fn visit_call(&mut self, expr: &ast::CallExpr) {
        walk_call(self, expr);
    }

    fn visit_if_expr(&mut self, expr: &ast::IfExpr) {
        walk_if_expr(self, expr);
    }

    fn visit_method_call(&mut self, expr: &ast::MethodCallExpr) {
        walk_method_call(self, expr);
    }
}
