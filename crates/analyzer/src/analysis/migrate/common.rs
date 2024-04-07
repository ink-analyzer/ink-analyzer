//! Utilities for ink! 5.0 migration.

use ink_analyzer_ir::ast;
use ink_analyzer_ir::syntax::SyntaxNode;

use crate::resolution;

pub fn last_segment_generic_args(path: &ast::Path) -> Option<ast::GenericArgList> {
    path.segment()
        .as_ref()
        .and_then(ast::PathSegment::generic_arg_list)
}

pub fn simplify_path(path: &ast::Path, generic_args: &ast::GenericArgList) -> Option<ast::Path> {
    let simple_path = path.to_string().replace(&generic_args.to_string(), "");
    ink_analyzer_ir::path_from_str(&simple_path)
}

pub fn is_call_path(
    path: &ast::Path,
    name: &str,
    qualifiers: &[&str],
    ref_node: &SyntaxNode,
) -> bool {
    let fn_path =
        last_segment_generic_args(path).and_then(|generic_args| simplify_path(path, &generic_args));
    resolution::is_external_crate_item(name, fn_path.as_ref().unwrap_or(path), qualifiers, ref_node)
}
