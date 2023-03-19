//! Utilities for creating ink! attribute intermediate representations (IRs) and abstractions.
use ra_ap_syntax::ast::{Attr, PathSegment};
use ra_ap_syntax::{AstNode, SyntaxKind};

/// Parse attribute path segments.
pub fn get_path_segments(attr: &Attr) -> Vec<String> {
    let mut path_segments: Vec<String> = Vec::new();

    if let Some(meta) = attr.meta() {
        if let Some(path) = meta.path() {
            path_segments = path
                .syntax()
                .descendants()
                .filter_map(PathSegment::cast)
                .map(|item| item.syntax().text().to_string())
                .collect();
        }
    }

    path_segments
}

/// Parse attribute arguments.
pub fn get_args(attr: &Attr) -> Vec<String> {
    let mut args: Vec<String> = Vec::new();

    if let Some(meta) = attr.meta() {
        if let Some(token_tree) = meta.token_tree() {
            for item in token_tree.syntax().descendants_with_tokens() {
                if let Some(token) = item.as_token() {
                    if token.kind() == SyntaxKind::IDENT {
                        args.push(token.text().to_string());
                        // TODO: Parse argument values
                    }
                }
            }
        }
    }

    args
}

// TODO: Add unit tests
