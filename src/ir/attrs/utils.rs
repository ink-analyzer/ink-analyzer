//! Utilities for creating ink! attribute intermediate representations (IRs) and abstractions.
use ra_ap_syntax::ast::{Attr, Ident, PathSegment};
use ra_ap_syntax::{AstNode, AstToken};

/// Parse attribute path segments.
pub fn get_path_segments(attr: &Attr) -> Vec<PathSegment> {
    let mut path_segments: Vec<PathSegment> = Vec::new();

    if let Some(meta) = attr.meta() {
        if let Some(path) = meta.path() {
            path_segments = path
                .syntax()
                .descendants()
                .filter_map(PathSegment::cast)
                .collect();
        }
    }

    path_segments
}

/// Parse attribute arguments.
pub fn get_args(attr: &Attr) -> Vec<Ident> {
    let mut args: Vec<Ident> = Vec::new();

    if let Some(meta) = attr.meta() {
        if let Some(token_tree) = meta.token_tree() {
            args = token_tree
                .syntax()
                .descendants_with_tokens()
                .filter_map(|item| item.into_token())
                .filter_map(Ident::cast)
                .collect();
        }
    }

    args
}

// TODO: Add unit tests
