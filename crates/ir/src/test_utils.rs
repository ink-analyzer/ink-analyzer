//! Test utilities for IR types and abstractions.

#![cfg(test)]

use ra_ap_syntax::ast::Attr;
use ra_ap_syntax::{AstNode, SourceFile};

pub fn get_first_attribute(code: &str) -> Attr {
    SourceFile::parse(code)
        .tree()
        .syntax()
        .descendants()
        .find_map(Attr::cast)
        .unwrap()
}
