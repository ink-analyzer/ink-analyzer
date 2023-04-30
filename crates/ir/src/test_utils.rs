//! Test utilities for IR types and abstractions.

#![cfg(test)]

use crate::InkAttribute;
use ra_ap_syntax::ast::Attr;
use ra_ap_syntax::{AstNode, SourceFile};

pub fn parse_first_attribute(code: &str) -> Attr {
    SourceFile::parse(code)
        .tree()
        .syntax()
        .descendants()
        .find_map(Attr::cast)
        .unwrap()
}

pub fn parse_first_ink_attribute(code: &str) -> InkAttribute {
    SourceFile::parse(code)
        .tree()
        .syntax()
        .descendants()
        .find_map(|node| InkAttribute::cast(Attr::cast(node)?))
        .unwrap()
}
