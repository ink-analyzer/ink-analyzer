//! ink! attribute IR utilities.

use itertools::Itertools;
use ra_ap_syntax::ast::{Attr, TokenTree};
use ra_ap_syntax::{AstNode, AstToken, SyntaxElement, SyntaxToken, T};

use crate::meta::{MetaName, MetaNameValue, MetaOption, MetaSeparator, MetaValue};
use crate::{InkArg, InkArgKind};

/// Parse ink! attribute arguments.
pub fn parse_ink_args(attr: &Attr) -> Vec<InkArg> {
    if let Some(meta) = attr.meta() {
        if let Some(token_tree) = meta.token_tree() {
            return parse_args(&token_tree)
                .into_iter()
                .map(InkArg::from)
                .collect();
        }
    }
    Vec::new()
}

/// Sort ink! attribute arguments so that we choose the best `InkArgKind` for ink! attributes
/// regardless of their actual ordering in source code.
/// e.g the kind for `#[ink(selector=1, payable, message)]` should still be `InkArgKind::Message`.
pub fn sort_ink_args_by_kind(args: &[InkArg]) -> Vec<InkArg> {
    let mut sorted_args = args.to_owned();
    sorted_args.sort_by_key(|arg| {
        match arg.kind() {
            // Required (e.g `#[ink(storage)]`) and/or standalone (e.g `#[ink(event)]`)
            // arguments get highest priority.
            InkArgKind::Constructor
            | InkArgKind::Event
            | InkArgKind::Extension
            | InkArgKind::Impl
            | InkArgKind::Message
            | InkArgKind::Namespace
            | InkArgKind::Storage
            | InkArgKind::Topic => 0,
            // Macro-specific arguments get the next priority level.
            InkArgKind::Env | InkArgKind::KeepAttr => 1,
            // Everything else gets the lowest priority.
            // This group includes optional attributes (e.g anonymous, payable, selector e.t.c)
            _ => 2,
        }
    });
    sorted_args
}

/// Parse attribute arguments.
pub fn parse_args(token_tree: &TokenTree) -> Vec<MetaNameValue> {
    let l_paren = token_tree.l_paren_token();
    let r_paren = token_tree.r_paren_token();

    let mut last_separator_offset = if let Some(start_token) = l_paren.as_ref() {
        start_token.text_range().end()
    } else {
        token_tree.syntax().text_range().start()
    };

    token_tree
        .syntax()
        .children_with_tokens()
        // Skip starting parenthesis if present.
        .skip(if l_paren.is_some() { 1 } else { 0 })
        // Ignore closing parenthesis if present.
        .take_while(|it| r_paren.is_none() || it.as_token() != r_paren.as_ref())
        // Comma separated groups.
        .group_by(|token| token.kind() == T![,])
        .into_iter()
        //.filter_map(|(is_sep, group)| (!is_sep || true).then_some(group))
        .filter_map(|(is_sep, mut group)| {
            if is_sep {
                // This is the comma token, so we update last separator offset.
                last_separator_offset = group.next().unwrap().text_range().end();
                None
            } else {
                let arg_elems: Vec<SyntaxElement> = group.into_iter().collect();
                if arg_elems.is_empty() {
                    // Empty argument.
                    // Use last operator offset as the offset for the empty argument.
                    Some(MetaNameValue::empty(last_separator_offset))
                } else {
                    let arg_item_groups: Vec<Vec<SyntaxElement>> = arg_elems
                        .into_iter()
                        // Equal sign (=) separated groups.
                        .group_by(|token| token.kind() == T![=])
                        .into_iter()
                        .map(|(_, group)| group.into_iter().collect())
                        .collect();

                    let empty = Vec::new();
                    let arg_name = arg_item_groups.get(0).unwrap_or(&empty);
                    let arg_eq = arg_item_groups.get(1).unwrap_or(&empty);
                    let arg_value = arg_item_groups.get(2).unwrap_or(&empty);

                    Some(MetaNameValue::new(
                        get_arg_name(arg_name),
                        get_arg_eq(arg_eq),
                        get_arg_value(arg_value),
                        last_separator_offset,
                    ))
                }
            }
        })
        .collect()
}

fn only_non_trivia_elements(elems: &[SyntaxElement]) -> Vec<&SyntaxElement> {
    elems
        .iter()
        .filter_map(|elem| (!elem.kind().is_trivia()).then_some(elem))
        .collect()
}

fn get_token_at_index<'a>(elems: &'a [&SyntaxElement], idx: usize) -> Option<&'a SyntaxToken> {
    elems.get(idx)?.as_token()
}

fn get_arg_name(elems: &[SyntaxElement]) -> MetaOption<MetaName> {
    let non_trivia_elems = only_non_trivia_elements(elems);
    match non_trivia_elems.len() {
        0 => MetaOption::None,
        1 => {
            let mut name = MetaOption::Err(elems.to_owned());
            if let Some(token) = get_token_at_index(&non_trivia_elems, 0) {
                if let Some(meta_name) = MetaName::cast(token.to_owned()) {
                    name = MetaOption::Ok(meta_name);
                }
            }
            name
        }
        _ => MetaOption::Err(elems.to_owned()),
    }
}

fn get_arg_eq(elems: &[SyntaxElement]) -> Option<MetaSeparator> {
    let non_trivia_elems = only_non_trivia_elements(elems);
    if non_trivia_elems.len() == 1 {
        MetaSeparator::cast(get_token_at_index(&non_trivia_elems, 0)?.to_owned())
    } else {
        None
    }
}

fn get_arg_value(elems: &[SyntaxElement]) -> MetaOption<MetaValue> {
    if elems.is_empty() {
        MetaOption::None
    } else if let Some(path_or_lit) = MetaValue::parse(elems.to_owned()) {
        MetaOption::Ok(path_or_lit)
    } else {
        MetaOption::Err(elems.to_owned())
    }
}

// TODO: Add unit tests
