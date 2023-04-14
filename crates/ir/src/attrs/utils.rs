//! ink! attribute IR utilities.

use itertools::Itertools;
use ra_ap_syntax::ast::{Attr, Ident, PathSegment};
use ra_ap_syntax::{AstNode, AstToken, SyntaxElement, SyntaxToken, T};

use crate::{MetaArg, MetaOption, MetaSeparator, MetaValue};

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
pub fn get_args(attr: &Attr) -> Vec<MetaArg> {
    let mut args: Vec<MetaArg> = Vec::new();

    if let Some(meta) = attr.meta() {
        if let Some(token_tree) = meta.token_tree() {
            let l_paren = token_tree.l_paren_token();
            let r_paren = token_tree.r_paren_token();
            args = token_tree
                .syntax()
                .children_with_tokens()
                // Skip starting parenthesis if present
                .skip(if l_paren.is_some() { 1 } else { 0 })
                // Ignore closing parenthesis if present
                .take_while(|it| r_paren.is_none() || it.as_token() != r_paren.as_ref())
                // Comma separated groups
                .group_by(|token| token.kind() == T![,])
                .into_iter()
                .filter_map(|(is_sep, group)| (!is_sep).then_some(group))
                .map(|group| {
                    let arg_elems: Vec<SyntaxElement> = group.into_iter().collect();
                    if arg_elems.is_empty() {
                        // Empty argument
                        MetaArg::default()
                    } else {
                        let arg_item_groups: Vec<Vec<SyntaxElement>> = arg_elems
                            .into_iter()
                            // Equal sign (=) separated groups
                            .group_by(|token| token.kind() == T![=])
                            .into_iter()
                            .map(|(_, group)| group.into_iter().collect())
                            .collect();

                        let empty = Vec::new();
                        let arg_name = arg_item_groups.get(0).unwrap_or(&empty);
                        let arg_eq = arg_item_groups.get(1).unwrap_or(&empty);
                        let arg_value = arg_item_groups.get(2).unwrap_or(&empty);

                        MetaArg {
                            name: get_arg_name(arg_name),
                            eq: get_arg_eq(arg_eq),
                            value: get_arg_value(arg_value),
                        }
                    }
                })
                .collect();
        }
    }

    args
}

fn only_non_trivia_elements(elems: &[SyntaxElement]) -> Vec<&SyntaxElement> {
    elems
        .iter()
        .filter_map(|elem| (!elem.kind().is_trivia()).then_some(elem))
        .collect()
}

fn get_token_at_index(elems: Vec<&SyntaxElement>, idx: usize) -> Option<&SyntaxToken> {
    elems.get(idx)?.as_token()
}

fn get_arg_name(elems: &[SyntaxElement]) -> MetaOption<Ident> {
    let non_trivia_elems = only_non_trivia_elements(elems);
    match non_trivia_elems.len() {
        0 => MetaOption::None,
        1 => {
            let mut name = MetaOption::Err(elems.to_owned());
            if let Some(token) = get_token_at_index(non_trivia_elems, 0) {
                if let Some(ident) = Ident::cast(token.to_owned()) {
                    name = MetaOption::Ok(ident);
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
        MetaSeparator::cast(get_token_at_index(non_trivia_elems, 0)?.to_owned())
    } else {
        None
    }
}

fn get_arg_value(elems: &Vec<SyntaxElement>) -> MetaOption<MetaValue> {
    if elems.is_empty() {
        MetaOption::None
    } else if let Some(path_or_lit) = MetaValue::parse(elems.clone()) {
        MetaOption::Ok(path_or_lit)
    } else {
        MetaOption::Err(elems.clone())
    }
}

// TODO: Add unit tests
