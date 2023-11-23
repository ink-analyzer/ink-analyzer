//! ink! attribute IR utilities.

use itertools::Itertools;
use ra_ap_syntax::{ast, AstNode, AstToken, SyntaxElement, T};

use crate::meta::{MetaName, MetaNameValue, MetaOption, MetaSeparator, MetaValue};
use crate::InkArg;

/// Parse ink! attribute arguments.
pub fn parse_ink_args(attr: &ast::Attr) -> Vec<InkArg> {
    if let Some(token_tree) = attr.token_tree() {
        parse_meta_items(&token_tree)
            .into_iter()
            .map(InkArg::from)
            .collect()
    } else {
        Vec::new()
    }
}

// Parse meta items.
fn parse_meta_items(token_tree: &ast::TokenTree) -> Vec<MetaNameValue> {
    let l_paren = token_tree.l_paren_token();
    let r_paren = token_tree.r_paren_token();

    let mut last_separator_offset = match l_paren.as_ref() {
        Some(start_token) => start_token.text_range().end(),
        None => token_tree.syntax().text_range().start(),
    };

    token_tree
        .syntax()
        .children_with_tokens()
        // Skip starting parenthesis if present.
        .skip(usize::from(l_paren.is_some()))
        // Ignore closing parenthesis if present.
        .take_while(|it| r_paren.is_none() || it.as_token() != r_paren.as_ref())
        // Comma (`,`) separated groups.
        .group_by(|token| token.kind() == T![,])
        .into_iter()
        .filter_map(|(is_comma, mut group)| {
            if is_comma {
                // This is the comma token, so we update last separator offset.
                if let Some(comma) = group.next() {
                    last_separator_offset = comma.text_range().end();
                }
                None
            } else {
                let mut arg_tokens = group;
                let mut eq = None;
                let name: Vec<_> = arg_tokens
                    .by_ref()
                    .take_while(|it| {
                        let is_sep = it.kind() == T![=];
                        if is_sep {
                            // Sets the equal sign (`=`) if its present (before its consumed).
                            eq = it.clone().into_token().and_then(MetaSeparator::cast);
                        }
                        !is_sep
                    })
                    .collect();
                let value: Vec<_> = arg_tokens.collect();

                Some(MetaNameValue::new(
                    parse_meta_name(&name),
                    eq,
                    parse_meta_value(&value),
                    last_separator_offset,
                ))
            }
        })
        .collect()
}

// Parse meta name.
fn parse_meta_name(tokens: &[SyntaxElement]) -> MetaOption<MetaName> {
    let non_trivia_tokens: Vec<_> = tokens.iter().filter(|it| !it.kind().is_trivia()).collect();
    match non_trivia_tokens.len() {
        0 => MetaOption::None,
        1 => {
            let meta_name_option = non_trivia_tokens
                .get(0)
                .cloned()
                .and_then(SyntaxElement::as_token)
                .cloned()
                .and_then(MetaName::cast);
            match meta_name_option {
                Some(meta_name) => MetaOption::Ok(meta_name),
                None => MetaOption::Err(tokens.to_owned()),
            }
        }
        _ => MetaOption::Err(tokens.to_owned()),
    }
}

// Parse meta value.
fn parse_meta_value(tokens: &[SyntaxElement]) -> MetaOption<MetaValue> {
    if tokens.is_empty() {
        MetaOption::None
    } else {
        match MetaValue::parse(tokens) {
            Some(expr) => MetaOption::Ok(expr),
            None => MetaOption::Err(tokens.to_owned()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::InkArgKind;
    use ra_ap_syntax::SyntaxKind;
    use test_utils::quote_as_str;

    #[test]
    fn parse_ink_args_works() {
        for (code, expected_args) in [
            // No arguments.
            (
                quote_as_str! {
                    #[ink::contract]
                },
                vec![],
            ),
            (
                quote_as_str! {
                    #[ink_e2e::test]
                },
                vec![],
            ),
            // Macro with arguments.
            (
                quote_as_str! {
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                },
                vec![
                    (InkArgKind::Env, Some(SyntaxKind::PATH)),
                    (InkArgKind::KeepAttr, Some(SyntaxKind::STRING)),
                ],
            ),
            (
                quote_as_str! {
                    #[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=my::env::Types, keep_attr="foo,bar")]
                },
                vec![
                    (InkArgKind::AdditionalContracts, Some(SyntaxKind::STRING)),
                    (InkArgKind::Environment, Some(SyntaxKind::PATH)),
                    (InkArgKind::KeepAttr, Some(SyntaxKind::STRING)),
                ],
            ),
            // Argument with no value.
            (
                quote_as_str! {
                    #[ink(storage)]
                },
                vec![(InkArgKind::Storage, None)],
            ),
            (
                quote_as_str! {
                    #[ink(anonymous)]
                },
                vec![(InkArgKind::Anonymous, None)],
            ),
            // Compound arguments with no value.
            (
                quote_as_str! {
                    #[ink(event, anonymous)]
                },
                vec![(InkArgKind::Event, None), (InkArgKind::Anonymous, None)],
            ),
            // Argument with integer value.
            (
                quote_as_str! {
                    #[ink(selector=1)] // Decimal.
                },
                vec![(InkArgKind::Selector, Some(SyntaxKind::INT_NUMBER))],
            ),
            (
                quote_as_str! {
                    #[ink(extension=0x1)] // Hexadecimal.
                },
                vec![(InkArgKind::Extension, Some(SyntaxKind::INT_NUMBER))],
            ),
            // Argument with wildcard/underscore value.
            (
                quote_as_str! {
                    #[ink(selector=_)]
                },
                vec![(InkArgKind::Selector, Some(SyntaxKind::UNDERSCORE))],
            ),
            // Argument with string value.
            (
                quote_as_str! {
                    #[ink(namespace="my_namespace")]
                },
                vec![(InkArgKind::Namespace, Some(SyntaxKind::STRING))],
            ),
            // Argument with boolean value.
            (
                quote_as_str! {
                    #[ink(handle_status=true)]
                },
                vec![(InkArgKind::HandleStatus, Some(SyntaxKind::TRUE_KW))],
            ),
            (
                quote_as_str! {
                    #[ink(derive=false)]
                },
                vec![(InkArgKind::Derive, Some(SyntaxKind::FALSE_KW))],
            ),
            // Argument with path value.
            (
                quote_as_str! {
                    #[ink(env=my::env::Types)]
                },
                vec![(InkArgKind::Env, Some(SyntaxKind::PATH))],
            ),
            // Compound arguments of different kinds.
            (
                quote_as_str! {
                    #[ink(message, payable, selector=1)]
                },
                vec![
                    (InkArgKind::Message, None),
                    (InkArgKind::Payable, None),
                    (InkArgKind::Selector, Some(SyntaxKind::INT_NUMBER)),
                ],
            ),
            (
                quote_as_str! {
                    #[ink(extension=1, handle_status=false)]
                },
                vec![
                    (InkArgKind::Extension, Some(SyntaxKind::INT_NUMBER)),
                    (InkArgKind::HandleStatus, Some(SyntaxKind::FALSE_KW)),
                ],
            ),
            // Unknown argument.
            (
                quote_as_str! {
                    #[ink(unknown)]
                },
                vec![(InkArgKind::Unknown, None)],
            ),
            (
                quote_as_str! {
                    #[ink(xyz)]
                },
                vec![(InkArgKind::Unknown, None)],
            ),
            (
                quote_as_str! {
                    #[ink(xyz="abc")]
                },
                vec![(InkArgKind::Unknown, Some(SyntaxKind::STRING))],
            ),
            // Non-ink argument.
            (
                quote_as_str! {
                    #[cfg_attr(not(feature = "std"), no_std)]
                },
                vec![(InkArgKind::Unknown, None), (InkArgKind::Unknown, None)],
            ),
        ] {
            // Parse attribute.
            let attr = parse_first_attribute(code);

            // Parse ink! attribute arguments from attribute and convert to an array of tuples with
            // ink! attribute argument kind and meta value syntax kind for easy comparisons.
            let actual_args: Vec<(InkArgKind, Option<SyntaxKind>)> = parse_ink_args(&attr)
                .iter()
                .map(|arg| (*arg.kind(), arg.value().map(|value| value.kind())))
                .collect();

            // actual arguments should match expected arguments.
            assert_eq!(actual_args, expected_args);
        }
    }

    #[test]
    fn sort_ink_args_works() {
        for (code, expected_order) in [
            // Required and/or root-level/unambiguous arguments always have the highest priority.
            (
                quote_as_str! {
                    #[ink(message, payable, default, selector=1)]
                },
                vec![
                    InkArgKind::Message,
                    InkArgKind::Payable,
                    InkArgKind::Default,
                    InkArgKind::Selector,
                ],
            ),
            (
                quote_as_str! {
                    #[ink(selector=1, payable, default, message)]
                },
                vec![
                    InkArgKind::Message,
                    InkArgKind::Selector,
                    InkArgKind::Payable,
                    InkArgKind::Default,
                ],
            ),
            (
                quote_as_str! {
                    #[ink(extension=1, handle_status=false)]
                },
                vec![InkArgKind::Extension, InkArgKind::HandleStatus],
            ),
            (
                quote_as_str! {
                    #[ink(handle_status=false, extension=1)]
                },
                vec![InkArgKind::Extension, InkArgKind::HandleStatus],
            ),
            (
                quote_as_str! {
                    #[ink(event, anonymous)]
                },
                vec![InkArgKind::Event, InkArgKind::Anonymous],
            ),
            (
                quote_as_str! {
                    #[ink(anonymous, event)]
                },
                vec![InkArgKind::Event, InkArgKind::Anonymous],
            ),
            // Sorting is stable for arguments of the same priority level.
            (
                quote_as_str! {
                    #[ink(payable, default, selector=1)]
                },
                vec![
                    InkArgKind::Payable,
                    InkArgKind::Default,
                    InkArgKind::Selector,
                ],
            ),
            (
                quote_as_str! {
                    #[ink(selector=1, default, payable)]
                },
                vec![
                    InkArgKind::Selector,
                    InkArgKind::Default,
                    InkArgKind::Payable,
                ],
            ),
            (
                quote_as_str! {
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                },
                vec![InkArgKind::Env, InkArgKind::KeepAttr],
            ),
            (
                quote_as_str! {
                    #[ink::contract(keep_attr="foo,bar", env=my::env::Types)]
                },
                vec![InkArgKind::KeepAttr, InkArgKind::Env],
            ),
            // Unknown arguments are always last.
            (
                quote_as_str! {
                    #[ink::contract(unknown, keep_attr="foo,bar", env=my::env::Types)]
                },
                vec![InkArgKind::KeepAttr, InkArgKind::Env, InkArgKind::Unknown],
            ),
            (
                quote_as_str! {
                    #[ink::contract(xyz, keep_attr="foo,bar", env=my::env::Types)]
                },
                vec![InkArgKind::KeepAttr, InkArgKind::Env, InkArgKind::Unknown],
            ),
            (
                quote_as_str! {
                    #[ink::contract(keep_attr="foo,bar", xyz, env=my::env::Types)]
                },
                vec![InkArgKind::KeepAttr, InkArgKind::Env, InkArgKind::Unknown],
            ),
            (
                quote_as_str! {
                    #[ink::contract(keep_attr="foo,bar", env=my::env::Types, xyz)]
                },
                vec![InkArgKind::KeepAttr, InkArgKind::Env, InkArgKind::Unknown],
            ),
        ] {
            // Parse attribute.
            let attr = parse_first_attribute(code);

            // Parse ink! attribute arguments from attribute and
            // convert to an array of ink! attribute argument kinds for easy comparisons.
            let args = parse_ink_args(&attr);
            let actual_order: Vec<InkArgKind> =
                args.iter().sorted().map(|arg| *arg.kind()).collect();

            // actual order of argument kinds should match expected order.
            assert_eq!(actual_order, expected_order);
        }
    }
}
