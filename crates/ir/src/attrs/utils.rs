//! ink! attribute IR utilities.

use itertools::Itertools;
use ra_ap_syntax::{ast, AstNode, AstToken, SyntaxElement, SyntaxToken, T};

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

/// Parse meta items.
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
                let arg_elems: Vec<SyntaxElement> = group.collect();
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
                    Some(MetaNameValue::new(
                        arg_name_meta_option(arg_item_groups.get(0).unwrap_or(&empty)),
                        arg_eq_option(arg_item_groups.get(1).unwrap_or(&empty)),
                        arg_value_meta_option(arg_item_groups.get(2).unwrap_or(&empty)),
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
        .filter(|elem| !elem.kind().is_trivia())
        .collect()
}

fn token_at_index<'a>(elems: &'a [&SyntaxElement], idx: usize) -> Option<&'a SyntaxToken> {
    elems.get(idx)?.as_token()
}

fn arg_name_meta_option(elems: &[SyntaxElement]) -> MetaOption<MetaName> {
    let non_trivia_elems = only_non_trivia_elements(elems);
    match non_trivia_elems.len() {
        0 => MetaOption::None,
        1 => {
            let mut name = MetaOption::Err(elems.to_owned());
            if let Some(token) = token_at_index(&non_trivia_elems, 0) {
                if let Some(meta_name) = MetaName::cast(token.clone()) {
                    name = MetaOption::Ok(meta_name);
                }
            }
            name
        }
        _ => MetaOption::Err(elems.to_owned()),
    }
}

fn arg_eq_option(elems: &[SyntaxElement]) -> Option<MetaSeparator> {
    let non_trivia_elems = only_non_trivia_elements(elems);
    (non_trivia_elems.len() == 1)
        .then(|| MetaSeparator::cast(token_at_index(&non_trivia_elems, 0)?.clone()))?
}

fn arg_value_meta_option(elems: &[SyntaxElement]) -> MetaOption<MetaValue> {
    if elems.is_empty() {
        return MetaOption::None;
    }

    match MetaValue::parse(elems) {
        Some(expr) => MetaOption::Ok(expr),
        None => MetaOption::Err(elems.to_owned()),
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
