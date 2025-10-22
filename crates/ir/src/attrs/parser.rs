//! ink! attribute IR utilities.

use itertools::Itertools;
use ra_ap_syntax::{ast, AstNode, AstToken, SyntaxElement, SyntaxKind, T};

use super::meta::{MetaName, MetaNameValue, MetaOption, MetaSeparator, MetaValue};
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

    fn is_error_wrapped_closing_bracket(elem: &SyntaxElement) -> bool {
        elem.kind() == SyntaxKind::ERROR
            && elem.as_node().is_some_and(|error| {
                error
                    .last_token()
                    .is_some_and(|token| token.kind() == SyntaxKind::R_BRACK)
            })
    }

    token_tree
        .syntax()
        .children_with_tokens()
        // Skip starting parenthesis if present.
        .skip(usize::from(l_paren.is_some()))
        // Ignore closing parenthesis if present.
        .take_while(|it| {
            (r_paren.is_none() || it.as_token() != r_paren.as_ref())
                // Stops before closing bracket (i.e. `]`) in unbalanced attributes (e.g. `#[ink_e2e::test(backend]`)
                && !is_error_wrapped_closing_bracket(it)
        })
        // Comma (`,`) separated groups.
        .chunk_by(|token| token.kind() == T![,])
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
                let mut nested_token_tree = None;
                let mut n_nested_tts = 0;

                let mut name: Vec<_> = arg_tokens
                    .by_ref()
                    .take_while(|elem| {
                        let is_sep = elem.kind() == T![=];
                        if is_sep {
                            // Sets the equal sign (`=`) if its present (before its consumed).
                            eq = elem.clone().into_token().and_then(MetaSeparator::cast);
                        } else if elem.kind() == SyntaxKind::TOKEN_TREE {
                            // Tracks the number of token trees and stores the first one.
                            n_nested_tts += 1;
                            if nested_token_tree.is_none() {
                                nested_token_tree =
                                    elem.as_node().cloned().and_then(ast::TokenTree::cast);
                            }
                        }
                        !is_sep
                    })
                    .collect();
                let value: Vec<_> = arg_tokens.collect();

                let mut nested_meta = None;
                if n_nested_tts == 1 {
                    if let Some(nested_token_tree) = nested_token_tree {
                        let nested_meta_items = parse_meta_items(&nested_token_tree);
                        // At the moment, ink! syntax (specifically v5) only nests a single meta item.
                        if nested_meta_items.len() == 1 {
                            nested_meta = Some(Box::new(nested_meta_items[0].to_owned()));
                        } else if matches!(nested_token_tree.to_string().as_str(), "(" | "()") {
                            // Handles incomplete nested args.
                            nested_meta = Some(Box::new(MetaNameValue::empty(
                                nested_token_tree.syntax().text_range().start(),
                                Some(nested_token_tree.syntax().text_range().end()),
                                token_tree.clone(),
                            )));
                        }
                        if nested_meta.is_some() {
                            // Remove the nested token tree from the name.
                            name.retain(|elem| elem.kind() != SyntaxKind::TOKEN_TREE);
                        }
                    }
                }

                let meta_name = parse_meta_name(&name);
                let meta_value = parse_meta_value(&value);
                Some(
                    if meta_name.is_some()
                        || eq.is_some()
                        || meta_value.is_some()
                        || nested_meta.is_some()
                    {
                        MetaNameValue::new(
                            meta_name,
                            eq,
                            meta_value,
                            nested_meta,
                            last_separator_offset,
                        )
                    } else {
                        MetaNameValue::empty(last_separator_offset, None, token_tree.clone())
                    },
                )
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
                .first()
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
        #[derive(Debug, PartialEq, Eq)]
        struct NestedArg(InkArgKind, Option<SyntaxKind>, Option<Box<NestedArg>>);

        impl NestedArg {
            fn new(args: &[(InkArgKind, Option<SyntaxKind>)]) -> Self {
                let mut args_rev_iter = args.iter().cloned().rev();
                let (arg_kind, syntax_kind) = args_rev_iter.next().unwrap();
                let mut nested_arg = Self(arg_kind, syntax_kind, None);
                for (arg_kind, syntax_kind) in args_rev_iter {
                    nested_arg = Self(arg_kind, syntax_kind, Some(Box::new(nested_arg)))
                }
                nested_arg
            }
        }

        impl From<InkArg> for NestedArg {
            fn from(arg: InkArg) -> Self {
                Self(
                    *arg.kind(),
                    arg.value().map(MetaValue::kind),
                    arg.nested()
                        .map(|nested_arg| Box::new(NestedArg::from(nested_arg))),
                )
            }
        }

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
                    NestedArg::new(&[(InkArgKind::Env, Some(SyntaxKind::PATH))]),
                    NestedArg::new(&[(InkArgKind::KeepAttr, Some(SyntaxKind::STRING))]),
                ],
            ),
            (
                quote_as_str! {
                    #[ink::scale_derive(Encode, Decode, TypeInfo)]
                },
                vec![
                    NestedArg::new(&[(InkArgKind::Encode, None)]),
                    NestedArg::new(&[(InkArgKind::Decode, None)]),
                    NestedArg::new(&[(InkArgKind::TypeInfo, None)]),
                ],
            ),
            (
                quote_as_str! {
                    #[ink_e2e::test(
                        additional_contracts="adder/Cargo.toml flipper/Cargo.toml",
                        environment=my::env::Types,
                        keep_attr="foo,bar",
                        backend(node),
                    )]
                },
                vec![
                    NestedArg::new(&[(InkArgKind::AdditionalContracts, Some(SyntaxKind::STRING))]),
                    NestedArg::new(&[(InkArgKind::Environment, Some(SyntaxKind::PATH))]),
                    NestedArg::new(&[(InkArgKind::KeepAttr, Some(SyntaxKind::STRING))]),
                    NestedArg::new(&[(InkArgKind::Backend, None), (InkArgKind::Node, None)]),
                ],
            ),
            // Argument with no value.
            (
                quote_as_str! {
                    #[ink(storage)]
                },
                vec![NestedArg::new(&[(InkArgKind::Storage, None)])],
            ),
            (
                quote_as_str! {
                    #[ink(anonymous)]
                },
                vec![NestedArg::new(&[(InkArgKind::Anonymous, None)])],
            ),
            // Compound arguments with no value.
            (
                quote_as_str! {
                    #[ink(event, anonymous)]
                },
                vec![
                    NestedArg::new(&[(InkArgKind::Event, None)]),
                    NestedArg::new(&[(InkArgKind::Anonymous, None)]),
                ],
            ),
            // Argument with integer value.
            (
                quote_as_str! {
                    #[ink(selector=1)] // Decimal.
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Selector,
                    Some(SyntaxKind::INT_NUMBER),
                )])],
            ),
            (
                quote_as_str! {
                    #[ink(extension=0x1)] // Hexadecimal.
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Extension,
                    Some(SyntaxKind::INT_NUMBER),
                )])],
            ),
            (
                quote_as_str! {
                    #[ink(function=1)]
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Function,
                    Some(SyntaxKind::INT_NUMBER),
                )])],
            ),
            // Argument with wildcard/underscore value.
            (
                quote_as_str! {
                    #[ink(selector=_)]
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Selector,
                    Some(SyntaxKind::UNDERSCORE),
                )])],
            ),
            // Argument with `@` symbol as value.
            (
                quote_as_str! {
                    #[ink(selector=@)]
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Selector,
                    Some(SyntaxKind::AT),
                )])],
            ),
            // Argument with string value.
            (
                quote_as_str! {
                    #[ink(namespace="my_namespace")]
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Namespace,
                    Some(SyntaxKind::STRING),
                )])],
            ),
            // Argument with boolean value.
            (
                quote_as_str! {
                    #[ink(handle_status=true)]
                },
                vec![NestedArg::new(&[(
                    InkArgKind::HandleStatus,
                    Some(SyntaxKind::TRUE_KW),
                )])],
            ),
            (
                quote_as_str! {
                    #[ink(derive=false)]
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Derive,
                    Some(SyntaxKind::FALSE_KW),
                )])],
            ),
            // Argument with path value.
            (
                quote_as_str! {
                    #[ink(env=my::env::Types)]
                },
                vec![NestedArg::new(&[(InkArgKind::Env, Some(SyntaxKind::PATH))])],
            ),
            // Compound arguments of different kinds.
            (
                quote_as_str! {
                    #[ink(message, payable, selector=1)]
                },
                vec![
                    NestedArg::new(&[(InkArgKind::Message, None)]),
                    NestedArg::new(&[(InkArgKind::Payable, None)]),
                    NestedArg::new(&[(InkArgKind::Selector, Some(SyntaxKind::INT_NUMBER))]),
                ],
            ),
            (
                quote_as_str! {
                    #[ink(extension=1, handle_status=false)]
                },
                vec![
                    NestedArg::new(&[(InkArgKind::Extension, Some(SyntaxKind::INT_NUMBER))]),
                    NestedArg::new(&[(InkArgKind::HandleStatus, Some(SyntaxKind::FALSE_KW))]),
                ],
            ),
            // Nested arguments.
            (
                quote_as_str! {
                    #[ink_e2e::test(
                        backend(node),
                    )]
                },
                vec![NestedArg::new(&[
                    (InkArgKind::Backend, None),
                    (InkArgKind::Node, None),
                ])],
            ),
            (
                quote_as_str! {
                    #[ink_e2e::test(
                        backend(node(url = "ws://127.0.0.1:8000")),
                    )]
                },
                vec![NestedArg::new(&[
                    (InkArgKind::Backend, None),
                    (InkArgKind::Node, None),
                    (InkArgKind::Url, Some(SyntaxKind::STRING)),
                ])],
            ),
            (
                quote_as_str! {
                    #[ink_e2e::test(
                        backend(runtime_only),
                    )]
                },
                vec![NestedArg::new(&[
                    (InkArgKind::Backend, None),
                    (InkArgKind::RuntimeOnly, None),
                ])],
            ),
            (
                quote_as_str! {
                    #[ink_e2e::test(
                        backend(runtime_only(sandbox = ::ink_e2e::MinimalSandbox)),
                    )]
                },
                vec![NestedArg::new(&[
                    (InkArgKind::Backend, None),
                    (InkArgKind::RuntimeOnly, None),
                    (InkArgKind::Sandbox, Some(SyntaxKind::PATH)),
                ])],
            ),
            // Unknown argument.
            (
                quote_as_str! {
                    #[ink(unknown)]
                },
                vec![NestedArg::new(&[(InkArgKind::Unknown, None)])],
            ),
            (
                quote_as_str! {
                    #[ink(xyz)]
                },
                vec![NestedArg::new(&[(InkArgKind::Unknown, None)])],
            ),
            (
                quote_as_str! {
                    #[ink(xyz="abc")]
                },
                vec![NestedArg::new(&[(
                    InkArgKind::Unknown,
                    Some(SyntaxKind::STRING),
                )])],
            ),
            // Non-ink argument.
            (
                quote_as_str! {
                    #[cfg_attr(not(feature = "std"), no_std)]
                },
                vec![
                    NestedArg::new(&[
                        (InkArgKind::Unknown, None),
                        (InkArgKind::Unknown, Some(SyntaxKind::STRING)),
                    ]),
                    NestedArg::new(&[(InkArgKind::Unknown, None)]),
                ],
            ),
        ] {
            // Parse attribute.
            let attr = parse_first_attribute(code);

            // Parse ink! attribute arguments from attribute and convert to
            // an array-like structure of tuples with ink! attribute argument kind and
            // meta value syntax kind for easy comparisons.
            let actual_args: Vec<_> = parse_ink_args(&attr)
                .into_iter()
                .map(NestedArg::from)
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
            (
                quote_as_str! {
                    #[ink::scale_derive(Encode, Decode, TypeInfo)]
                },
                vec![InkArgKind::Encode, InkArgKind::Decode, InkArgKind::TypeInfo],
            ),
            (
                quote_as_str! {
                    #[ink::scale_derive(TypeInfo, Decode, Encode)]
                },
                vec![InkArgKind::TypeInfo, InkArgKind::Decode, InkArgKind::Encode],
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
