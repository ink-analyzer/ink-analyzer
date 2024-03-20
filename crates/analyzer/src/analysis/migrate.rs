//! ink! 5.0 migration.

use crate::analysis::text_edit::format_edits;
use ink_analyzer_ir::syntax::{AstNode, AstToken, SyntaxKind, SyntaxToken, TextRange, TextSize};
use ink_analyzer_ir::{ast, ChainExtension, InkArg, InkArgKind, InkEntity, InkFile};
use itertools::Itertools;
use std::collections::HashMap;

use super::{utils, TextEdit};
use crate::resolution;

/// Computes text edits for migrating an ink! file to ink! 5.0.
pub fn migrate(file: &InkFile) -> Vec<TextEdit> {
    let mut results = Vec::new();

    // Migrate events.
    events(&mut results, file);

    // Migrate chain extensions.
    chain_extensions(&mut results, file);

    // Migrate e2e test attribute arguments.
    e2e_tests(&mut results, file);

    // Migrate built-in `derive`s of SCALE codec traits.
    scale_derive(&mut results, file);

    // Format and return edits.
    format_edits(results.into_iter(), file).collect()
}

/// Computes text edits for migrating ink! events to ink! 5.0 standalone events (aka events 2.0).
fn events(results: &mut Vec<TextEdit>, file: &InkFile) {
    for contract in file.contracts() {
        // Replace legacy events with 2.0 events.
        for event in contract.events() {
            if let Some(ink_attr) = event.ink_attr() {
                results.push(TextEdit::replace(
                    format!(
                        "#[ink::event{}]",
                        if event.anonymous_arg().is_some() {
                            "(anonymous)"
                        } else {
                            ""
                        }
                    ),
                    ink_attr.syntax().text_range(),
                ));

                // Remove standalone anonymous attribute (if any).
                if let Some(arg) = event.anonymous_arg() {
                    if !ink_attr
                        .syntax()
                        .text_range()
                        .contains_range(arg.text_range())
                    {
                        let range = utils::ink_arg_and_delimiter_removal_range(&arg, None);
                        results.push(TextEdit::delete(range));
                    }
                }
            }
        }
    }
}

/// Computes text edits for migrating ink! chain extension to ink! 5.0.
fn chain_extensions(results: &mut Vec<TextEdit>, file: &InkFile) {
    let mut unavailable_ids = file
        .chain_extensions()
        .iter()
        .filter_map(ChainExtension::id)
        .collect();

    for chain_extension in file.chain_extensions() {
        // Add `extension` arg to `chain_extension` attribute macro, if it's missing.
        if chain_extension.extension_arg().is_none() {
            if let Some(ink_attr) = chain_extension.ink_attr() {
                if let Some((insert_offset, insert_prefix, insert_suffix)) =
                    utils::ink_arg_insert_offset_and_affixes(ink_attr, Some(InkArgKind::Extension))
                {
                    let id = utils::suggest_unique_id(None, &unavailable_ids).unwrap_or(1);
                    unavailable_ids.insert(id);
                    results.push(TextEdit::insert(
                        format!(
                            "{}extension = {id}{}",
                            insert_prefix.unwrap_or_default(),
                            insert_suffix.unwrap_or_default()
                        ),
                        insert_offset,
                    ));
                }
            }
        }

        // Rename `extension` args on associated `fn`s to `function`.
        for extension in chain_extension.extensions() {
            if let Some(arg_name) = extension.extension_arg().as_ref().and_then(InkArg::name) {
                results.push(TextEdit::replace(
                    "function".to_owned(),
                    arg_name.syntax().text_range(),
                ));
            }
        }
    }
}

/// Computes text edits for migrating ink! chain extension to ink! 5.0.
fn e2e_tests(results: &mut Vec<TextEdit>, file: &InkFile) {
    for e2e_test in file.e2e_tests().iter().chain(
        file.contracts()
            .iter()
            .flat_map(|contract| contract.e2e_tests()),
    ) {
        if e2e_test.environment_arg().is_none()
            && (e2e_test.additional_contracts_arg().is_some()
                || e2e_test.additional_contracts_arg().is_some())
        {
            // Remove all attribute arguments if the ink! e2e attribute has either an
            // `additional_contracts` and/or `keep_attr` argument but no `environment` argument.
            if let Some(meta) = e2e_test.ink_attr().and_then(|attr| attr.ast().token_tree()) {
                results.push(TextEdit::delete(meta.syntax().text_range()));
            }
        } else {
            // Remove `additional_contracts` argument (if any).
            if let Some(arg) = e2e_test.additional_contracts_arg() {
                let range = utils::ink_arg_and_delimiter_removal_range(&arg, None);
                results.push(TextEdit::delete(range));
            }

            // Remove `keep_attr` argument (if any).
            if let Some(arg) = e2e_test.keep_attr_arg() {
                let range = utils::ink_arg_and_delimiter_removal_range(&arg, None);
                results.push(TextEdit::delete(range));
            }
        }
    }
}

/// Computes text edits for migrating built-in `derive`s of SCALE codec traits to ink! 5.0 `scale_derive` macro.
fn scale_derive(results: &mut Vec<TextEdit>, file: &InkFile) {
    // Mapping from target item (via location/start offset) to scale traits to derive and their insert offset.
    // Used to consolidate scale item traits from multiple attributes into a single new `scale_derive` attribute.
    let mut scale_items_map: HashMap<TextSize, (TextSize, Vec<InkArgKind>)> = HashMap::new();
    let scale_trait_options: [(&str, &[&str], InkArgKind); 3] = [
        ("Encode", &resolution::SCALE_QUALIFIERS, InkArgKind::Encode),
        ("Decode", &resolution::SCALE_QUALIFIERS, InkArgKind::Decode),
        (
            "TypeInfo",
            &resolution::SCALE_INFO_QUALIFIERS,
            InkArgKind::TypeInfo,
        ),
    ];

    for attr in file.syntax().descendants().filter_map(ast::Attr::cast) {
        let is_standalone_derive = attr
            .path()
            .is_some_and(|path| path.to_string().trim() == "derive");
        let is_cfg_attr = !is_standalone_derive
            && attr
                .path()
                .is_some_and(|path| path.to_string().trim() == "cfg_attr");

        // Extracts derive token tree (if any).
        let derive_meta = if is_standalone_derive {
            attr.token_tree()
        } else if is_cfg_attr {
            attr.token_tree().and_then(|token_tree| {
                token_tree.syntax().children().find_map(|node| {
                    let is_after_derive = || {
                        node.first_token()
                            .and_then(|token| {
                                ink_analyzer_ir::closest_non_trivia_token(
                                    &token,
                                    SyntaxToken::prev_token,
                                )
                            })
                            .is_some_and(|token| token.text() == "derive")
                    };
                    if ast::TokenTree::can_cast(node.kind()) && is_after_derive() {
                        ast::TokenTree::cast(node)
                    } else {
                        None
                    }
                })
            })
        } else {
            None
        };

        if let Some(derive_meta) = derive_meta {
            let mut scale_items = Vec::new();
            let mut non_scale_items = Vec::new();
            for name in utils::token_tree_to_non_delimited_meta_string(&derive_meta)
                .replace(' ', "")
                .split(',')
            {
                if let Some(path) = ink_analyzer_ir::path_from_str(name) {
                    let ref_node = attr
                        .syntax()
                        .parent()
                        .unwrap_or_else(|| file.syntax().clone());

                    let scale_arg_kind = scale_trait_options.iter().find_map(
                        |(trait_name, qualifiers, arg_kind)| {
                            resolution::is_external_crate_item(
                                trait_name, &path, qualifiers, &ref_node,
                            )
                            .then_some(arg_kind)
                        },
                    );
                    if let Some(scale_arg_kind) = scale_arg_kind {
                        scale_items.push(*scale_arg_kind);
                    } else {
                        non_scale_items.push(name.to_owned());
                    }
                } else {
                    non_scale_items.push(name.to_owned());
                }
            }

            // Refactors SCALE codec trait derives.
            if !scale_items.is_empty() {
                // Track SCALE code traits to migrate by target item or insert offset.
                let attr_start = attr.syntax().text_range().start();
                let parent_or_attr_start = ink_analyzer_ir::parent_ast_item(attr.syntax())
                    .map(|parent| parent.syntax().text_range().start())
                    .unwrap_or(attr_start);
                if let Some((_, items)) = scale_items_map.get_mut(&parent_or_attr_start) {
                    items.extend(scale_items);
                } else {
                    scale_items_map.insert(parent_or_attr_start, (attr_start, scale_items));
                }

                // Modifies attribute to remove SCALE codec trait derives (if any).
                if non_scale_items.is_empty() {
                    if is_standalone_derive {
                        // Removes `derive` attribute if it only includes SCALE codec trait derives.
                        results.push(TextEdit::delete(attr.syntax().text_range()));
                    } else {
                        // Finds derive token.
                        let derive_token = derive_meta
                            .syntax()
                            .first_token()
                            .and_then(|token| {
                                ink_analyzer_ir::closest_non_trivia_token(
                                    &token,
                                    SyntaxToken::prev_token,
                                )
                            })
                            .filter(|token| token.text() == "derive")
                            .expect("Expected a `derive` token before the token tree");
                        let derive_start = derive_token.text_range().start();

                        // Determines if `cfg_attr` has other attributes apart from the `derive` attribute.
                        let has_other_attrs = attr.token_tree().is_some_and(|cfg_attr_meta| {
                            let mut has_seen_comma = false;
                            let r_paren_option = cfg_attr_meta.r_paren_token();
                            cfg_attr_meta
                                .syntax()
                                .children_with_tokens()
                                .skip_while(|it| {
                                    let skip_current = !has_seen_comma;
                                    if !has_seen_comma && it.kind() == SyntaxKind::COMMA {
                                        has_seen_comma = true;
                                    }
                                    skip_current
                                })
                                .take_while(|it| {
                                    r_paren_option.is_none()
                                        || it.as_token() != r_paren_option.as_ref()
                                })
                                .any(|elem| {
                                    !elem.kind().is_trivia()
                                        && (elem.text_range().start() < derive_start
                                            || derive_meta.syntax().text_range().end()
                                                < elem.text_range().end())
                                })
                        });

                        if has_other_attrs {
                            // Removes `derive` attr if it only contains SCALE codec trait derives
                            // but the `cfg_attr` attribute contains other attributes.
                            let derive_end = derive_meta.syntax().text_range().end();
                            let delimiter_after = utils::node_and_delimiter_range(
                                derive_meta.syntax(),
                                SyntaxKind::COMMA,
                            );
                            // Determines delete range (including the delimiter if present).
                            let range = if delimiter_after.end() == derive_end {
                                let delimiter_before = utils::token_and_delimiter_range(
                                    &derive_token,
                                    SyntaxKind::COMMA,
                                );
                                TextRange::new(delimiter_before.start(), derive_end)
                            } else {
                                TextRange::new(derive_start, delimiter_after.end())
                            };
                            results.push(TextEdit::delete(range));
                        } else {
                            // Removes `cfg_attr` attribute if it only contains a `derive` attr with
                            // only SCALE codec trait derives.
                            results.push(TextEdit::delete(attr.syntax().text_range()));
                        }
                    }
                } else {
                    // Updates derive meta.
                    results.push(TextEdit::replace(
                        format!("({})", non_scale_items.join(", ")),
                        derive_meta.syntax().text_range(),
                    ));
                }
            }
        }
    }

    // Add `ink::scale_derive` attributes.
    for (insert_offset, scale_items) in scale_items_map.values() {
        results.push(TextEdit::insert(
            format!("#[ink::scale_derive({})]", scale_items.iter().join(", ")),
            *insert_offset,
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TextSize;
    use quote::quote;
    use test_utils::{parse_offset_at, quote_as_pretty_string};

    fn convert_results(
        code: &str,
        expected_results: Vec<(&str, Option<&str>, Option<&str>)>,
    ) -> Vec<TextEdit> {
        expected_results
            .into_iter()
            .map(|(text, start, end)| {
                TextEdit::new(
                    text.to_owned(),
                    TextRange::new(
                        TextSize::from(parse_offset_at(&code, start).unwrap() as u32),
                        TextSize::from(parse_offset_at(&code, end).unwrap() as u32),
                    ),
                    None,
                )
            })
            .collect()
    }

    #[test]
    fn events_works() {
        for (code, expected_results) in [
            (
                quote! {
                    #[ink(event)]
                    pub struct MyEvent {}
                },
                vec![(
                    "#[ink::event]",
                    Some("<-#[ink(event)]"),
                    Some("#[ink(event)]"),
                )],
            ),
            (
                quote! {
                    #[ink(event, anonymous)]
                    pub struct MyEvent {}
                },
                vec![(
                    "#[ink::event(anonymous)]",
                    Some("<-#[ink(event, anonymous)]"),
                    Some("#[ink(event, anonymous)]"),
                )],
            ),
            (
                quote! {
                    #[ink(event)]
                    #[ink(anonymous)]
                    pub struct MyEvent {}
                },
                vec![
                    (
                        "#[ink::event(anonymous)]",
                        Some("<-#[ink(event)]"),
                        Some("#[ink(event)]"),
                    ),
                    ("", Some("<-#[ink(anonymous)]"), Some("#[ink(anonymous)]")),
                ],
            ),
            (
                quote! {
                    #[ink(event)]
                    pub struct MyEvent {}

                    #[ink(event, anonymous)]
                    pub struct MyEvent2 {}
                },
                vec![
                    (
                        "#[ink::event]",
                        Some("<-#[ink(event)]"),
                        Some("#[ink(event)]"),
                    ),
                    (
                        "#[ink::event(anonymous)]",
                        Some("<-#[ink(event, anonymous)]"),
                        Some("#[ink(event, anonymous)]"),
                    ),
                ],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::contract]
                mod my_contract {
                    #code
                }
            };

            let mut results = Vec::new();
            events(&mut results, &InkFile::parse(&code));

            assert_eq!(results, convert_results(&code, expected_results));
        }
    }

    #[test]
    fn chain_extensions_works() {
        for (code, expected_results) in [
            (
                quote_as_pretty_string! {
                    #[ink::chain_extension]
                    pub trait MyChainExtension {
                        #[ink(extension = 1)]
                        fn my_extension();

                        #[ink(extension = 2, handle_status = false)]
                        fn my_extension2();

                        #[ink(extension = 3)]
                        #[ink(handle_status = false)]
                        fn my_extension3();
                    }
                },
                vec![
                    (
                        "(extension = 1)",
                        Some("#[ink::chain_extension"),
                        Some("#[ink::chain_extension"),
                    ),
                    ("function", Some("<-extension = 1"), Some("<- = 1")),
                    ("function", Some("<-extension = 2"), Some("<- = 2")),
                    ("function", Some("<-extension = 3"), Some("<- = 3")),
                ],
            ),
            (
                quote_as_pretty_string! {
                    #[ink::chain_extension]
                    pub trait MyChainExtension {
                        #[ink(extension = 1)]
                        fn my_extension();
                    }

                    #[ink::chain_extension]
                    pub trait MyOtherChainExtension {
                        #[ink(extension = 2)]
                        fn my_extension();
                    }
                },
                vec![
                    (
                        "(extension = 1)",
                        Some("#[ink::chain_extension"),
                        Some("#[ink::chain_extension"),
                    ),
                    ("function", Some("<-extension = 1"), Some("<- = 1")),
                    (
                        "(extension = 2)",
                        Some("#[ink::chain_extension->"),
                        Some("#[ink::chain_extension->"),
                    ),
                    ("function", Some("<-extension = 2"), Some("<- = 2")),
                ],
            ),
        ] {
            let mut results = Vec::new();
            chain_extensions(&mut results, &InkFile::parse(&code));

            assert_eq!(results, convert_results(&code, expected_results));
        }
    }

    #[test]
    fn e2e_tests_works() {
        for (code, expected_results) in [
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(environment = ink::env::DefaultEnvironment)]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(additional_contracts = "adder/Cargo.toml flipper/Cargo.toml")]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![("", Some("#[ink_e2e::test"), Some("<-]"))],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(keep_attr = "foo,bar")]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![("", Some("#[ink_e2e::test"), Some("<-]"))],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(
                        additional_contracts = "adder/Cargo.toml flipper/Cargo.toml",
                        keep_attr = "foo,bar"
                    )]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![("", Some("#[ink_e2e::test"), Some("<-]"))],
            ),
            (
                quote_as_pretty_string! {
                    type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

                    #[ink_e2e::test(
                        additional_contracts = "adder/Cargo.toml flipper/Cargo.toml",
                        environment = ink::env::DefaultEnvironment,
                        keep_attr = "foo,bar"
                    )]
                    async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {}
                },
                vec![
                    (
                        "",
                        Some("<-additional_contracts"),
                        Some(r#""adder/Cargo.toml flipper/Cargo.toml","#),
                    ),
                    ("", Some("<-keep_attr"), Some("\"foo,bar\"\n")),
                ],
            ),
        ] {
            let mut results = Vec::new();
            e2e_tests(&mut results, &InkFile::parse(&code));

            assert_eq!(results, convert_results(&code, expected_results));
        }
    }

    #[test]
    fn scale_derive_works() {
        for (code, expected_results) in [
            (
                quote_as_pretty_string! {
                    #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
                    pub struct MyType {}
                },
                vec![
                    (
                        "",
                        Some("<-#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]"),
                        Some("#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]"),
                    ),
                    (
                        "#[ink::scale_derive(Encode, Decode, TypeInfo)]",
                        Some("<-#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]"),
                        Some("<-#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]"),
                    ),
                ],
            ),
            (
                quote_as_pretty_string! {
                    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
                    pub struct MyType {}
                },
                vec![
                    (
                        "",
                        Some(r#"<-#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]"#),
                        Some(r#"#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]"#),
                    ),
                    (
                        "#[ink::scale_derive(TypeInfo)]",
                        Some("<-#[cfg_attr("),
                        Some("<-#[cfg_attr("),
                    ),
                ],
            ),
            (
                quote_as_pretty_string! {
                    #[derive(scale::Encode, scale::Decode)]
                    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
                    pub struct MyType {}
                },
                vec![
                    (
                        "",
                        Some("<-#[derive(scale::Encode, scale::Decode)]"),
                        Some("#[derive(scale::Encode, scale::Decode)]"),
                    ),
                    (
                        "",
                        Some(r#"<-#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]"#),
                        Some(r#"#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]"#),
                    ),
                    (
                        "#[ink::scale_derive(Encode, Decode, TypeInfo)]",
                        Some("<-#[derive(scale::Encode, scale::Decode)]"),
                        Some("<-#[derive(scale::Encode, scale::Decode)]"),
                    ),
                ],
            ),
            (
                quote_as_pretty_string! {
                    #[derive(Clone, Copy, scale::Encode, scale::Decode, scale_info::TypeInfo)]
                    pub struct MyType {}
                },
                vec![
                    ("(Clone, Copy)", Some("#[derive"), Some("<-]")),
                    (
                        "#[ink::scale_derive(Encode, Decode, TypeInfo)]",
                        Some("<-#[derive("),
                        Some("<-#[derive("),
                    ),
                ],
            ),
            (
                quote_as_pretty_string! {
                    #[cfg_attr(feature = "std", derive(Clone, scale_info::TypeInfo), doc = "Hello")]
                    pub struct MyType {}
                },
                vec![
                    (
                        "(Clone)",
                        Some("<-(Clone, scale_info::TypeInfo)"),
                        Some("(Clone, scale_info::TypeInfo)"),
                    ),
                    (
                        "#[ink::scale_derive(TypeInfo)]",
                        Some("<-#[cfg_attr("),
                        Some("<-#[cfg_attr("),
                    ),
                ],
            ),
            (
                quote_as_pretty_string! {
                    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo), doc = "Hello")]
                    pub struct MyType {}
                },
                vec![
                    (
                        "",
                        Some("<-derive(scale_info::TypeInfo)"),
                        Some("derive(scale_info::TypeInfo),"),
                    ),
                    (
                        "#[ink::scale_derive(TypeInfo)]",
                        Some("<-#[cfg_attr("),
                        Some("<-#[cfg_attr("),
                    ),
                ],
            ),
            (
                quote_as_pretty_string! {
                    #[derive(Clone, scale::Encode, scale::Decode)]
                    #[cfg_attr(feature = "std", derive(Copy, scale_info::TypeInfo), doc = "Hello")]
                    pub struct MyType {}
                },
                vec![
                    (
                        "(Clone)",
                        Some("#[derive"),
                        Some("#[derive(Clone, scale::Encode, scale::Decode)"),
                    ),
                    (
                        "(Copy)",
                        Some("<-(Copy, scale_info::TypeInfo)"),
                        Some("(Copy, scale_info::TypeInfo)"),
                    ),
                    (
                        "#[ink::scale_derive(Encode, Decode, TypeInfo)]",
                        Some("<-#[derive(Clone, scale::Encode, scale::Decode)]"),
                        Some("<-#[derive(Clone, scale::Encode, scale::Decode)]"),
                    ),
                ],
            ),
        ] {
            let mut results = Vec::new();
            scale_derive(&mut results, &InkFile::parse(&code));

            assert_eq!(results, convert_results(&code, expected_results));
        }
    }
}
