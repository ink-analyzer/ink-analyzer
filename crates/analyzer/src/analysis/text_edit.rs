//! A text edit.

mod common;

use ink_analyzer_ir::syntax::{AstNode, SyntaxKind, SyntaxToken, TextRange, TextSize};
use ink_analyzer_ir::{InkEntity, InkFile};

use super::utils;

pub use common::*;

/// A text edit (with an optional snippet - i.e tab stops and/or placeholders).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextEdit {
    /// Replacement text for the text edit.
    pub text: String,
    /// Range to which the text edit will be applied.
    pub range: TextRange,
    /// Formatted snippet for the text edit (includes tab stops and/or placeholders).
    pub snippet: Option<String>,
}

impl TextEdit {
    /// Creates text edit.
    pub fn new(text: String, range: TextRange, snippet: Option<String>) -> Self {
        Self {
            text,
            range,
            snippet,
        }
    }

    /// Creates text edit for inserting at the given offset.
    pub fn insert(text: String, offset: TextSize) -> Self {
        Self::insert_with_snippet(text, offset, None)
    }

    /// Creates text edit for inserting at the given offset (including an optional snippet).
    pub fn insert_with_snippet(text: String, offset: TextSize, snippet: Option<String>) -> Self {
        Self {
            text,
            range: TextRange::new(offset, offset),
            snippet,
        }
    }

    /// Creates text edit for replacing the given range.
    pub fn replace(text: String, range: TextRange) -> Self {
        Self::replace_with_snippet(text, range, None)
    }

    /// Creates text edit for replacing the given range (including an optional snippet) - i.e an alias of [`Self::new`].
    pub fn replace_with_snippet(text: String, range: TextRange, snippet: Option<String>) -> Self {
        Self::new(text, range, snippet)
    }

    /// Creates a text edit for deleting the specified range.
    pub fn delete(range: TextRange) -> Self {
        Self {
            text: String::new(),
            range,
            snippet: None,
        }
    }
}

/// Format text edits (i.e. add indenting and new lines based on context).
pub fn format_edits<'a>(
    edits: impl Iterator<Item = TextEdit> + 'a,
    file: &'a InkFile,
) -> impl Iterator<Item = TextEdit> + 'a {
    edits.map(|item| format_edit(item, file))
}

/// Format text edit (i.e. add indenting and new lines based on context).
pub fn format_edit(mut edit: TextEdit, file: &InkFile) -> TextEdit {
    // Adds affixes to a text edit.
    fn add_affixes(edit: &mut TextEdit, prefix: Option<String>, suffix: Option<String>) {
        if prefix.is_some() || suffix.is_some() {
            edit.text = format!(
                "{}{}{}",
                prefix.as_deref().unwrap_or_default(),
                edit.text,
                suffix.as_deref().unwrap_or_default(),
            );
            edit.snippet = edit.snippet.as_ref().map(|snippet| {
                format!(
                    "{}{snippet}{}",
                    prefix.as_deref().unwrap_or_default(),
                    suffix.as_deref().unwrap_or_default()
                )
            });
        }
    }

    // Generates affixes for edits between new lines and/or file boundaries.
    let affix_edit_between_whitespace_or_file_boundaries =
        |whitespace_before: Option<&str>, whitespace_after: Option<&str>| {
            let build_affix = |ws_text: &str| {
                (!utils::starts_with_two_or_more_newlines(ws_text)).then(|| "\n".to_owned())
            };
            match (
                whitespace_before.map(|ws_before| (ws_before.contains('\n'), ws_before)),
                whitespace_after.map(|ws_after| (ws_after.contains('\n'), ws_after)),
            ) {
                // Handles edits between new lines and/or file boundaries.
                (Some((true, ws_text_before)), Some((true, ws_text_after))) => {
                    (build_affix(ws_text_before), build_affix(ws_text_after))
                }
                (Some((true, ws_text_before)), None) => (build_affix(ws_text_before), None),
                (None, Some((true, ws_text_after))) => (None, build_affix(ws_text_after)),
                _ => (None, None),
            }
        };

    // Handles edits inside whitespace.
    let covering_whitespace = file
        .syntax()
        .covering_element(edit.range)
        .into_token()
        .filter(|token| {
            token.kind() == SyntaxKind::WHITESPACE
                && token.text_range().start() < edit.range.start()
                && edit.range.end() < token.text_range().end()
        });
    if let Some(whitespace_token) = covering_whitespace {
        if edit.text.is_empty() {
            return edit;
        }

        let whitespace_text = whitespace_token.text();
        let start = edit.range.start() - whitespace_token.text_range().start();
        let (whitespace_before, whitespace_after) = if edit.range.is_empty() {
            whitespace_text.split_at(start.into())
        } else {
            let end = whitespace_token.text_range().end() - edit.range.end();
            let (whitespace_before, _) = whitespace_text.split_at(start.into());
            let (_, whitespace_after) = whitespace_text.split_at(end.into());
            (whitespace_before, whitespace_after)
        };
        let (prefix, suffix) = affix_edit_between_whitespace_or_file_boundaries(
            Some(whitespace_before),
            Some(whitespace_after),
        );
        add_affixes(&mut edit, prefix, suffix);
        return edit;
    }

    // Determines the token right before the start of the edit offset.
    let token_before_option = file
        .syntax()
        .token_at_offset(edit.range.start())
        .left_biased()
        .and_then(|token| {
            if token.text_range().end() <= edit.range.start() {
                Some(token)
            } else {
                token.prev_token()
            }
        });
    // Determines the token right after the end of the edit offset.
    let token_after_option = file
        .syntax()
        .token_at_offset(edit.range.end())
        .right_biased()
        .and_then(|token| {
            if token.text_range().start() >= edit.range.end() {
                Some(token)
            } else {
                token.next_token()
            }
        });

    if edit.text.is_empty() {
        // Handles deletes.
        // Removes whitespace immediately following a delete if the text is surrounded by whitespace,
        // but only when the token right after the whitespace is not a closing curly bracket
        // (because it would otherwise break the indenting of the closing curly bracket).
        if let Some(token_after) = token_after_option {
            let is_whitespace_or_bof_before =
                token_before_option.as_ref().map_or(true, |token_before| {
                    token_before.kind() == SyntaxKind::WHITESPACE
                });
            let is_at_the_end_block = token_after
                .next_token()
                .is_some_and(|it| it.kind() == SyntaxKind::R_CURLY);
            if is_whitespace_or_bof_before
                && token_after.kind() == SyntaxKind::WHITESPACE
                && !is_at_the_end_block
            {
                edit.range = TextRange::new(edit.range.start(), token_after.text_range().end());
            }
        }
    } else {
        // Handles insert and replace.
        let affix_edit_after_whitespace_or_bof =
            |token_before_option: Option<&SyntaxToken>,
             token_after_option: Option<&SyntaxToken>| {
                let is_whitespace_or_bof_before =
                    token_before_option.as_ref().map_or(true, |token_before| {
                        token_before.kind() == SyntaxKind::WHITESPACE
                    });
                let is_whitespace_or_eof_after =
                    token_after_option.as_ref().map_or(true, |token_after| {
                        token_after.kind() == SyntaxKind::WHITESPACE
                    });
                if is_whitespace_or_bof_before && is_whitespace_or_eof_after {
                    // Handles edits between whitespace and/or file boundaries.
                    affix_edit_between_whitespace_or_file_boundaries(
                        token_before_option.map(SyntaxToken::text),
                        token_after_option.map(SyntaxToken::text),
                    )
                } else if is_whitespace_or_bof_before && !is_whitespace_or_eof_after {
                    // Handles preserving formatting for next non-whitespace item.
                    (
                        // No formatting prefix.
                        None,
                        // Adds formatting suffix only if the edit is not surrounded by whitespace
                        // (treats end of the file like whitespace)
                        // and its preceding whitespace contains a new line.
                        match token_before_option {
                            // Handles beginning of file.
                            None => Some("\n".to_owned()),
                            Some(token_before) => token_before.text().contains('\n').then(|| {
                                format!(
                                    "\n{}{}",
                                    if utils::starts_with_two_or_more_newlines(token_before.text())
                                    {
                                        "\n"
                                    } else {
                                        ""
                                    },
                                    utils::end_indenting(token_before.text())
                                )
                            }),
                        },
                    )
                } else {
                    (None, None)
                }
            };
        let (prefix, suffix) = if let Some(token_before) = token_before_option {
            match token_before.kind() {
                // Handles edits after whitespace.
                SyntaxKind::WHITESPACE => affix_edit_after_whitespace_or_bof(
                    Some(&token_before),
                    token_after_option.as_ref(),
                ),
                // Handles edits at the beginning of blocks (i.e right after the opening curly bracket).
                SyntaxKind::L_CURLY => {
                    (
                        // Adds formatting prefix only if the edit doesn't start with a new line
                        // and then only add indenting if the edit doesn't start with a space (i.e ' ') or a tab (i.e. '\t').
                        (!edit.text.starts_with('\n')).then(|| {
                            format!(
                                "\n{}",
                                (!edit.text.starts_with(' ') && !edit.text.starts_with('\t'))
                                    .then(|| {
                                        ink_analyzer_ir::parent_ast_item(&token_before)
                                            .map(|it| utils::item_children_indenting(it.syntax()))
                                    })
                                    .flatten()
                                    .as_deref()
                                    .unwrap_or_default()
                            )
                        }),
                        // Adds formatting suffix if the edit is followed by either a non-whitespace character
                        // or whitespace that doesn't start with at least 2 new lines
                        // (the new lines can be interspersed with other whitespace)
                        // and the edit doesn't end with 2 new lines.
                        token_after_option.as_ref().and_then(|token_after| {
                            ((token_after.kind() != SyntaxKind::WHITESPACE
                                || !utils::starts_with_two_or_more_newlines(token_after.text()))
                                && !edit.text.ends_with("\n\n"))
                            .then(|| {
                                format!(
                                    "\n{}",
                                    if token_after.text().starts_with('\n') {
                                        ""
                                    } else {
                                        "\n"
                                    }
                                )
                            })
                        }),
                    )
                }
                // Handles edits at the end of a statement or block or after a comment.
                SyntaxKind::SEMICOLON | SyntaxKind::R_CURLY | SyntaxKind::COMMENT => {
                    (
                        // Adds formatting prefix only if the edit doesn't start with a new line
                        // and then only add indenting if the edit doesn't start with a space (i.e ' ') or a tab (i.e. '\t').
                        (!edit.text.starts_with('\n')).then(|| {
                            format!(
                                "\n{}{}",
                                // Extra new line at the end of statements and blocks.
                                if token_before.kind() == SyntaxKind::COMMENT {
                                    ""
                                } else {
                                    "\n"
                                },
                                if !edit.text.starts_with(' ') && !edit.text.starts_with('\t') {
                                    ink_analyzer_ir::parent_ast_item(&token_before)
                                        .and_then(|it| utils::item_indenting(it.syntax()))
                                } else {
                                    None
                                }
                                .as_deref()
                                .unwrap_or_default(),
                            )
                        }),
                        // No formatting suffix.
                        None,
                    )
                }
                // Ignores all other cases.
                _ => (None, None),
            }
        } else {
            affix_edit_after_whitespace_or_bof(None, token_after_option.as_ref())
        };

        // Adds formatting if necessary.
        add_affixes(&mut edit, prefix, suffix);
    }
    edit
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::parse_offset_at;

    #[test]
    fn format_insert_and_replace_works() {
        for (input, output, source, start_pat, end_pat) in [
            // Insert after whitespace.
            (
                "#[ink::contract]",
                "\n#[ink::contract]",
                r#"
#![cfg_attr(not(feature = "std"), no_std, no_main)]
"#,
                Some("\n->"),
                Some("\n->"),
            ),
            (
                "mod contract {}",
                "mod contract {}\n\n",
                r#"
#![cfg_attr(not(feature = "std"), no_std, no_main)]

#[cfg(test)]
mod tests {}"#,
                Some("<-#[cfg(test)]"),
                Some("<-#[cfg(test)]"),
            ),
            (
                "mod contract {}",
                "\nmod contract {}\n",
                r#"
#![cfg_attr(not(feature = "std"), no_std, no_main)]

#[cfg(test)]
mod tests {}"#,
                Some("<-\n#[cfg(test)]"),
                Some("<-\n#[cfg(test)]"),
            ),
            (
                "mod contract {}",
                "\nmod contract {}\n",
                "#![cfg_attr(not(feature = \"std\"), no_std)]\n \n#[cfg(test)]\nmod tests {}",
                Some("<- \n#[cfg(test)]"),
                Some("<-\n#[cfg(test)]"),
            ),
            (
                "mod contract {}",
                "mod contract {}\n",
                r#"
#![cfg_attr(not(feature = "std"), no_std, no_main)]


#[cfg(test)]
mod tests {}"#,
                Some("<-\n#[cfg(test)]"),
                Some("<-\n#[cfg(test)]"),
            ),
            (
                "#[ink::contract]",
                "#[ink::contract]\n",
                "mod contract {}",
                Some("<-mod contract {"),
                Some("<-mod contract {"),
            ),
            (
                "#[ink::contract]",
                "#[ink::contract]\n",
                r"
#[doc(hidden)]
mod contract {}",
                Some("<-mod contract {"),
                Some("<-mod contract {"),
            ),
            (
                "#[ink::contract]",
                "#[ink::contract]\n",
                "#[doc(hidden)]\nmod contract {}",
                Some("<-#[doc(hidden)]"),
                Some("<-#[doc(hidden)]"),
            ),
            (
                "#[ink::contract]",
                "#[ink::contract]\n",
                r"
#[doc(hidden)]
mod contract {}",
                Some("<-#[doc(hidden)]"),
                Some("<-#[doc(hidden)]"),
            ),
            (
                "#[ink::contract]",
                "#[ink::contract]\n",
                r"
#[doc(hidden)]
mod contract {}",
                Some("<-mod contract {"),
                Some("<-mod contract {"),
            ),
            (
                "#[ink(storage)]",
                "#[ink(storage)]\n    ",
                r"
mod contract {
    struct MyContract {}
}",
                Some("<-struct MyContract {}"),
                Some("<-struct MyContract {}"),
            ),
            (
                "#[ink(topic)]",
                "#[ink(topic)]\n        ",
                r"
mod contract {
    struct MyEvent {
        status: bool,
    }
}",
                Some("<-status: bool,"),
                Some("<-status: bool,"),
            ),
            (
                "#[ink(impl)]",
                "#[ink(impl)]\n    ",
                r"
mod contract {
    impl MyContract {}
}",
                Some("<-impl MyContract {}"),
                Some("<-impl MyContract {}"),
            ),
            (
                "#[ink(impl)]",
                "#[ink(impl)]\n    ",
                r#"
mod contract {
    #[ink(namespace = "my_namespace")]
    impl MyContract {}
}
                "#,
                Some(r#"<-#[ink(namespace = "my_namespace")]"#),
                Some(r#"<-#[ink(namespace = "my_namespace")]"#),
            ),
            (
                "#[ink(message)]",
                "#[ink(message)]\n        ",
                r"
mod contract {
    impl MyContract {
        pub fn message(&self) {}
    }
}",
                Some("<-pub fn message(&self) {}"),
                Some("<-pub fn message(&self) {}"),
            ),
            // Insert at the beginning of block.
            (
                "struct MyContract {}",
                "\n    struct MyContract {}\n",
                r"
mod contract {
}",
                Some("mod contract {"),
                Some("mod contract {"),
            ),
            (
                "status: bool,",
                "\n        status: bool,\n",
                r"
mod contract {
    struct MyContract {
    }
}",
                Some("struct MyContract {"),
                Some("struct MyContract {"),
            ),
            (
                "impl MyContract {}",
                "\n    impl MyContract {}\n",
                r"
mod contract {
}",
                Some("mod contract {"),
                Some("mod contract {"),
            ),
            (
                "pub fn message(&self) {}",
                "\n        pub fn message(&self) {}\n",
                r"
mod contract {
    impl MyContract {
    }
}",
                Some("impl MyContract {"),
                Some("impl MyContract {"),
            ),
            // Insert at the end of a statement or block or after a comment.
            (
                "struct MyEvent {}",
                "\n\n    struct MyEvent {}",
                r"
mod contract {
    struct MyContract {}
}",
                Some("struct MyContract {}"),
                Some("struct MyContract {}"),
            ),
            (
                "struct MyEvent {}",
                "\n\n    struct MyEvent {}",
                r"
mod contract {
    struct MyContract;
}",
                Some("struct MyContract;"),
                Some("struct MyContract;"),
            ),
            (
                "struct MyEvent {}",
                "\n\n    struct MyEvent {}",
                r"
mod contract {
    struct MyContract {}

    struct MyOtherEvent {}
}",
                Some("struct MyContract {}"),
                Some("struct MyContract {}"),
            ),
            (
                "struct MyEvent {}",
                "\n\n    struct MyEvent {}",
                r"
mod contract {
    struct MyContract {}
    struct MyOtherEvent {}
}",
                Some("struct MyContract {}"),
                Some("struct MyContract {}"),
            ),
            (
                "pub fn message(&self) {}",
                "\n\n        pub fn message(&self) {}",
                r"
mod contract {
    impl MyContract {
        pub fn constructor() {}
    }
}",
                Some("pub fn constructor() {}"),
                Some("pub fn constructor() {}"),
            ),
            (
                "pub fn message(&self) {}",
                "\n\n        pub fn message(&self) {}",
                r"
mod contract {
    impl MyContract {
        pub fn constructor() {}

        pub fn message2(&self) {}
    }
}",
                Some("pub fn constructor() {}"),
                Some("pub fn constructor() {}"),
            ),
            (
                "pub fn message(&self) {}",
                "\n\n        pub fn message(&self) {}",
                r"
mod contract {
    impl MyContract {
        pub fn constructor() {}
        pub fn message2(&self) {}
    }
}",
                Some("pub fn constructor() {}"),
                Some("pub fn constructor() {}"),
            ),
            // Everything else should remain unchanged.
            (
                "(env = crate::MyEnvironment)",
                "(env = crate::MyEnvironment)",
                r"
#[ink::contract]
mod contract {}",
                Some("#[ink::contract"),
                Some("#[ink::contract"),
            ),
            (
                "env = crate::MyEnvironment",
                "env = crate::MyEnvironment",
                r"
#[ink::contract()]
mod contract {}",
                Some("#[ink::contract("),
                Some("#[ink::contract("),
            ),
            (
                r#", keep_attr = "foo,bar""#,
                r#", keep_attr = "foo,bar""#,
                r"
#[ink::contract(env = crate::MyEnvironment)]
mod contract {}",
                Some("#[ink::contract(env = crate::MyEnvironment"),
                Some("#[ink::contract(env = crate::MyEnvironment"),
            ),
            (
                "crate::MyEnvironment",
                "crate::MyEnvironment",
                r"
#[ink::contract(env = self::MyEnvironment)]
mod contract {}",
                Some("#[ink::contract(env = "),
                Some("#[ink::contract(env = self::MyEnvironment"),
            ),
            (
                " crate::MyEnvironment",
                " crate::MyEnvironment",
                r"
#[ink::contract(env = self::MyEnvironment)]
mod contract {}",
                Some("#[ink::contract(env ="),
                Some("#[ink::contract(env = self::MyEnvironment"),
            ),
            (
                "&self",
                "&self",
                "pub fn message() {}",
                Some("pub fn message("),
                Some("pub fn message("),
            ),
            (
                ", status: bool",
                ", status: bool",
                "pub fn message(&self) {}",
                Some("pub fn message(&self"),
                Some("pub fn message(&self"),
            ),
            (
                " status: bool",
                " status: bool",
                "pub fn message(&self,) {}",
                Some("pub fn message(&self,"),
                Some("pub fn message(&self,"),
            ),
            (
                "status: bool",
                "status: bool",
                "pub fn message(&self, ) {}",
                Some("pub fn message(&self, "),
                Some("pub fn message(&self, "),
            ),
            (
                " -> u8",
                " -> u8",
                "pub fn message(&self) {}",
                Some("pub fn message(&self)"),
                Some("pub fn message(&self)"),
            ),
            (
                "-> u8",
                "-> u8",
                "pub fn message(&self) {}",
                Some("pub fn message(&self) "),
                Some("pub fn message(&self) "),
            ),
        ] {
            let file = InkFile::parse(source);
            let range = TextRange::new(
                TextSize::from(parse_offset_at(source, start_pat).unwrap() as u32),
                TextSize::from(parse_offset_at(source, end_pat).unwrap() as u32),
            );
            let edit = TextEdit {
                text: input.to_owned(),
                range,
                snippet: None,
            };
            let result = format_edit(edit, &file);
            let expected = TextEdit {
                text: output.to_owned(),
                range,
                snippet: None,
            };
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn format_delete_works() {
        for (start_pat_input, end_pat_input, pat_range_output, source) in [
            // Removes space after delete if its surrounded by whitespace and
            // the next token after trailing whitespace is not a closing curly bracket.
            (
                Some("<-#[ink::contract]"),
                Some("#[ink::contract]"),
                Some((Some("<-#[ink::contract]"), Some("<-mod contract {}"))),
                "#[ink::contract]\nmod contract {}",
            ),
            (
                Some("<-#[ink::contract]"),
                Some("#[ink::contract]"),
                Some((Some("<-#[ink::contract]"), Some("<-mod contract {}"))),
                r"
#[ink::contract]
mod contract {}
",
            ),
            (
                Some("<-#[ink::contract]"),
                Some("#[ink::contract]"),
                Some((Some("<-#[ink::contract]"), Some("<-mod contract {}"))),
                r"
#[doc(hidden)]
#[ink::contract]
mod contract {}",
            ),
            (
                Some("<-#[ink::contract]"),
                Some("#[ink::contract]"),
                Some((Some("<-#[ink::contract]"), Some("<-#[doc(hidden)]"))),
                r"
#[ink::contract]
#[doc(hidden)]
mod contract {}",
            ),
            (
                Some("<-#[ink(storage)]"),
                Some("#[ink(storage)]"),
                Some((Some("<-#[ink(storage)]"), Some("<-struct MyContract {}"))),
                r"
mod contract {
    #[ink(storage)]
    struct MyContract {}
}",
            ),
            (
                Some("<-#[ink(topic)]"),
                Some("#[ink(topic)]"),
                Some((Some("<-#[ink(topic)]"), Some("<-status: bool,"))),
                r"
mod contract {
    struct MyEvent {
        #[ink(topic)]
        status: bool,
    }
}",
            ),
            (
                Some("<-#[ink(impl)]"),
                Some("#[ink(impl)]"),
                Some((Some("<-#[ink(impl)]"), Some("<-impl MyContract {}"))),
                r"
mod contract {
    #[ink(impl)]
    impl MyContract {}
}",
            ),
            (
                Some("<-#[ink(impl)]"),
                Some("#[ink(impl)]"),
                Some((
                    Some("<-#[ink(impl)]"),
                    Some(r#"<-#[ink(namespace = "my_namespace")]"#),
                )),
                r#"
mod contract {
    #[ink(impl)]
    #[ink(namespace = "my_namespace")]
    impl MyContract {}
}"#,
            ),
            (
                Some("<-#[ink(message)]"),
                Some("#[ink(message)]"),
                Some((
                    Some("<-#[ink(message)]"),
                    Some("<-pub fn message(&self) {}"),
                )),
                r"
mod contract {
    impl MyContract {
        #[ink(message)]
        pub fn message(&self) {}
    }
}",
            ),
            (
                Some("<--> u8"),
                Some("-> u8"),
                Some((Some("<--> u8"), Some("-> u8 "))),
                "pub fn message(&self) -> u8 {}",
            ),
            (
                Some("<-struct MyEvent {}"),
                Some("struct MyEvent {}"),
                Some((
                    Some("<-struct MyEvent {}"),
                    Some("<-struct MyOtherEvent {}"),
                )),
                r"
mod contract {
    struct MyContract {}

    struct MyEvent {}

    struct MyOtherEvent {}
}",
            ),
            (
                Some("<-struct MyEvent {}"),
                Some("struct MyEvent {}"),
                Some((
                    Some("<-struct MyEvent {}"),
                    Some("<-struct MyOtherEvent {}"),
                )),
                r"
mod contract {
    struct MyContract {}
    struct MyEvent {}
    struct MyOtherEvent {}
}",
            ),
            (
                Some("<-pub fn message(&self) {}"),
                Some("pub fn message(&self) {}"),
                Some((
                    Some("<-pub fn message(&self) {}"),
                    Some("<-pub fn message2(&self) {}"),
                )),
                r"
mod contract {
    impl MyContract {
        pub fn constructor() {}

        pub fn message(&self) {}

        pub fn message2(&self) {}
    }
}",
            ),
            (
                Some("<-pub fn message(&self) {}"),
                Some("pub fn message(&self) {}"),
                Some((
                    Some("<-pub fn message(&self) {}"),
                    Some("<-pub fn message2(&self) {}"),
                )),
                r"
mod contract {
    impl MyContract {
        pub fn constructor() {}
        pub fn message(&self) {}
        pub fn message2(&self) {}
    }
}",
            ),
            // Everything else should remain unchanged.
            (
                Some("<-struct MyContract {}"),
                Some("struct MyContract {}"),
                None,
                r"
mod contract {
    struct MyContract {}
}",
            ),
            (
                Some("<-status: bool,"),
                Some("status: bool,"),
                None,
                r"
mod contract {
    struct MyContract {
        status: bool,
    }
}",
            ),
            (
                Some("<-impl MyContract {}"),
                Some("impl MyContract {}"),
                None,
                r"
mod contract {
    impl MyContract {}
}",
            ),
            (
                Some("<-pub fn message(&self) {}"),
                Some("pub fn message(&self) {}"),
                None,
                r"
mod contract {
    impl MyContract {
        pub fn message(&self) {}
    }
}",
            ),
            (
                Some("<-struct MyEvent {}"),
                Some("struct MyEvent {}"),
                None,
                r"
mod contract {
    struct MyContract {}

    struct MyEvent {}
}",
            ),
            (
                Some("<-struct MyEvent {}"),
                Some("struct MyEvent {}"),
                None,
                r"
mod contract {
    struct MyContract;

    struct MyEvent {}
}",
            ),
            (
                Some("<-pub fn message(&self) {}"),
                Some("pub fn message(&self) {}"),
                None,
                r"
mod contract {
    impl MyContract {
        pub fn constructor() {}

        pub fn message(&self) {}
    }
}",
            ),
            (
                Some("<-(env = crate::MyEnvironment)"),
                Some("(env = crate::MyEnvironment)"),
                None,
                r"
#[ink::contract(env = crate::MyEnvironment)]
mod contract {}",
            ),
            (
                Some("<-env = crate::MyEnvironment"),
                Some("env = crate::MyEnvironment"),
                None,
                r"
#[ink::contract(env = crate::MyEnvironment)]
mod contract {}",
            ),
            (
                Some(r#"<-, keep_attr = "foo,bar""#),
                Some(r#", keep_attr = "foo,bar""#),
                None,
                r#"
#[ink::contract(env = crate::MyEnvironment, keep_attr = "foo,bar")]
mod contract {}"#,
            ),
            (
                Some("<-crate::MyEnvironment"),
                Some("crate::MyEnvironment"),
                None,
                r"
#[ink::contract(env = crate::MyEnvironment)]
mod contract {}",
            ),
            (
                Some("<- crate::MyEnvironment"),
                Some(" crate::MyEnvironment"),
                None,
                r"
#[ink::contract(env = crate::MyEnvironment)]
mod contract {}",
            ),
            (
                Some("<-&self"),
                Some("&self"),
                None,
                "pub fn message(&self) {}",
            ),
            (
                Some("<-, status: bool"),
                Some(", status: bool"),
                None,
                "pub fn message(&self, status: bool) {}",
            ),
            (
                Some("<- status: bool"),
                Some(" status: bool"),
                None,
                "pub fn message(&self, status: bool) {}",
            ),
            (
                Some("<-status: bool"),
                Some("status: bool"),
                None,
                "pub fn message(&self, status: bool) {}",
            ),
            (
                Some("<- -> u8"),
                Some(" -> u8"),
                None,
                "pub fn message(&self) -> u8 {}",
            ),
        ] {
            let file = InkFile::parse(source);
            let range_input = TextRange::new(
                TextSize::from(parse_offset_at(source, start_pat_input).unwrap() as u32),
                TextSize::from(parse_offset_at(source, end_pat_input).unwrap() as u32),
            );
            let range_output =
                pat_range_output.map_or(range_input, |(start_pat_output, end_pat_output)| {
                    TextRange::new(
                        TextSize::from(parse_offset_at(source, start_pat_output).unwrap() as u32),
                        TextSize::from(parse_offset_at(source, end_pat_output).unwrap() as u32),
                    )
                });

            let edit = TextEdit {
                text: String::new(),
                range: range_input,
                snippet: None,
            };
            let result = format_edit(edit, &file);
            let expected = TextEdit {
                text: String::new(),
                range: range_output,
                snippet: None,
            };
            assert_eq!(result, expected);
        }
    }
}
