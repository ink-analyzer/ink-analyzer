//! offset-based ink! entity tree traversal types and abstractions.

use ra_ap_syntax::{ast, AstNode, SyntaxKind, SyntaxNode, SyntaxToken, TextSize, TokenAtOffset};

use super::ast_ext;
use crate::attrs::InkAttribute;

/// A wrapper for offset-based ink! entity tree traversal methods.
#[derive(Debug, Clone)]
pub struct ItemAtOffset {
    /// The wrapped representation of a token in the subtree that covers the offset.
    token_at_offset: TokenAtOffset<SyntaxToken>,
    /// The position.
    offset: TextSize,
}

impl ItemAtOffset {
    /// Creates a representation of a token in the subtree that covers the offset.
    pub fn new(node: &SyntaxNode, offset: TextSize) -> Self {
        Self {
            token_at_offset: node.token_at_offset(offset),
            offset,
        }
    }

    /// Returns the wrapped `TokenAtOffset` representation.
    pub fn token_at_offset(&self) -> &TokenAtOffset<SyntaxToken> {
        &self.token_at_offset
    }

    /// Returns the token (if any) that's in focus at the offset.
    ///
    /// It's left biased, in the case of an offset between 2 tokens unless the left token is trivia while the right token is not.
    pub fn focused_token(&self) -> Option<&SyntaxToken> {
        match &self.token_at_offset {
            TokenAtOffset::Single(token) => Some(token),
            TokenAtOffset::Between(left, right) => {
                // We're biased towards the left token unless it's trivia while the right token is not.
                if left.kind().is_trivia() && !right.kind().is_trivia() {
                    Some(right)
                } else {
                    Some(left)
                }
            }
            TokenAtOffset::None => None,
        }
    }

    /// Returns the relative offset in the focused token (if any).
    fn offset_in_focused_token(&self) -> Option<TextSize> {
        self.focused_token()
            .map(|token| self.offset - token.text_range().start())
    }

    /// Returns the offset affixes of the focused token (if any).
    fn focused_token_affixes(&self) -> Option<(&str, &str)> {
        self.focused_token().and_then(|token| {
            self.offset_in_focused_token()
                .map(|offset| token.text().split_at(offset.into()))
        })
    }

    /// Returns the offset prefix of the focused token (if any).
    pub fn focused_token_prefix(&self) -> Option<&str> {
        self.focused_token_affixes()
            .and_then(|(prefix, _)| (!prefix.is_empty()).then_some(prefix))
    }

    /// Returns the offset suffix of the focused token (if any).
    pub fn focused_token_suffix(&self) -> Option<&str> {
        self.focused_token_affixes()
            .and_then(|(_, suffix)| (!suffix.is_empty()).then_some(suffix))
    }

    /// Returns the previous token in the tree (i.e not necessary a sibling).
    pub fn prev_token(&self) -> Option<SyntaxToken> {
        self.focused_token().and_then(SyntaxToken::prev_token)
    }

    /// Returns the next token in the tree (i.e not necessary a sibling).
    pub fn next_token(&self) -> Option<SyntaxToken> {
        self.focused_token().and_then(SyntaxToken::next_token)
    }

    /// Returns the closest previous token in the tree (i.e not necessary a sibling) that's not trivia.
    pub fn prev_non_trivia_token(&self) -> Option<SyntaxToken> {
        self.focused_token()
            .and_then(|token| ast_ext::closest_non_trivia_token(token, SyntaxToken::prev_token))
    }

    /// Returns the closest next token in the tree (i.e not necessary a sibling) that's not trivia.
    pub fn next_non_trivia_token(&self) -> Option<SyntaxToken> {
        self.focused_token()
            .and_then(|token| ast_ext::closest_non_trivia_token(token, SyntaxToken::next_token))
    }

    /// Returns the parent attribute of the focused token (if any).
    pub fn parent_attr(&self) -> Option<ast::Attr> {
        self.focused_token()
            .and_then(ast_ext::closest_ancestor_ast_type)
    }

    /// Returns the parent ink! attribute of the focused token (if any).
    pub fn parent_ink_attr(&self) -> Option<InkAttribute> {
        self.parent_attr().and_then(InkAttribute::cast)
    }

    /// Returns the parent AST item of the focused token (if any).
    pub fn parent_ast_item(&self) -> Option<ast::Item> {
        match self.parent_attr() {
            // Unclosed attributes are tricky and can yield the wrong parent AST item.
            Some(attr) => match attr.r_brack_token() {
                // So we only return an AST item if the attribute is closed (i.e has a closing right bracket token).
                Some(_) => ast_ext::parent_ast_item(attr.syntax()),
                // And return None otherwise,
                // see `probable_parent_ast_item_keyword` and `normalized_parent_ast_item_keyword`
                // for more robust solutions for unclosed attributes.
                None => None,
            },
            // Non-attributes are straight forward.
            None => self.focused_token().and_then(ast_ext::parent_ast_item),
        }
    }

    /// Returns the probable parent attribute of the focused token (if any) and
    /// a bool representing whether or not it covers the focused token.
    ///
    /// This is useful in cases when the parent attribute is unclosed making it tricky to determine where it ends.
    pub fn probable_unclosed_parent_attr(&self) -> Option<(ast::Attr, bool)> {
        match self.parent_attr() {
            Some(attr) => attr.r_brack_token().is_none().then_some((attr, true)),
            None => self
                .prev_non_trivia_token()
                .and_then(|prev_token| {
                    ast_ext::closest_ancestor_ast_type::<SyntaxToken, ast::Attr>(&prev_token)
                })
                .and_then(|prev_attr| {
                    prev_attr
                        .r_brack_token()
                        .is_none()
                        .then_some((prev_attr, false))
                }),
        }
    }

    /// Returns the probable parent ink! attribute of the focused token (if any) and
    /// a bool representing whether or not it covers the focused token.
    ///
    /// See `probable_unclosed_parent_attr` doc.
    pub fn probable_unclosed_parent_ink_attr(&self) -> Option<(InkAttribute, bool)> {
        self.probable_unclosed_parent_attr()
            .and_then(|(attr, is_covering)| {
                InkAttribute::cast(attr).map(|ink_attr| (ink_attr, is_covering))
            })
    }

    /// Returns the probable parent AST item keyword of the focused token (if any) and a
    /// bool representing whether or not its covered by the unclosed attribute covering the focused token.
    ///
    /// This is useful in cases where an unclosed parent attribute can cover
    /// the real parent AST item leaving it unrecognized by the parser.
    /// See `probable_unclosed_parent_attr` doc.
    pub fn probable_parent_ast_item_keyword(&self) -> Option<(SyntaxToken, bool)> {
        self.probable_unclosed_parent_attr()
            .and_then(|(attr, is_covering)| {
                self.focused_token().and_then(|start_token| {
                    ast_ext::closest_item_which(
                        start_token,
                        SyntaxToken::next_token,
                        |subject| {
                            matches!(
                                subject.kind(),
                                // We only care about AST item keywords that can be annotated with ink! attributes.
                                SyntaxKind::MOD_KW
                                    | SyntaxKind::TRAIT_KW
                                    | SyntaxKind::ENUM_KW
                                    | SyntaxKind::STRUCT_KW
                                    | SyntaxKind::UNION_KW
                                    | SyntaxKind::FN_KW
                                    | SyntaxKind::IMPL_KW
                            )
                        },
                        // We only continue searching if the current token is either trivia, punctuation or the `pub` keyword.
                        |subject| {
                            !subject.kind().is_trivia()
                                && !subject.kind().is_punct()
                                && subject.kind() != SyntaxKind::PUB_KW
                        },
                    )
                    .map(|keyword| {
                        let is_covered = is_covering
                            && ast_ext::closest_ancestor_ast_type::<SyntaxToken, ast::Attr>(
                                &keyword,
                            )
                            .map(|attr| attr.syntax().text_range())
                                == Some(attr.syntax().text_range());
                        (keyword, is_covered)
                    })
                })
            })
    }

    /// Returns the "normalized" parent attribute of the focused token (if any),
    /// a bool representing which variant was used
    /// (i.e certain i.e `parent_attr` or probable i.e `probable_unclosed_parent_attr`) and
    /// a bool representing whether or not the parent attribute covers the focused token.
    ///
    /// The certain variant i.e `parent_attr` is preferred but
    /// the probable variant i.e `probable_unclosed_parent_attr` is used as a fallback.
    /// It therefore provides "normalized" API for retrieving the parent attribute that's
    /// biased towards certainty (if possible) but also remains robustness in cases where certainty is not possible.
    ///
    /// See `parent_attr` and `probable_unclosed_parent_attr` docs.
    pub fn normalized_parent_attr(&self) -> Option<(ast::Attr, bool, bool)> {
        match self.parent_attr() {
            Some(attr) => Some((attr, true, true)),
            None => self
                .probable_unclosed_parent_attr()
                .map(|(attr, is_covering)| (attr, false, is_covering)),
        }
    }

    /// Returns the "normalized" parent ink! attribute of the focused token (if any),
    /// a bool representing which variant was used
    /// (i.e certain i.e `parent_ink_attr` or probable i.e `probable_unclosed_parent_ink_attr`) and
    /// a bool representing whether or not the parent ink! attribute covers the focused token.
    ///
    /// See `normalized_parent_attr`, `parent_ink_attr` and `probable_unclosed_parent_ink_attr` docs.
    pub fn normalized_parent_ink_attr(&self) -> Option<(InkAttribute, bool, bool)> {
        match self.parent_ink_attr() {
            Some(attr) => Some((attr, true, true)),
            None => self
                .probable_unclosed_parent_ink_attr()
                .map(|(attr, is_covering)| (attr, false, is_covering)),
        }
    }

    /// Returns the "normalized" parent AST item keyword of the focused token (if any),
    /// a bool representing which variant was used
    /// (i.e certain i.e `parent_ast_item` or probable i.e `probable_parent_ast_item_keyword`) and a
    /// bool representing whether or not the AST item keyword is covered by the
    /// unclosed attribute covering the focused token.
    ///
    /// See `normalized_parent_attr`, `parent_ast_item` and `probable_parent_ast_item_keyword` docs.
    pub fn normalized_parent_ast_item_keyword(&self) -> Option<(SyntaxToken, bool, bool)> {
        match self.parent_ast_item() {
            Some(item) => match item {
                // We only care about AST item keywords that can be annotated with ink! attributes.
                ast::Item::Module(item) => item.mod_token(),
                ast::Item::Trait(item) => item.trait_token(),
                ast::Item::Enum(item) => item.enum_token(),
                ast::Item::Struct(item) => item.struct_token(),
                ast::Item::Union(item) => item.union_token(),
                ast::Item::Fn(item) => item.fn_token(),
                ast::Item::Impl(item) => item.impl_token(),
                _ => None,
            }
            .map(|keyword| (keyword, true, false)),
            None => self
                .probable_parent_ast_item_keyword()
                .map(|(keyword, is_covered)| (keyword, false, is_covered)),
        }
    }

    /// Returns the syntax kind for the "normalized parent item".
    /// (See [`ItemAtOffset::normalized_parent_ast_item_keyword`] doc for details about "normalization" in this context).
    pub fn normalized_parent_item_syntax_kind(&self) -> Option<SyntaxKind> {
        match self.normalized_parent_ast_item_keyword() {
            // Returns syntax kind based on the AST item type keyword.
            Some((ast_item_keyword, ..)) => Some(ast_item_keyword.kind()),
            // Handles cases where either the AST item type is unknown or
            // the ink! attribute is not applied to an AST item (e.g. ink! topic).
            None => {
                // Checks whether the parent is a struct `RecordField`.
                // `RecordFieldList` is also matched for cases where the ink! attribute is
                // unclosed and so the field is parsed as if it's part of the attribute.
                self.normalized_parent_ink_attr()
                    .and_then(|(ink_attr, ..)| {
                        ink_attr.syntax().parent().and_then(|parent| {
                            (matches!(
                                parent.kind(),
                                SyntaxKind::RECORD_FIELD | SyntaxKind::RECORD_FIELD_LIST
                            ) && matches!(
                                ast_ext::parent_ast_item(&parent),
                                Some(ast::Item::Struct(_))
                            ))
                            .then_some(
                                if ink_attr.ast().r_brack_token().is_some() {
                                    parent.kind()
                                } else {
                                    SyntaxKind::RECORD_FIELD
                                },
                            )
                        })
                    })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ItemAtOffset;
    use crate::{InkArgKind, InkAttributeKind, InkMacroKind};
    use ra_ap_syntax::{AstNode, SourceFile, SyntaxKind, TextSize};
    use test_utils::parse_offset_at;

    fn parse_item_at_offset(code: &str, offset: TextSize) -> ItemAtOffset {
        ItemAtOffset::new(SourceFile::parse(code).tree().syntax(), offset)
    }

    #[test]
    fn focused_token_and_affixes_works() {
        for (code, scenarios) in [
            // (code, [(pat, token, prefix, suffix)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // token = focused token,
            // prefix = chars before the offset in the focused token,
            // suffix = chars after the offset in the focused token.
            (
                "hello, world!",
                vec![
                    // Inside token.
                    (Some("el"), Some("hello"), Some("hel"), Some("lo")),
                    (Some("or"), Some("world"), Some("wor"), Some("ld")),
                    // Between tokens.
                    (Some(","), Some(","), Some(","), None),
                    (Some(" "), Some("world"), None, Some("world")),
                    (Some("hello"), Some("hello"), Some("hello"), None),
                    (Some("world"), Some("world"), Some("world"), None),
                    (Some("<-world"), Some("world"), None, Some("world")),
                    // First or last token.
                    (Some(""), Some("hello"), None, Some("hello")),
                    (Some("!"), Some("!"), Some("!"), None),
                    (None, Some("!"), Some("!"), None),
                ],
            ),
            (
                "let x = 10 + 2;",
                vec![
                    // Inside token.
                    (Some("le"), Some("let"), Some("le"), Some("t")),
                    (Some("1"), Some("10"), Some("1"), Some("0")),
                    // Between tokens.
                    (Some("x"), Some("x"), Some("x"), None),
                    (Some("<-x"), Some("x"), None, Some("x")),
                    (Some("="), Some("="), Some("="), None),
                    (Some("0"), Some("10"), Some("10"), None),
                    (Some("+"), Some("+"), Some("+"), None),
                    (Some("2"), Some("2"), Some("2"), None),
                    // First or last token.
                    (Some(""), Some("let"), None, Some("let")),
                    (Some(";"), Some(";"), Some(";"), None),
                    (None, Some(";"), Some(";"), None),
                ],
            ),
            (
                "mod my_contract {}",
                vec![
                    // Inside token.
                    (Some("mo"), Some("mod"), Some("mo"), Some("d")),
                    (
                        Some("my"),
                        Some("my_contract"),
                        Some("my"),
                        Some("_contract"),
                    ),
                    (
                        Some("_"),
                        Some("my_contract"),
                        Some("my_"),
                        Some("contract"),
                    ),
                    // Between tokens.
                    (Some("mod"), Some("mod"), Some("mod"), None),
                    (Some(" "), Some("my_contract"), None, Some("my_contract")),
                    (
                        Some("contract"),
                        Some("my_contract"),
                        Some("my_contract"),
                        None,
                    ),
                    (Some("{"), Some("{"), Some("{"), None),
                    // First or last token.
                    (Some(""), Some("mod"), None, Some("mod")),
                    (Some("}"), Some("}"), Some("}"), None),
                    (None, Some("}"), Some("}"), None),
                ],
            ),
            (
                r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#,
                vec![
                    // Inside token.
                    (Some("in"), Some("ink"), Some("in"), Some("k")),
                    (Some("con"), Some("contract"), Some("con"), Some("tract")),
                    (Some("en"), Some("env"), Some("en"), Some("v")),
                    (Some("keep"), Some("keep_attr"), Some("keep"), Some("_attr")),
                    (Some("_"), Some("keep_attr"), Some("keep_"), Some("attr")),
                    (Some("Ty"), Some("Types"), Some("Ty"), Some("pes")),
                    (Some(":"), Some("::"), Some(":"), Some(":")),
                    // Between tokens.
                    (Some("#"), Some("#"), Some("#"), None),
                    (Some("["), Some("["), Some("["), None),
                    (Some("::"), Some("::"), Some("::"), None),
                    (Some("ink"), Some("ink"), Some("ink"), None),
                    (Some("contract"), Some("contract"), Some("contract"), None),
                    (Some("("), Some("("), Some("("), None),
                    (Some("Types"), Some("Types"), Some("Types"), None),
                    (Some(","), Some(","), Some(","), None),
                    (Some(")"), Some(")"), Some(")"), None),
                    // First or last token.
                    (Some(""), Some("#"), None, Some("#")),
                    (Some("]"), Some("]"), Some("]"), None),
                    (None, Some("]"), Some("]"), None),
                ],
            ),
        ] {
            for (pat, expected_token, expected_prefix, expected_suffix) in scenarios {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
                let item = parse_item_at_offset(code, offset);

                // Focused token.
                assert_eq!(
                    item.focused_token().map(|token| token.text()),
                    expected_token
                );

                // Focused token prefix.
                assert_eq!(item.focused_token_prefix(), expected_prefix);

                // Focused token suffix.
                assert_eq!(item.focused_token_suffix(), expected_suffix);
            }
        }
    }

    #[test]
    fn prev_and_next_token_variants_works() {
        for (code, scenarios) in [
            // (code, [(pat, focused_token, prev_token, next_token, prev_non_trivia_token, next_non_trivia_token)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // focused_token = focused token,
            // prev_token = previous token in the tree,
            // next_token = next token in the tree,
            // prev_non_trivia_token = closest previous token in the tree that is not trivia (i.e not space or comment),
            // next_non_trivia_token = closest previous token in the tree that is not trivia (i.e not space or comment).
            (
                "hello/*comment*/, world!",
                vec![
                    // Inside token.
                    (
                        Some("el"),
                        Some("hello"),
                        None,
                        Some("/*comment*/"),
                        None,
                        Some(","),
                    ),
                    (
                        Some("or"),
                        Some("world"),
                        Some(" "),
                        Some("!"),
                        Some(","),
                        Some("!"),
                    ),
                    (
                        Some("comm"),
                        Some("/*comment*/"),
                        Some("hello"),
                        Some(","),
                        Some("hello"),
                        Some(","),
                    ),
                    // Between tokens.
                    (
                        Some(","),
                        Some(","),
                        Some("/*comment*/"),
                        Some(" "),
                        Some("hello"),
                        Some("world"),
                    ),
                    (
                        Some(" "),
                        Some("world"),
                        Some(" "),
                        Some("!"),
                        Some(","),
                        Some("!"),
                    ),
                    (
                        Some("hello"),
                        Some("hello"),
                        None,
                        Some("/*comment*/"),
                        None,
                        Some(","),
                    ),
                    (
                        Some("world"),
                        Some("world"),
                        Some(" "),
                        Some("!"),
                        Some(","),
                        Some("!"),
                    ),
                    (
                        Some("<-world"),
                        Some("world"),
                        Some(" "),
                        Some("!"),
                        Some(","),
                        Some("!"),
                    ),
                    // First or last token.
                    (
                        Some(""),
                        Some("hello"),
                        None,
                        Some("/*comment*/"),
                        None,
                        Some(","),
                    ),
                    (
                        Some("!"),
                        Some("!"),
                        Some("world"),
                        None,
                        Some("world"),
                        None,
                    ),
                    (None, Some("!"), Some("world"), None, Some("world"), None),
                ],
            ),
            (
                "let x = 10 + 2;",
                vec![
                    // Inside token.
                    (Some("le"), Some("let"), None, Some(" "), None, Some("x")),
                    (
                        Some("1"),
                        Some("10"),
                        Some(" "),
                        Some(" "),
                        Some("="),
                        Some("+"),
                    ),
                    // Between tokens.
                    (
                        Some("x"),
                        Some("x"),
                        Some(" "),
                        Some(" "),
                        Some("let"),
                        Some("="),
                    ),
                    (
                        Some("<-x"),
                        Some("x"),
                        Some(" "),
                        Some(" "),
                        Some("let"),
                        Some("="),
                    ),
                    (
                        Some("="),
                        Some("="),
                        Some(" "),
                        Some(" "),
                        Some("x"),
                        Some("10"),
                    ),
                    (
                        Some("0"),
                        Some("10"),
                        Some(" "),
                        Some(" "),
                        Some("="),
                        Some("+"),
                    ),
                    (
                        Some("+"),
                        Some("+"),
                        Some(" "),
                        Some(" "),
                        Some("10"),
                        Some("2"),
                    ),
                    (
                        Some("2"),
                        Some("2"),
                        Some(" "),
                        Some(";"),
                        Some("+"),
                        Some(";"),
                    ),
                    // First or last token.
                    (Some(""), Some("let"), None, Some(" "), None, Some("x")),
                    (Some(";"), Some(";"), Some("2"), None, Some("2"), None),
                    (None, Some(";"), Some("2"), None, Some("2"), None),
                ],
            ),
            (
                "mod my_contract {}",
                vec![
                    // Inside token.
                    (
                        Some("mo"),
                        Some("mod"),
                        None,
                        Some(" "),
                        None,
                        Some("my_contract"),
                    ),
                    (
                        Some("my"),
                        Some("my_contract"),
                        Some(" "),
                        Some(" "),
                        Some("mod"),
                        Some("{"),
                    ),
                    (
                        Some("_"),
                        Some("my_contract"),
                        Some(" "),
                        Some(" "),
                        Some("mod"),
                        Some("{"),
                    ),
                    // Between tokens.
                    (
                        Some("mod"),
                        Some("mod"),
                        None,
                        Some(" "),
                        None,
                        Some("my_contract"),
                    ),
                    (
                        Some(" "),
                        Some("my_contract"),
                        Some(" "),
                        Some(" "),
                        Some("mod"),
                        Some("{"),
                    ),
                    (
                        Some("contract"),
                        Some("my_contract"),
                        Some(" "),
                        Some(" "),
                        Some("mod"),
                        Some("{"),
                    ),
                    (
                        Some("{"),
                        Some("{"),
                        Some(" "),
                        Some("}"),
                        Some("my_contract"),
                        Some("}"),
                    ),
                    // First or last token.
                    (
                        Some(""),
                        Some("mod"),
                        None,
                        Some(" "),
                        None,
                        Some("my_contract"),
                    ),
                    (Some("}"), Some("}"), Some("{"), None, Some("{"), None),
                    (None, Some("}"), Some("{"), None, Some("{"), None),
                ],
            ),
            (
                r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]"#,
                vec![
                    // Inside token.
                    (
                        Some("in"),
                        Some("ink"),
                        Some("["),
                        Some("::"),
                        Some("["),
                        Some("::"),
                    ),
                    (
                        Some("con"),
                        Some("contract"),
                        Some("::"),
                        Some("("),
                        Some("::"),
                        Some("("),
                    ),
                    (
                        Some("en"),
                        Some("env"),
                        Some("("),
                        Some("="),
                        Some("("),
                        Some("="),
                    ),
                    (
                        Some("keep"),
                        Some("keep_attr"),
                        Some(" "),
                        Some("="),
                        Some(","),
                        Some("="),
                    ),
                    (
                        Some("_"),
                        Some("keep_attr"),
                        Some(" "),
                        Some("="),
                        Some(","),
                        Some("="),
                    ),
                    (
                        Some("Ty"),
                        Some("Types"),
                        Some(":"),
                        Some(","),
                        Some(":"),
                        Some(","),
                    ), // Can we normalize ":" to "::"?
                    (
                        Some(":"),
                        Some("::"),
                        Some("ink"),
                        Some("contract"),
                        Some("ink"),
                        Some("contract"),
                    ),
                    // Between tokens.
                    (Some("#"), Some("#"), None, Some("["), None, Some("[")),
                    (
                        Some("["),
                        Some("["),
                        Some("#"),
                        Some("ink"),
                        Some("#"),
                        Some("ink"),
                    ),
                    (
                        Some("::"),
                        Some("::"),
                        Some("ink"),
                        Some("contract"),
                        Some("ink"),
                        Some("contract"),
                    ),
                    (
                        Some("ink"),
                        Some("ink"),
                        Some("["),
                        Some("::"),
                        Some("["),
                        Some("::"),
                    ),
                    (
                        Some("contract"),
                        Some("contract"),
                        Some("::"),
                        Some("("),
                        Some("::"),
                        Some("("),
                    ),
                    (
                        Some("("),
                        Some("("),
                        Some("contract"),
                        Some("env"),
                        Some("contract"),
                        Some("env"),
                    ),
                    (
                        Some("Types"),
                        Some("Types"),
                        Some(":"),
                        Some(","),
                        Some(":"),
                        Some(","),
                    ),
                    (
                        Some(","),
                        Some(","),
                        Some("Types"),
                        Some(" "),
                        Some("Types"),
                        Some("keep_attr"),
                    ),
                    (
                        Some(")"),
                        Some(")"),
                        Some(r#""foo,bar""#),
                        Some("]"),
                        Some(r#""foo,bar""#),
                        Some("]"),
                    ),
                    // First or last token.
                    (Some(""), Some("#"), None, Some("["), None, Some("[")),
                    (Some("]"), Some("]"), Some(")"), None, Some(")"), None),
                    (None, Some("]"), Some(")"), None, Some(")"), None),
                ],
            ),
        ] {
            for (
                pat,
                focused_token,
                prev_token,
                next_token,
                prev_non_trivia_token,
                next_non_trivia_token,
            ) in scenarios
            {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
                let item = parse_item_at_offset(code, offset);

                // Focused token.
                assert_eq!(
                    item.focused_token().map(|token| token.text()),
                    focused_token
                );

                // Previous token.
                assert_eq!(
                    item.prev_token().as_ref().map(|token| token.text()),
                    prev_token
                );

                // Next token.
                assert_eq!(
                    item.next_token().as_ref().map(|token| token.text()),
                    next_token
                );

                // Previous non-trivia token.
                assert_eq!(
                    item.prev_non_trivia_token()
                        .as_ref()
                        .map(|token| token.text()),
                    prev_non_trivia_token
                );

                // Next non-trivia token.
                assert_eq!(
                    item.next_non_trivia_token()
                        .as_ref()
                        .map(|token| token.text()),
                    next_non_trivia_token
                );
            }
        }
    }

    #[test]
    fn parent_variants_works() {
        for (code, scenarios) in [
            // (code, [(pat, focused_token, parent_attr, parent_ink_attr, parent_ast_item)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // focused_token = focused token,
            // parent_attr = the parent attribute item in the tree,
            // parent_ink_attr = the parent ink! attribute item in the tree,
            // parent_ast_item = the parent AST item in the tree.
            (
                "hello, world!",
                vec![
                    // Inside token.
                    (
                        Some("el"),
                        Some("hello"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (
                        Some("or"),
                        Some("world"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    // Between tokens.
                    (Some(","), Some(","), None, None, None),
                    (
                        Some(" "),
                        Some("world"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (
                        Some("hello"),
                        Some("hello"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (
                        Some("world"),
                        Some("world"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (
                        Some("<-world"),
                        Some("world"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    // First or last token.
                    (
                        Some(""),
                        Some("hello"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (
                        Some("!"),
                        Some("!"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (None, Some("!"), None, None, Some(SyntaxKind::MACRO_CALL)),
                ],
            ),
            (
                "let x = 10 + 2;",
                vec![
                    // Inside token.
                    (Some("le"), Some("let"), None, None, None),
                    (Some("1"), Some("10"), None, None, None),
                    // Between tokens.
                    (
                        Some("x"),
                        Some("x"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (
                        Some("<-x"),
                        Some("x"),
                        None,
                        None,
                        Some(SyntaxKind::MACRO_CALL),
                    ),
                    (Some("="), Some("="), None, None, None),
                    (Some("0"), Some("10"), None, None, None),
                    (Some("+"), Some("+"), None, None, None),
                    (Some("2"), Some("2"), None, None, None),
                    // First or last token.
                    (Some(""), Some("let"), None, None, None),
                    (Some(";"), Some(";"), None, None, None),
                    (None, Some(";"), None, None, None),
                ],
            ),
            (
                r#"#[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                   mod my_contract {}"#,
                vec![
                    // Inside token.
                    (
                        Some("in"),
                        Some("ink"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("::con"),
                        Some("contract"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("(en"),
                        Some("env"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("keep"),
                        Some("keep_attr"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("Ty"),
                        Some("Types"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("k:"),
                        Some("::"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("mo"),
                        Some("mod"),
                        None,
                        None,
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("my_con"),
                        Some("my_contract"),
                        None,
                        None,
                        Some(SyntaxKind::MODULE),
                    ),
                    // Between tokens.
                    (
                        Some("#"),
                        Some("#"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("["),
                        Some("["),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("::"),
                        Some("::"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("ink"),
                        Some("ink"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some(":contract"),
                        Some("contract"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("contract("),
                        Some("("),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("Types"),
                        Some("Types"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("Types,"),
                        Some(","),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some(")"),
                        Some(")"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("]"),
                        Some("]"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("<-mod"), // offset at start of mod.
                        Some("mod"),
                        None,
                        None,
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("mod"),
                        Some("mod"),
                        None,
                        None,
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("mod "),
                        Some("my_contract"),
                        None,
                        None,
                        Some(SyntaxKind::MODULE),
                    ),
                    (
                        Some("my_contract"),
                        Some("my_contract"),
                        None,
                        None,
                        Some(SyntaxKind::MODULE),
                    ),
                    (Some("{"), Some("{"), None, None, Some(SyntaxKind::MODULE)),
                    // First or last token.
                    (
                        Some(""),
                        Some("#"),
                        Some("ink::contract"),
                        Some(InkAttributeKind::Macro(InkMacroKind::Contract)),
                        Some(SyntaxKind::MODULE),
                    ),
                    (Some("}"), Some("}"), None, None, Some(SyntaxKind::MODULE)),
                    (None, Some("}"), None, None, Some(SyntaxKind::MODULE)),
                ],
            ),
        ] {
            for (pat, focused_token, parent_attr, parent_ink_attr, parent_ast_item) in scenarios {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
                let item = parse_item_at_offset(code, offset);

                // Focused token.
                assert_eq!(
                    item.focused_token().map(|token| token.text()),
                    focused_token
                );

                // Parent attribute path.
                assert_eq!(
                    item.parent_attr()
                        .and_then(|attr| attr.path().map(|path| path.syntax().to_string())),
                    parent_attr.map(|attr| attr.to_string())
                );

                // Parent ink! attribute kind.
                assert_eq!(
                    item.parent_ink_attr().map(|ink_attr| *ink_attr.kind()),
                    parent_ink_attr
                );

                // Parent AST item kind.
                assert_eq!(
                    item.parent_ast_item().map(|item| item.syntax().kind()),
                    parent_ast_item
                );
            }
        }
    }

    #[test]
    fn probable_and_normalized_parent_variants_works() {
        for (
            code,
            pat,
            focused_token,
            probable_parent_attr,
            probable_parent_ink_attr,
            probable_parent_ast_item_token,
            normalized_parent_attr,
            normalized_parent_ink_attr,
            normalized_parent_ast_item_token,
        ) in [
            // (
            //     code, pat, focused_token,
            //     (probable_parent_attr, is_covering),
            //     (probable_parent_ink_attr, is_covering),
            //     (probable_parent_ast_item_keyword, is_covered),
            //     (normalized_parent_attr, is_covering),
            //     (normalized_parent_ink_attr, is_covering),
            //     (normalized_parent_ast_item_keyword, is_covered),
            // ) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // focused_token = focused token,
            // probable_parent_attr = the probable unclosed parent attribute item in the tree,
            // probable_parent_ink_attr = the probable unclosed parent ink! attribute item in the tree,
            // probable_parent_ast_item_keyword = the probable closest parent AST item keyword in the tree,
            // normalized_parent_attr = the probable unclosed parent attribute item in the tree,
            // normalized_parent_ink_attr = the probable unclosed parent ink! attribute item in the tree,
            // normalized_parent_ast_item_keyword = the probable closest parent AST item keyword in the tree,
            // is_covering = true if the attribute is covering the focused token,
            // is_covered = true if the token is covered by the same attribute (if any) covering the focused token.
            // is_certain = for normalized variant, it's true if the item is a result from the non-probable variant of the utility.
            (
                // The single `:` is not parsed as being part of an attribute by `ra_ap_syntax`.
                "#[ink:",
                Some(":"),
                Some(":"),
                Some(("ink", false)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), false)),
                None,
                Some(("ink", false, false)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), false, false)),
                None,
            ),
            (
                "#[ink::",
                Some("::"),
                Some("::"),
                Some(("ink::", true)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), true)),
                None,
                Some(("ink::", true, true)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), true, true)),
                None,
            ),
            (
                "#[ink(",
                Some("("),
                Some("("),
                Some(("ink", true)),
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), true)),
                None,
                Some(("ink", true, true)),
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), true, true)),
                None,
            ),
            (
                "#[ink(]",
                Some("("),
                Some("("),
                Some(("ink", true)),
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), true)),
                None,
                Some(("ink", true, true)),
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), true, true)),
                None,
            ),
            (
                r#"
                    #[ink:
                    mod my_contract {}
                "#,
                Some(":"),
                Some(":"),
                Some(("ink", false)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), false)),
                Some((SyntaxKind::MOD_KW, false)),
                Some(("ink", false, false)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), false, false)),
                Some((SyntaxKind::MOD_KW, false, false)),
            ),
            (
                r#"
                    #[ink::
                    mod my_contract {}
                "#,
                Some("::"),
                Some("::"),
                Some(("ink::", true)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), true)),
                Some((SyntaxKind::MOD_KW, false)),
                Some(("ink::", true, true)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), true, true)),
                Some((SyntaxKind::MOD_KW, false, false)),
            ),
            (
                r#"
                    #[ink::contract(
                    mod my_contract {}
                "#,
                Some("("),
                Some("("),
                Some(("ink::contract", true)),
                Some((InkAttributeKind::Macro(InkMacroKind::Contract), true)),
                Some((SyntaxKind::MOD_KW, true)),
                Some(("ink::contract", true, true)),
                Some((InkAttributeKind::Macro(InkMacroKind::Contract), true, true)),
                Some((SyntaxKind::MOD_KW, false, true)),
            ),
            (
                r#"
                    #[ink(
                    struct MyStruct {}
                "#,
                Some("("),
                Some("("),
                Some(("ink", true)),
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), true)),
                Some((SyntaxKind::STRUCT_KW, true)),
                Some(("ink", true, true)),
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), true, true)),
                Some((SyntaxKind::STRUCT_KW, false, true)),
            ),
            // No false positives.
            (
                "#[ink]",
                Some("k"),
                Some("ink"),
                None,
                None,
                None,
                Some(("ink", true, true)),
                Some((InkAttributeKind::Macro(InkMacroKind::Unknown), true, true)),
                None,
            ),
            (
                "#[ink()]",
                Some("("),
                Some("("),
                None,
                None,
                None,
                Some(("ink", true, true)),
                Some((InkAttributeKind::Arg(InkArgKind::Unknown), true, true)),
                None,
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
            let item = parse_item_at_offset(code, offset);

            // Focused token.
            assert_eq!(
                item.focused_token().map(|token| token.text()),
                focused_token
            );

            // Probable parent attribute path.
            assert_eq!(
                item.probable_unclosed_parent_attr()
                    .and_then(|(attr, is_covering)| attr
                        .path()
                        .map(|path| (path.syntax().to_string(), is_covering))),
                probable_parent_attr.map(|(attr, is_covering)| (attr.to_string(), is_covering))
            );

            // Probable parent ink! attribute kind.
            assert_eq!(
                item.probable_unclosed_parent_ink_attr()
                    .map(|(ink_attr, is_covering)| (*ink_attr.kind(), is_covering)),
                probable_parent_ink_attr
            );

            // Probable parent AST item keyword kind.
            assert_eq!(
                item.probable_parent_ast_item_keyword()
                    .map(|(item, is_covered)| (item.kind(), is_covered)),
                probable_parent_ast_item_token
            );

            // Normalized parent attribute path.
            assert_eq!(
                item.normalized_parent_attr()
                    .and_then(|(attr, is_certain, is_covering)| attr.path().map(|path| (
                        path.syntax().to_string(),
                        is_certain,
                        is_covering
                    ))),
                normalized_parent_attr.map(|(attr, is_certain, is_covering)| (
                    attr.to_string(),
                    is_certain,
                    is_covering
                ))
            );

            // Normalized parent ink! attribute kind.
            assert_eq!(
                item.normalized_parent_ink_attr()
                    .map(|(ink_attr, is_certain, is_covering)| (
                        *ink_attr.kind(),
                        is_certain,
                        is_covering
                    )),
                normalized_parent_ink_attr
            );

            // Normalized parent AST item keyword kind.
            assert_eq!(
                item.normalized_parent_ast_item_keyword()
                    .map(|(item, is_certain, is_covered)| (item.kind(), is_certain, is_covered)),
                normalized_parent_ast_item_token
            );
        }
    }
}
