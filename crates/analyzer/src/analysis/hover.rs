//! ink! attribute hover content.

use ink_analyzer_ir::syntax::{AstNode, AstToken, SyntaxElement, TextRange};
use ink_analyzer_ir::{
    ast, FromSyntax, InkArgKind, InkAttribute, InkAttributeKind, InkFile, InkMacroKind, IsInkEntity,
};

mod content;

/// An ink! attribute hover result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hover {
    /// Range the hover content applies to.
    pub range: TextRange,
    /// Replacement text for the action.
    pub content: String,
}

/// Returns descriptive/informational text for the ink! attribute at the given position (if any).
pub fn hover(file: &InkFile, range: TextRange) -> Option<Hover> {
    // Finds the covering ink! attribute for the text range (if any).
    let covering_ink_attr = if range.is_empty() {
        // Uses item at offset utility if the range start and end are equal.
        file.item_at_offset(range.start()).parent_ink_attr()
    } else {
        file.syntax()
            .text_range()
            // Ensure the text range is in the bounds of the source code.
            .contains_range(range)
            .then(|| {
                // Returns deepest ink! attribute that fully covers the text range.
                let covering_element = file.syntax().covering_element(range);
                let attr = if ast::Attr::can_cast(covering_element.kind()) {
                    // Casts cover element to `ast::Attr` node if it's an attribute.
                    covering_element.into_node().and_then(ast::Attr::cast)
                } else {
                    // Finds the parent attribute (if any) of the covering element.
                    ink_analyzer_ir::closest_ancestor_ast_type::<SyntaxElement, ast::Attr>(
                        &covering_element,
                    )
                };
                // Converts to ink! attribute (if present).
                attr.and_then(InkAttribute::cast)
            })
            .flatten()
    };

    // Returns hover content only if the text range is covered by an ink! attribute.
    covering_ink_attr.and_then(|ink_attr| {
        // Finds the covered ink! attribute argument (if any).
        let ink_arg = ink_attr
            .args()
            .iter()
            .find(|arg| arg.text_range().contains_range(range));
        match ink_arg {
            // Returns hover content for the covered ink! attribute argument if it's valid.
            Some(ink_arg) => (*ink_arg.kind() != InkArgKind::Unknown).then_some(Hover {
                range: ink_arg
                    .name()
                    .map(|ink_arg_name| ink_arg_name.syntax().text_range())
                    .unwrap_or(ink_arg.text_range()),
                content: content::doc(&InkAttributeKind::Arg(*ink_arg.kind())).to_string(),
            }),
            // Returns hover content based on the macro or "primary" argument for the ink! attribute,
            // See `ink_analyzer_ir::attrs::utils::sort_ink_args_by_kind` doc.
            None => match ink_attr.kind() {
                // Returns hover content based on the "primary" ink! attribute argument if it's valid,
                // See `ink_analyzer_ir::attrs::utils::sort_ink_args_by_kind` doc.
                InkAttributeKind::Arg(arg_kind) => {
                    (*arg_kind != InkArgKind::Unknown).then_some(Hover {
                        range: ink_attr
                            .ink_arg_name()
                            .map(|ink_arg_name| ink_arg_name.syntax().text_range())
                            .unwrap_or(ink_attr.syntax().text_range()),
                        content: content::doc(&InkAttributeKind::Arg(*arg_kind)).to_string(),
                    })
                }
                // Returns hover content based on the ink! attribute macro if it's valid.
                InkAttributeKind::Macro(macro_kind) => (*macro_kind != InkMacroKind::Unknown)
                    .then_some(Hover {
                        range: ink_attr
                            .ink_macro()
                            .map(|ink_macro_name| ink_macro_name.syntax().text_range())
                            .unwrap_or(ink_attr.syntax().text_range()),
                        content: content::doc(&InkAttributeKind::Macro(*macro_kind)).to_string(),
                    }),
            },
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::syntax::TextSize;
    use test_utils::parse_offset_at;

    #[test]
    fn hover_works() {
        for (code, test_cases) in [
            // (code, pat, [(edit, pat_start, pat_end)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the text that will inserted (represented without whitespace for simplicity),
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // ink! attribute macros.
            (
                "#[ink::contract]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-contract"),
                        Some("contract"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                ],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                "#,
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-contract"),
                        Some("contract"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-env="),
                        Some("(env"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Env)),
                            Some("<-env="),
                            Some("(env"),
                        )),
                    ),
                    (
                        Some("<-my::env::Types"),
                        Some("my::env::Types"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Env)),
                            Some("<-env="),
                            Some("(env"),
                        )),
                    ),
                    (
                        Some("<-,"),
                        Some(","),
                        Some((
                            content::doc(&InkAttributeKind::Macro(InkMacroKind::Contract)),
                            Some("<-contract"),
                            Some("contract"),
                        )),
                    ),
                    (
                        Some("<-keep_attr"),
                        Some("keep_attr"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::KeepAttr)),
                            Some("<-keep_attr"),
                            Some("keep_attr"),
                        )),
                    ),
                    (
                        Some(r#"<-"foo,bar""#),
                        Some(r#""foo,bar""#),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::KeepAttr)),
                            Some("<-keep_attr"),
                            Some("keep_attr"),
                        )),
                    ),
                ],
            ),
            // ink! attribute arguments.
            (
                "#[ink(storage)]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                    (
                        Some("<-storage"),
                        Some("storage"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Storage)),
                            Some("<-storage"),
                            Some("storage"),
                        )),
                    ),
                ],
            ),
            (
                "#[ink(message, default, payable, selector=_)]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-message"),
                        Some("message"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Message)),
                            Some("<-message"),
                            Some("message"),
                        )),
                    ),
                    (
                        Some("<-payable"),
                        Some("payable"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Payable)),
                            Some("<-payable"),
                            Some("payable"),
                        )),
                    ),
                    (
                        Some("<-selector"),
                        Some("selector"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Selector)),
                            Some("<-selector"),
                            Some("selector"),
                        )),
                    ),
                    (
                        Some("<-_"),
                        Some("_"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Selector)),
                            Some("<-selector"),
                            Some("selector"),
                        )),
                    ),
                ],
            ),
            (
                "#[ink(extension=1, handle_status=true)]",
                vec![
                    (
                        Some("<-#"),
                        Some("<-#"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("ink"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-extension"),
                        Some("extension"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-#"),
                        Some("]"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-1"),
                        Some("1"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::Extension)),
                            Some("<-extension"),
                            Some("extension"),
                        )),
                    ),
                    (
                        Some("<-handle_status"),
                        Some("handle_status"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::HandleStatus)),
                            Some("<-handle_status"),
                            Some("handle_status"),
                        )),
                    ),
                    (
                        Some("<-true"),
                        Some("true"),
                        Some((
                            content::doc(&InkAttributeKind::Arg(InkArgKind::HandleStatus)),
                            Some("<-handle_status"),
                            Some("handle_status"),
                        )),
                    ),
                ],
            ),
        ] {
            for (pat_start, pat_end, expect_result) in test_cases {
                let range = TextRange::new(
                    TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                    TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32),
                );

                let result = hover(&InkFile::parse(code), range);

                assert_eq!(
                    result
                        .as_ref()
                        .map(|hover_result| (hover_result.content.as_str(), hover_result.range)),
                    expect_result.map(|(content, pat_start, pat_end)| (
                        content,
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    ))
                );
            }
        }
    }
}
