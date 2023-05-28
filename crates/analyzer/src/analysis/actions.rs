//! ink! attribute code/intent actions.

use ink_analyzer_ir::ast::HasAttrs;
use ink_analyzer_ir::syntax::{AstNode, SyntaxKind, SyntaxNode, TextRange, TextSize};
use ink_analyzer_ir::{ast, FromAST, FromSyntax, InkAttributeKind, InkEntity, InkFile};

use super::utils;

/// An ink! attribute code/intent action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Action {
    /// Label which identifies the action.
    pub label: String,
    /// Range where the action will be applied.
    pub range: TextRange,
    /// Replacement text for the action.
    pub edit: String,
}

/// Computes ink! attribute actions for the given offset.
pub fn actions(file: &InkFile, offset: TextSize) -> Vec<Action> {
    let mut results = Vec::new();

    // Compute AST item-based ink! attribute actions.
    ast_item_actions(&mut results, file, offset);

    // Compute ink! attribute actions based on focused ink! attribute.
    ink_attribute_actions(&mut results, file, offset);

    results
}

/// Computes AST item-based ink! attribute actions at the given offset.
pub fn ast_item_actions(results: &mut Vec<Action>, file: &InkFile, offset: TextSize) {
    let item_at_offset = file.item_at_offset(offset);

    // Only computes actions if a focused token can be determined.
    if let Some(focused_token) = item_at_offset.focused_token() {
        // Only computes actions if the focused token isn't part of an attribute.
        if item_at_offset.parent_attr().is_none() {
            // Only computes actions if the parent AST item can be determined.
            if let Some(ast_item) = item_at_offset.parent_ast_item() {
                // Gets the covering struct record field if the AST item is a struct.
                let record_field: Option<ast::RecordField> = match &ast_item {
                    ast::Item::Struct(_) => {
                        ink_analyzer_ir::closest_ancestor_ast_type(focused_token)
                    }
                    _ => None,
                };

                // Only computes actions if the focus is on either a struct record field or
                // an AST item's declaration (i.e not inside the AST item's item list or body) for
                // an item that can be annotated with ink! attributes.
                if record_field.is_some() || is_focused_on_ast_item_declaration(&ast_item, offset) {
                    // Set the target node as either the covering struct field (if present) or
                    // the parent AST item (for all other cases).
                    let target = record_field
                        .as_ref()
                        .map(|field| field.syntax())
                        .unwrap_or(ast_item.syntax());

                    // Gets the last attribute for the target node (if any).
                    let last_attr = record_field
                        .as_ref()
                        .map(|field| field.attrs())
                        .unwrap_or(ast_item.attrs())
                        .last();

                    // Determines the insertion point for the action, if the target node has attributes,
                    // then the new ink! attribute is inserted at the end of that list otherwise,
                    // it's inserted at the beginning of the target node.
                    let (insert_near, insert_offset) = last_attr
                        .as_ref()
                        .map(|attr| (attr.syntax(), attr.syntax().text_range().end()))
                        .unwrap_or((target, target.text_range().start()));

                    // Computes the text range for the edit.
                    let edit_range = TextRange::new(insert_offset, insert_offset);

                    // Get the indenting whitespace (if any) for the node we're inserting next to so that we preserve formatting after the insert.
                    let insert_indenting = insert_near
                        .prev_sibling_or_token()
                        .and_then(|prev_elem| {
                            (prev_elem.kind() == SyntaxKind::WHITESPACE)
                                .then_some(prev_elem.to_string())
                        })
                        .unwrap_or(String::new());

                    // Convenience closure for adding ink! attribute actions.
                    let mut add_action_to_accumulator =
                        |attr: &str, attr_kind: &str, label_kind: &str| {
                            results.push(Action {
                                label: format!("Add ink! {attr_kind} attribute {label_kind}."),
                                range: edit_range,
                                edit: if insert_offset > insert_near.text_range().start() {
                                    format!("{insert_indenting}{attr}")
                                } else {
                                    format!("{attr}{insert_indenting}")
                                },
                            });
                        };

                    // Suggests ink! attribute macros based on the context.
                    let mut ink_macro_suggestions =
                        utils::valid_ink_macros_by_syntax_kind(target.kind());

                    // Filters out duplicate and invalid ink! attribute macro actions based on parent ink! scope (if any).
                    utils::remove_duplicate_ink_macro_suggestions(
                        &mut ink_macro_suggestions,
                        target,
                    );
                    utils::remove_invalid_ink_macro_suggestions_for_parent_ink_scope(
                        &mut ink_macro_suggestions,
                        target,
                    );

                    // Add ink! attribute macro actions to accumulator.
                    ink_macro_suggestions.iter().for_each(|macro_kind| {
                        add_action_to_accumulator(
                            &format!("#[ink::{macro_kind}]"),
                            &macro_kind.to_string(),
                            "macro",
                        );
                    });

                    // Suggests ink! attribute arguments based on the context.
                    let mut ink_arg_suggestions =
                        utils::valid_ink_ink_args_by_syntax_kind(target.kind());

                    // Filters out duplicate and invalid (based on parent ink! scope) ink! attribute argument actions.
                    utils::remove_duplicate_ink_arg_suggestions(&mut ink_arg_suggestions, target);
                    utils::remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
                        &mut ink_arg_suggestions,
                        target,
                    );

                    // Add ink! attribute argument actions to accumulator.
                    ink_arg_suggestions.iter().for_each(|arg_kind| {
                        add_action_to_accumulator(
                            &format!(
                                "#[ink({})]",
                                utils::ink_arg_insertion_text(
                                    arg_kind,
                                    edit_range.end(),
                                    ast_item.syntax(),
                                )
                            ),
                            &arg_kind.to_string(),
                            "argument",
                        );
                    });
                }
            }
        }
    }
}

/// Computes AST item-based ink! attribute actions at the given offset.
pub fn ink_attribute_actions(results: &mut Vec<Action>, file: &InkFile, offset: TextSize) {
    let item_at_offset = file.item_at_offset(offset);

    // Only computes actions if the focused token is part of an ink! attribute.
    if let Some(ink_attr) = item_at_offset.parent_ink_attr() {
        // Only computes actions for closed attributes because
        // unclosed attributes are too tricky for useful contextual edits.
        if let Some(r_bracket) = ink_attr.ast().r_brack_token() {
            // Suggests ink! attribute arguments based on the context.
            let mut ink_arg_suggestions = utils::valid_sibling_ink_args(ink_attr.kind());

            if let Some(attr_parent) = ink_attr.syntax().parent() {
                // Filters out duplicate ink! attribute argument actions.
                utils::remove_duplicate_ink_arg_suggestions(&mut ink_arg_suggestions, &attr_parent);

                // Filters out invalid (based on parent ink! scope) ink! attribute argument actions,
                // Doesn't apply to ink! attribute macros as their arguments are not influenced by the parent scope.
                if let InkAttributeKind::Arg(_) = ink_attr.kind() {
                    utils::remove_invalid_ink_arg_suggestions_for_parent_ink_scope(
                        &mut ink_arg_suggestions,
                        &attr_parent,
                    );
                }
            }

            // Determines the insertion point for the action as well as the affixes that should
            // surround the ink! attribute text (i.e whitespace and delimiters e.g `(`, `,` and `)`).
            let attr = ink_attr.ast();
            let (insert_offset, insert_prefix, insert_suffix) = attr
                .token_tree()
                .as_ref()
                .map(|token_tree| {
                    (
                        // Computes the insertion offset.
                        token_tree
                            .r_paren_token()
                            // Inserts just before right parenthesis if it's exists.
                            .map(|r_paren| r_paren.text_range().start())
                            // Inserts at the end of the token tree if no right parenthesis exists.
                            .unwrap_or(token_tree.syntax().text_range().end()),
                        // Determines the prefix to insert before the ink! attribute text.
                        match token_tree.l_paren_token() {
                            Some(_) => token_tree
                                .r_paren_token()
                                .and_then(|r_paren| {
                                    r_paren.prev_token().map(|penultimate_token| {
                                        match penultimate_token.kind() {
                                            SyntaxKind::COMMA | SyntaxKind::L_PAREN => "",
                                            // Adds a comma if the token before the right parenthesis is
                                            // neither a comma nor the left parenthesis.
                                            _ => ", ",
                                        }
                                    })
                                })
                                .unwrap_or(
                                    match ink_analyzer_ir::last_child_token(token_tree.syntax()) {
                                        Some(last_token) => match last_token.kind() {
                                            SyntaxKind::COMMA
                                            | SyntaxKind::L_PAREN
                                            | SyntaxKind::R_PAREN => "",
                                            // Adds a comma if there is no right parenthesis and the last token is
                                            // neither a comma nor the left parenthesis
                                            // (the right parenthesis in the pattern above will likely never match anything,
                                            // but parsers is weird :-) so we leave it for robustness? and clarity).
                                            _ => ", ",
                                        },
                                        None => "",
                                    },
                                ),
                            // Adds a left parenthesis if non already exists.
                            None => "(",
                        },
                        // Determines the suffix to insert before the ink! attribute text.
                        match token_tree.r_paren_token() {
                            Some(_) => "",
                            // Adds a right parenthesis if non already exists.
                            None => ")",
                        },
                    )
                })
                .unwrap_or((r_bracket.text_range().start(), "(", ")"));

            // Computes the text range for the edit.
            let edit_range = TextRange::new(insert_offset, insert_offset);

            // Add ink! attribute argument actions to accumulator.
            ink_arg_suggestions.iter().for_each(|arg_kind| {
                results.push(Action {
                    label: format!("Add ink! {arg_kind} attribute argument."),
                    range: edit_range,
                    edit: format!(
                        "{insert_prefix}{}{insert_suffix}",
                        utils::ink_arg_insertion_text(
                            arg_kind,
                            edit_range.end(),
                            ink_attr.syntax(),
                        )
                    ),
                });
            });
        }
    }
}

/// Determines if the offset is focused on an AST item's declaration
/// (i.e not inside the AST item's item list or body) for an item that can be annotated with ink! attributes.
fn is_focused_on_ast_item_declaration(item: &ast::Item, offset: TextSize) -> bool {
    // Shared logic that ensures the offset in not inside the AST item's item list or body.
    let is_focused_on_declaration_impl = |item_list: Option<&SyntaxNode>| {
        item_list
            .map(|node| {
                let opening_curly_end =
                    ink_analyzer_ir::first_child_token(node).and_then(|token| {
                        (token.kind() == SyntaxKind::L_CURLY).then_some(token.text_range().end())
                    });
                let closing_curly_start =
                    ink_analyzer_ir::last_child_token(node).and_then(|token| {
                        (token.kind() == SyntaxKind::R_CURLY).then_some(token.text_range().start())
                    });
                offset <= opening_curly_end.unwrap_or(node.text_range().start())
                    || closing_curly_start.unwrap_or(node.text_range().end()) <= offset
            })
            .unwrap_or(true)
    };

    // Gets the last attribute for the AST item (if any).
    let last_attr = item.attrs().last();

    // Ensures offset is either after the last attribute (if any) or after the beginning of the AST item.
    last_attr
        .map(|attr| attr.syntax().text_range().end())
        .unwrap_or(item.syntax().text_range().start())
        <= offset
        // Ensures offset is before the end of the AST item.
        && offset <= item.syntax().text_range().end()
        // Ensures the offset in not inside the AST item's item list or body.
        && match item {
            // We only care about AST items that can be annotated with ink! attributes.
            ast::Item::Module(item) => {
                is_focused_on_declaration_impl(item.item_list().as_ref().map(|node| node.syntax()))
            }
            ast::Item::Trait(item) => is_focused_on_declaration_impl(
                item.assoc_item_list().as_ref().map(|node| node.syntax()),
            ),
            ast::Item::Enum(item) => is_focused_on_declaration_impl(
                item.variant_list().as_ref().map(|node| node.syntax()),
            ),
            ast::Item::Struct(item) => {
                is_focused_on_declaration_impl(item.field_list().as_ref().map(|node| node.syntax()))
            }
            ast::Item::Union(item) => is_focused_on_declaration_impl(
                item.record_field_list().as_ref().map(|node| node.syntax()),
            ),
            ast::Item::Fn(item) => {
                is_focused_on_declaration_impl(item.body().as_ref().map(|node| node.syntax()))
            }
            ast::Item::Impl(item) => is_focused_on_declaration_impl(
                item.assoc_item_list().as_ref().map(|node| node.syntax()),
            ),
            // Everything else is ignored.
            _ => false,
        }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::FromSyntax;
    use test_utils::parse_offset_at;

    #[test]
    fn ast_item_actions_works() {
        for (code, pat, expected_results) in [
            // (code, pat, [(edit, pat_start, pat_end)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the text that will inserted (represented without whitespace for simplicity),
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // No AST item declaration in focus.
            ("// A comment in focus.", None, vec![]),
            (
                r#"
                    mod my_module {
                        // The module declaration is out of focus when this comment is in focus.
                    }
                "#,
                Some("<-//"),
                vec![],
            ),
            // Module focus.
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("<-mod"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("my_con"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("<-{"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("{"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("}"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    mod my_contract {
                    }
                "#,
                Some("<-}"),
                vec![("#[ink::contract]", Some("<-mod"), Some("<-mod"))],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("<-mod"),
                vec![],
            ),
            (
                r#"
                    #[foo]
                    mod my_contract {
                    }
                "#,
                Some("<-mod"),
                vec![("#[ink::contract]", Some("foo]"), Some("foo]"))],
            ),
            // Trait focus.
            (
                r#"
                    pub trait MyTrait {
                    }
                "#,
                Some("<-pub"),
                vec![
                    ("#[ink::chain_extension]", Some("<-pub"), Some("<-pub")),
                    ("#[ink::trait_definition]", Some("<-pub"), Some("<-pub")),
                ],
            ),
            // ADT focus.
            (
                r#"
                    enum MyEnum {
                    }
                "#,
                Some("<-enum"),
                vec![("#[ink::storage_item]", Some("<-enum"), Some("<-enum"))],
            ),
            (
                r#"
                    struct MyStruct {
                    }
                "#,
                Some("<-struct"),
                vec![
                    ("#[ink::storage_item]", Some("<-struct"), Some("<-struct")),
                    ("#[ink(anonymous)]", Some("<-struct"), Some("<-struct")),
                    ("#[ink(event)]", Some("<-struct"), Some("<-struct")),
                    ("#[ink(storage)]", Some("<-struct"), Some("<-struct")),
                ],
            ),
            (
                r#"
                    union MyUnion {
                    }
                "#,
                Some("<-union"),
                vec![("#[ink::storage_item]", Some("<-union"), Some("<-union"))],
            ),
            // Struct field focus.
            (
                r#"
                    struct MyStruct {
                        value: bool,
                    }
                "#,
                Some("<-value"),
                vec![("#[ink(topic)]", Some("<-value"), Some("<-value"))],
            ),
            // Fn focus.
            (
                r#"
                    fn my_fn() {
                    }
                "#,
                Some("<-fn"),
                vec![
                    ("#[ink::test]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(constructor)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(default)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(extension=)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(handle_status=)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(message)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(payable)]", Some("<-fn"), Some("<-fn")),
                    ("#[ink(selector=)]", Some("<-fn"), Some("<-fn")),
                ],
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

            let mut results = Vec::new();
            ast_item_actions(&mut results, &InkFile::parse(code), offset);

            assert_eq!(
                results
                    .iter()
                    .map(|action| (action.edit.trim(), action.range))
                    .collect::<Vec<(&str, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(edit, pat_start, pat_end)| (
                        edit,
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    ))
                    .collect::<Vec<(&str, TextRange)>>(),
                "code: {}",
                code
            );
        }
    }

    #[test]
    fn ink_attribute_actions_works() {
        for (code, pat, expected_results) in [
            // (code, pat, [(edit, pat_start, pat_end)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the text that will inserted (represented without whitespace for simplicity),
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // No ink! attribute in focus.
            ("// A comment in focus.", None, vec![]),
            (
                r#"
                    #[foo]
                    mod my_module {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            (
                r#"
                    #[foo]
                    mod my_module {
                    }
                "#,
                Some("[fo"),
                vec![],
            ),
            (
                r#"
                    #[foo]
                    mod my_module {
                    }
                "#,
                Some("foo]"),
                vec![],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_module {
                    }
                "#,
                Some("<-mod"),
                vec![],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_module {
                    }
                "#,
                Some("my_"),
                vec![],
            ),
            // ink! attribute macros.
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![
                    ("(env=)", Some("<-]"), Some("<-]")),
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("ink::"),
                vec![
                    ("(env=)", Some("<-]"), Some("<-]")),
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                "#,
                Some("contract]"),
                vec![
                    ("(env=)", Some("<-]"), Some("<-]")),
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![(", keep_attr=", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types,)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![("keep_attr=", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::chain_extension]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            (
                r#"
                    #[ink::trait_definition]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![
                    ("(keep_attr=)", Some("<-]"), Some("<-]")),
                    ("(namespace=)", Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::trait_definition(namespace="my_namespace")]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![(", keep_attr=", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    enum MyEnum {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    union MyUnion {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::test]
                    fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            // ink! attribute arguments.
            (
                r#"
                    #[ink(storage)]
                    pub struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![(", anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                    }
                "#,
                Some("ink("),
                vec![(", anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                    }
                "#,
                Some("event)]"),
                vec![(", anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event,)]
                    pub struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![("anonymous", Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                        #[ink(topic)]
                        value: bool,
                    }
                "#,
                Some("#[ink(top"),
                vec![],
            ),
            (
                r#"
                    #[ink(event)]
                    pub struct MyStruct {
                        #[ink(topic)]
                        value: bool,
                    }
                "#,
                Some("<-#[->"),
                vec![],
            ),
            (
                r#"
                    #[ink(constructor)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", payable", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(constructor, payable)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(constructor)]
                    #[ink(payable)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(message)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (", default", Some("<-)]"), Some("<-)]")),
                    (", payable", Some("<-)]"), Some("<-)]")),
                    (", selector=", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(extension=1)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![(", handle_status=", Some("<-)]"), Some("<-)]"))],
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

            let mut results = Vec::new();
            ink_attribute_actions(&mut results, &InkFile::parse(code), offset);

            assert_eq!(
                results
                    .iter()
                    .map(|action| (action.edit.trim(), action.range))
                    .collect::<Vec<(&str, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(edit, pat_start, pat_end)| (
                        edit,
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    ))
                    .collect::<Vec<(&str, TextRange)>>(),
                "code: {}",
                code
            );
        }
    }

    #[test]
    fn is_focused_on_ast_item_declaration_works() {
        for (code, scenarios) in [
            // (code, [(pat, result)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // result = expected result from calling `is_focused_on_ast_item_declaration`,

            // Module.
            (
                r#"
                    #[abc]
                    #[ink::contract]
                    mod my_module {
                        // The module declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::con"), false),
                    (Some("contract]"), true),
                    (Some("<-mod"), true),
                    (Some("mo"), true),
                    (Some("mod"), true),
                    (Some("<-my_module"), true),
                    (Some("my_"), true),
                    (Some("<-my_module"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Trait.
            (
                r#"
                    #[abc]
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        // The trait declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::trait"), false),
                    (Some("definition]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-trait MyTrait"), true),
                    (Some("pub tr"), true),
                    (Some("pub trait"), true),
                    (Some("<-MyTrait"), true),
                    (Some("My"), true),
                    (Some("<-MyTrait"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Enum.
            (
                r#"
                    #[abc]
                    #[ink::storage_item]
                    pub enum MyEnum {
                        // The enum declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::storage"), false),
                    (Some("storage_item]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-enum"), true),
                    (Some("en"), true),
                    (Some("enum"), true),
                    (Some("<-MyEnum"), true),
                    (Some("My"), true),
                    (Some("<-MyEnum"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Struct.
            (
                r#"
                    #[abc]
                    #[ink(event, anonymous)]
                    pub struct MyStruct {
                        // The struct declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink("), false),
                    (Some("(eve"), false),
                    (Some("(event,"), false),
                    (Some(", anon"), false),
                    (Some("anonymous)]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-struct"), true),
                    (Some("st"), true),
                    (Some("struct"), true),
                    (Some("<-MyStruct"), true),
                    (Some("My"), true),
                    (Some("<-MyStruct"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Union.
            (
                r#"
                    #[abc]
                    #[ink::storage_item]
                    pub union MyUnion {
                        // The union declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink::"), false),
                    (Some("::storage"), false),
                    (Some("storage_item]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-union"), true),
                    (Some("un"), true),
                    (Some("union"), true),
                    (Some("<-MyUnion"), true),
                    (Some("My"), true),
                    (Some("<-MyUnion"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
            // Fn.
            (
                r#"
                    #[abc]
                    #[ink(constructor, selector=1)]
                    #[ink(payable)]
                    pub fn my_fn() {
                        // The fn declaration is out of focus when this comment is in focus.
                    }
                "#,
                vec![
                    (Some("<-#[a"), false),
                    (Some("#[ab"), false),
                    (Some("abc]"), false),
                    (Some("<-#[ink"), false),
                    (Some("#[in"), false),
                    (Some("ink("), false),
                    (Some("(con"), false),
                    (Some("(constructor,"), false),
                    (Some(", select"), false),
                    (Some("selector=1)]"), false),
                    (Some("(pay"), false),
                    (Some("payable)]"), true),
                    (Some("<-pub"), true),
                    (Some("pu"), true),
                    (Some("pub"), true),
                    (Some("<-fn"), true),
                    (Some("f"), true),
                    (Some("fn"), true),
                    (Some("<-my_fn"), true),
                    (Some("my_"), true),
                    (Some("<-my_fn"), true),
                    (Some("<-{"), true),
                    (Some("{"), true),
                    (Some("<-//"), false),
                    (Some("<-}"), true),
                    (Some("}"), true),
                ],
            ),
        ] {
            for (pat, expected_result) in scenarios {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);

                let ast_item = InkFile::parse(code)
                    .syntax()
                    .descendants()
                    .filter_map(ast::Item::cast)
                    .next()
                    .unwrap();
                assert_eq!(
                    is_focused_on_ast_item_declaration(&ast_item, offset),
                    expected_result,
                    "code: {}",
                    code
                );
            }
        }
    }
}
