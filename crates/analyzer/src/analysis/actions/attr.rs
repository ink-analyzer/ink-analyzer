//! ink! attribute code/intent actions.

use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::InkFile;

use super::Action;
use crate::analysis::utils;
use crate::{ActionKind, TextEdit};

/// Computes ink! attribute-based actions at the given text range.
pub fn actions(results: &mut Vec<Action>, file: &InkFile, range: TextRange) {
    // Only computes actions if the focused range is part of/covered by an ink! attribute.
    if let Some(ink_attr) = utils::covering_ink_attribute(file, range) {
        // Only computes actions for closed attributes because
        // unclosed attributes are too tricky for useful contextual edits.
        if ink_attr.ast().r_brack_token().is_some() {
            // No ink! attribute argument suggestions for trait definition implementation messages.
            if ink_attr
                .syntax()
                .parent()
                .as_ref()
                .is_some_and(utils::is_trait_definition_impl_message)
            {
                return;
            }

            // Suggests ink! attribute arguments based on the context.
            let mut ink_arg_suggestions = utils::valid_sibling_ink_args(*ink_attr.kind());

            // Filters out duplicates, conflicting and invalidly scoped ink! arguments.
            utils::remove_duplicate_conflicting_and_invalid_scope_ink_arg_suggestions(
                &mut ink_arg_suggestions,
                &ink_attr,
            );

            // Adds ink! attribute argument actions to accumulator.
            for arg_kind in ink_arg_suggestions {
                // Determines the insertion offset and affixes for the action.
                if let Some((insert_offset, insert_prefix, insert_suffix)) =
                    utils::ink_arg_insert_offset_and_affixes(&ink_attr, Some(arg_kind))
                {
                    // Adds ink! attribute argument action to accumulator.
                    let (edit, snippet) =
                        utils::ink_arg_insert_text(arg_kind, Some(insert_offset), Some(&ink_attr));
                    results.push(Action {
                        label: format!("Add ink! {arg_kind} attribute argument."),
                        kind: ActionKind::Refactor,
                        range: ink_attr.syntax().text_range(),
                        edits: vec![TextEdit::insert_with_snippet(
                            format!(
                                "{}{edit}{}",
                                insert_prefix.unwrap_or_default(),
                                insert_suffix.unwrap_or_default()
                            ),
                            insert_offset,
                            snippet.map(|snippet| {
                                format!(
                                    "{}{snippet}{}",
                                    insert_prefix.unwrap_or_default(),
                                    insert_suffix.unwrap_or_default()
                                )
                            }),
                        )],
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::syntax::TextSize;
    use test_utils::{parse_offset_at, remove_whitespace};

    #[test]
    fn actions_works() {
        for (code, pat, expected_results) in [
            // (code, pat, [(edit, pat_start, pat_end)]) where:
            // code = source code,
            // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
            // edit = the text that will inserted (represented without whitespace for simplicity),
            // pat_start = substring used to find the start of the edit offset (see `test_utils::parse_offset_at` doc),
            // pat_end = substring used to find the end of the edit offset (see `test_utils::parse_offset_at` doc).

            // No ink! attribute in focus.
            ("", None, vec![]),
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
                    ("(env=crate::)", Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
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
                    ("(env=crate::)", Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
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
                    ("(env=crate::)", Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![(r#", keep_attr="""#, Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::contract(env=my::env::Types,)]
                    mod my_contract {
                    }
                "#,
                Some("<-#["),
                vec![(r#"keep_attr="""#, Some("<-)]"), Some("<-)]"))],
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
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
                    (r#"(namespace="my_namespace")"#, Some("<-]"), Some("<-]")),
                ],
            ),
            (
                r#"
                    #[ink::trait_definition(namespace="my_namespace")]
                    pub trait MyTrait {
                    }
                "#,
                Some("<-#["),
                vec![(r#", keep_attr="""#, Some("<-)]"), Some("<-)]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    enum MyEnum {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=true)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    struct MyStruct {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=true)", Some("<-]"), Some("<-]"))],
            ),
            (
                r#"
                    #[ink::storage_item]
                    union MyUnion {
                    }
                "#,
                Some("<-#["),
                vec![("(derive=true)", Some("<-]"), Some("<-]"))],
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
            (
                r#"
                    #[ink_e2e::test]
                    fn it_works() {
                    }
                "#,
                Some("<-#["),
                vec![
                    (r#"(additional_contracts="")"#, Some("<-]"), Some("<-]")),
                    (r#"(environment=crate::)"#, Some("<-]"), Some("<-]")),
                    (r#"(keep_attr="")"#, Some("<-]"), Some("<-]")),
                ],
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(constructor)]
                    #[ink(payable)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#[->"),
                vec![
                    (", default", Some("<-)]->"), Some("<-)]->")),
                    (", selector=1", Some("<-)]->"), Some("<-)]->")),
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
                    (", selector=1", Some("<-)]"), Some("<-)]")),
                ],
            ),
            (
                r#"
                    #[ink(extension=1)]
                    pub fn my_fn() {
                    }
                "#,
                Some("<-#["),
                vec![(", handle_status=true", Some("<-)]"), Some("<-)]"))],
            ),
            // Unique ids.
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(constructor, selector=1)]
                            pub fn constructor_1(&self) {}

                            #[ink(constructor)]
                            pub fn constructor_2(&self) {}
                        }
                    }
                "#,
                Some("<-#[ink(constructor)]"),
                vec![
                    (
                        ", default",
                        Some("#[ink(constructor->"),
                        Some("#[ink(constructor->"),
                    ),
                    (
                        ", payable",
                        Some("#[ink(constructor->"),
                        Some("#[ink(constructor->"),
                    ),
                    (
                        ", selector=2",
                        Some("#[ink(constructor->"),
                        Some("#[ink(constructor->"),
                    ),
                ],
            ),
            (
                r#"
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(message, selector=1)]
                            pub fn message_1(&self) {}

                            #[ink(message)]
                            pub fn message_2(&self) {}
                        }
                    }
                "#,
                Some("<-#[ink(message)]"),
                vec![
                    (
                        ", default",
                        Some("#[ink(message->"),
                        Some("#[ink(message->"),
                    ),
                    (
                        ", payable",
                        Some("#[ink(message->"),
                        Some("#[ink(message->"),
                    ),
                    (
                        ", selector=2",
                        Some("#[ink(message->"),
                        Some("#[ink(message->"),
                    ),
                ],
            ),
            (
                r#"
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, selector=1)]
                        fn message_1(&self);

                        #[ink(message)]
                        fn message_2(&self);
                    }
                "#,
                Some("<-#[ink(message)]"),
                vec![
                    (
                        ", default",
                        Some("#[ink(message->"),
                        Some("#[ink(message->"),
                    ),
                    (
                        ", payable",
                        Some("#[ink(message->"),
                        Some("#[ink(message->"),
                    ),
                    (
                        ", selector=2",
                        Some("#[ink(message->"),
                        Some("#[ink(message->"),
                    ),
                ],
            ),
            (
                r#"
                    #[ink::chain_extension]
                    pub trait MyChainExtension {
                        #[ink(extension=1)]
                        fn extension_1(&self);

                        #[ink(handle_status=true)]
                        fn extension_2(&self);
                    }
                "#,
                Some("<-#[ink(handle_status=true)]"),
                vec![("extension=2,", Some("#[ink(->"), Some("#[ink(->"))],
            ),
        ] {
            let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
            let range = TextRange::new(offset, offset);

            let mut results = Vec::new();
            actions(&mut results, &InkFile::parse(code), range);

            assert_eq!(
                results
                    .into_iter()
                    .map(|action| (
                        remove_whitespace(action.edits[0].text.clone()),
                        action.edits[0].range
                    ))
                    .collect::<Vec<(String, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(edit, pat_start, pat_end)| (
                        remove_whitespace(edit.to_owned()),
                        TextRange::new(
                            TextSize::from(parse_offset_at(code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(code, pat_end).unwrap() as u32)
                        )
                    ))
                    .collect::<Vec<(String, TextRange)>>(),
                "code: {code}"
            );
        }
    }
}
