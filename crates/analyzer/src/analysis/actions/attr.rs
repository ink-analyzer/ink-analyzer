//! ink! attribute code/intent actions.

use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::{InkArgKind, InkAttributeKind, InkFile, InkMacroKind};

use super::Action;
use crate::analysis::utils;
use crate::{ActionKind, TextEdit, Version};

/// Computes ink! attribute-based actions at the given text range.
pub fn actions(results: &mut Vec<Action>, file: &InkFile, range: TextRange, version: Version) {
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

            if version == Version::Legacy
                && matches!(
                    ink_attr.kind(),
                    InkAttributeKind::Macro(
                        InkMacroKind::Contract
                            | InkMacroKind::TraitDefinition
                            | InkMacroKind::ChainExtension
                            | InkMacroKind::StorageItem
                    )
                )
            {
                // Adds ink! 4.x project to ink! 5.0 action.
                results.push(Action {
                    label: "Migrate to ink! 5.0".to_owned(),
                    kind: ActionKind::Migrate,
                    range: ink_attr.syntax().text_range(),
                    edits: Vec::new(),
                });
            }

            if version.is_gte_v5()
                && matches!(
                    ink_attr.kind(),
                    InkAttributeKind::Macro(InkMacroKind::Event)
                        | InkAttributeKind::Arg(InkArgKind::Event)
                )
            {
                // Extracts ink! event into a standalone package.
                results.push(Action {
                    label: "Extract ink! event into a standalone package".to_owned(),
                    kind: ActionKind::Extract,
                    range: ink_attr.syntax().text_range(),
                    edits: Vec::new(),
                });
            }

            // Suggests ink! attribute arguments based on the context.
            let mut ink_arg_suggestions = utils::valid_sibling_ink_args(*ink_attr.kind(), version);

            // Filters out duplicates, conflicting and invalidly scoped ink! arguments.
            utils::remove_duplicate_conflicting_and_invalid_scope_ink_arg_suggestions(
                &mut ink_arg_suggestions,
                &ink_attr,
                version,
            );

            // Adds ink! attribute argument actions to accumulator.
            for arg_kind in ink_arg_suggestions {
                // Determines the insertion offset and affixes for the action.
                if let Some((insert_offset, insert_prefix, insert_suffix)) =
                    utils::ink_arg_insert_offset_and_affixes(&ink_attr, Some(arg_kind))
                {
                    // Adds ink! attribute argument action to accumulator.
                    let (edit, snippet) = utils::ink_arg_insert_text(
                        arg_kind,
                        version,
                        Some(insert_offset),
                        Some(&ink_attr),
                    );
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
    use crate::test_utils::verify_actions;
    use ink_analyzer_ir::{syntax::TextSize, MinorVersion};
    use test_utils::{parse_offset_at, TestResultAction, TestResultTextRange};

    macro_rules! prepend_migrate {
        ($version: expr, $list: expr) => {
            if $version == Version::Legacy {
                vec![TestResultAction {
                    label: "Migrate",
                    edits: vec![],
                }]
            } else {
                vec![]
            }
            .into_iter()
            .chain($list)
            .collect::<Vec<TestResultAction>>()
        };
        ($list: expr) => {
            prepend_migrate!(Version::Legacy, $list)
        };
        () => {
            vec![TestResultAction {
                label: "Migrate",
                edits: vec![],
            }]
        };
    }

    #[test]
    fn actions_works() {
        for (version, fixtures) in [
            (
                Version::Legacy,
                vec![
                    (
                        r#"
                        #[ink(event)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("<-#["),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", anonymous",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"
                        #[ink(event)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("ink("),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", anonymous",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"
                        #[ink(event)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("event)]"),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", anonymous",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"
                        #[ink(event,)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("<-#["),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "anonymous",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"
                        #[ink::chain_extension]
                        pub trait MyTrait {
                        }
                        "#,
                        Some("<-#["),
                        prepend_migrate!(),
                    ),
                    (
                        r#"
                        #[ink(extension=1)]
                        pub fn my_fn() {
                        }
                        "#,
                        Some("<-#["),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", handle_status = true",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"
                        #[ink_e2e::test]
                        fn it_works() {
                        }
                        "#,
                        Some("<-#["),
                        vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(additional_contracts = "")"#,
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(environment = ink::env::DefaultEnvironment)",
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
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
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "extension = 2,",
                                start_pat: Some("#[ink(->"),
                                end_pat: Some("#[ink(->"),
                            }],
                        }],
                    ),
                ],
            ),
            (
                Version::V5(MinorVersion::Base),
                vec![
                    (
                        r#"
                        #[ink::event]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("<-#["),
                        vec![
                            TestResultAction {
                                label: "Extract",
                                edits: vec![],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(anonymous)",
                                    start_pat: Some("#[ink::event"),
                                    end_pat: Some("#[ink::event"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(signature_topic = "")"#,
                                    start_pat: Some("#[ink::event"),
                                    end_pat: Some("#[ink::event"),
                                }],
                            },
                        ],
                    ),
                    (
                        r#"
                        #[ink(event)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("<-#["),
                        vec![
                            TestResultAction {
                                label: "Extract",
                                edits: vec![],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: ", anonymous",
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#", signature_topic = """#,
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                        ],
                    ),
                    (
                        r#"
                        #[ink(event)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("ink("),
                        vec![
                            TestResultAction {
                                label: "Extract",
                                edits: vec![],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: ", anonymous",
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#", signature_topic = """#,
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                        ],
                    ),
                    (
                        r#"
                        #[ink(event)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("event)]"),
                        vec![
                            TestResultAction {
                                label: "Extract",
                                edits: vec![],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: ", anonymous",
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#", signature_topic = """#,
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                        ],
                    ),
                    (
                        r#"
                        #[ink(event,)]
                        pub struct MyStruct {
                        }
                        "#,
                        Some("<-#["),
                        vec![
                            TestResultAction {
                                label: "Extract",
                                edits: vec![],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "anonymous",
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"signature_topic = """#,
                                    start_pat: Some("<-)]"),
                                    end_pat: Some("<-)]"),
                                }],
                            },
                        ],
                    ),
                    (
                        r#"
                        #[ink::chain_extension]
                        pub trait MyTrait {
                        }
                        "#,
                        Some("<-#["),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "(extension = 1)",
                                start_pat: Some("#[ink::chain_extension"),
                                end_pat: Some("#[ink::chain_extension"),
                            }],
                        }],
                    ),
                    (
                        r#"
                        #[ink(function=1)]
                        pub fn my_fn() {
                        }
                        "#,
                        Some("<-#["),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", handle_status = true",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }],
                    ),
                    (
                        r#"
                        #[ink_e2e::test]
                        fn it_works() {
                        }
                        "#,
                        Some("<-#["),
                        vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(backend(node))",
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(environment = ink::env::DefaultEnvironment)",
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                        ],
                    ),
                    (
                        r#"
                        #[ink::chain_extension]
                        pub trait MyChainExtension {
                            #[ink(function=1)]
                            fn function_1(&self);

                            #[ink(handle_status=true)]
                            fn function_2(&self);
                        }
                        "#,
                        Some("<-#[ink(handle_status=true)]"),
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "function = 2,",
                                start_pat: Some("#[ink(->"),
                                end_pat: Some("#[ink(->"),
                            }],
                        }],
                    ),
                ],
            ),
        ] {
            for (code, pat, expected_results) in [
                // (code, pat, [(edit, pat_start, pat_end)]) where:
                // code = source code,
                // pat = substring used to find the cursor offset (see `test_utils::parse_offset_at` doc),
                // edit = the text that will be inserted (represented without whitespace for simplicity),
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
                    prepend_migrate!(
                        version,
                        [
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(env = ink::env::DefaultEnvironment)",
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                        ]
                    ),
                ),
                (
                    r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                    "#,
                    Some("ink::"),
                    prepend_migrate!(
                        version,
                        [
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(env = ink::env::DefaultEnvironment)",
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                        ]
                    ),
                ),
                (
                    r#"
                    #[ink::contract]
                    mod my_contract {
                    }
                    "#,
                    Some("contract]"),
                    prepend_migrate!(
                        version,
                        [
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(env = ink::env::DefaultEnvironment)",
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                        ]
                    ),
                ),
                (
                    r#"
                    #[ink::contract(env=my::env::Types)]
                    mod my_contract {
                    }
                    "#,
                    Some("<-#["),
                    prepend_migrate!(
                        version,
                        [TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#", keep_attr = """#,
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }]
                    ),
                ),
                (
                    r#"
                    #[ink::contract(env=my::env::Types,)]
                    mod my_contract {
                    }
                    "#,
                    Some("<-#["),
                    prepend_migrate!(
                        version,
                        [TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#"keep_attr = """#,
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }]
                    ),
                ),
                (
                    r#"
                    #[ink::trait_definition]
                    pub trait MyTrait {
                    }
                    "#,
                    Some("<-#["),
                    prepend_migrate!(
                        version,
                        [
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(namespace = "my_namespace")"#,
                                    start_pat: Some("<-]"),
                                    end_pat: Some("<-]"),
                                }],
                            },
                        ]
                    ),
                ),
                (
                    r#"
                    #[ink::trait_definition(namespace="my_namespace")]
                    pub trait MyTrait {
                    }
                    "#,
                    Some("<-#["),
                    prepend_migrate!(
                        version,
                        [TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#", keep_attr = """#,
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        }]
                    ),
                ),
                (
                    r#"
                    #[ink::storage_item]
                    enum MyEnum {
                    }
                    "#,
                    Some("<-#["),
                    prepend_migrate!(
                        version,
                        [TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "(derive = true)",
                                start_pat: Some("<-]"),
                                end_pat: Some("<-]"),
                            }],
                        }]
                    ),
                ),
                (
                    r#"
                    #[ink::storage_item]
                    struct MyStruct {
                    }
                    "#,
                    Some("<-#["),
                    prepend_migrate!(
                        version,
                        [TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "(derive = true)",
                                start_pat: Some("<-]"),
                                end_pat: Some("<-]"),
                            }],
                        }]
                    ),
                ),
                (
                    r#"
                    #[ink::storage_item]
                    union MyUnion {
                    }
                    "#,
                    Some("<-#["),
                    prepend_migrate!(
                        version,
                        [TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "(derive = true)",
                                start_pat: Some("<-]"),
                                end_pat: Some("<-]"),
                            }],
                        }]
                    ),
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", payable",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 1",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 1",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 1",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("<-)]->"),
                                end_pat: Some("<-)]->"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 1",
                                start_pat: Some("<-)]->"),
                                end_pat: Some("<-)]->"),
                            }],
                        },
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", payable",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 1",
                                start_pat: Some("<-)]"),
                                end_pat: Some("<-)]"),
                            }],
                        },
                    ],
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("#[ink(constructor->"),
                                end_pat: Some("#[ink(constructor->"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", payable",
                                start_pat: Some("#[ink(constructor->"),
                                end_pat: Some("#[ink(constructor->"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 2",
                                start_pat: Some("#[ink(constructor->"),
                                end_pat: Some("#[ink(constructor->"),
                            }],
                        },
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("#[ink(message->"),
                                end_pat: Some("#[ink(message->"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", payable",
                                start_pat: Some("#[ink(message->"),
                                end_pat: Some("#[ink(message->"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 2",
                                start_pat: Some("#[ink(message->"),
                                end_pat: Some("#[ink(message->"),
                            }],
                        },
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
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", default",
                                start_pat: Some("#[ink(message->"),
                                end_pat: Some("#[ink(message->"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", payable",
                                start_pat: Some("#[ink(message->"),
                                end_pat: Some("#[ink(message->"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", selector = 2",
                                start_pat: Some("#[ink(message->"),
                                end_pat: Some("#[ink(message->"),
                            }],
                        },
                    ],
                ),
            ]
            .into_iter()
            .chain(fixtures)
            {
                let offset = TextSize::from(parse_offset_at(code, pat).unwrap() as u32);
                let range = TextRange::new(offset, offset);

                let mut results = Vec::new();
                actions(&mut results, &InkFile::parse(code), range, version);

                // Verifies actions.
                verify_actions(code, &results, &expected_results);
            }
        }
    }
}
