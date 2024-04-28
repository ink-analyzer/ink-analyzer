//! ink! event extraction.

use ink_analyzer_ir::ast::edit_in_place::AttrsOwnerEdit;
use ink_analyzer_ir::ast::{HasAttrs, HasName};
use ink_analyzer_ir::syntax::{AstNode, AstToken, TextRange};
use ink_analyzer_ir::{ast, Event, EventV2, InkArgKind, InkAttribute, InkEntity, InkFile};
use itertools::Itertools;

use crate::analysis::text_edit::format_edit;
use crate::analysis::utils;
use crate::{TextEdit, TextSize};

/// An ink! event extraction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Extraction {
    /// Name of item being extracted.
    pub name: String,
    /// Content of the new extracted item.
    pub content: String,
    /// Text edits that will be performed on the current file.
    pub edits: Vec<TextEdit>,
    /// Insert position for import.
    pub import_offset: TextSize,
    /// Indent for import.
    pub import_indent: String,
}

/// Computes ink! event extraction for the text range.
pub fn extract(file: &InkFile, range: TextRange) -> Option<Extraction> {
    // Finds `struct` item and retrieves its name (if any).
    let struct_item = utils::focused_element(file, range).and_then(|elem| {
        if ast::Struct::can_cast(elem.kind()) {
            elem.into_node().and_then(ast::Struct::cast)
        } else {
            ink_analyzer_ir::parent_ast_item(&elem).and_then(|item| match item {
                ast::Item::Struct(struct_item) => Some(struct_item),
                _ => None,
            })
        }
    });
    let struct_item = struct_item.filter(|struct_item| {
        Event::can_cast(struct_item.syntax()) || EventV2::can_cast(struct_item.syntax())
    })?;
    let name = struct_item.name().as_ref().map(ToString::to_string)?;

    // Finds enclosing module and retrieves the insert offset and indent for the import (if any).
    let module_item =
        ink_analyzer_ir::closest_ancestor_ast_type::<_, ast::Module>(struct_item.syntax())?;
    let import_offset = module_item
        .item_list()
        .map(|item_list| utils::item_insert_offset_start(&item_list))?;
    let import_indent = format!("\n{}", utils::item_children_indenting(module_item.syntax()));

    // Composes content of extracted ink! event.
    // Effectively migrates event to 2.0 if necessary.
    let comments = struct_item
        .syntax()
        .children_with_tokens()
        .filter_map(|elem| elem.into_token().and_then(ast::Comment::cast))
        .join("\n");
    let non_ink_attrs = struct_item
        .attrs()
        .filter(|attr| !InkAttribute::can_cast(attr))
        .join("\n");
    let event_arg = ink_analyzer_ir::ink_arg_by_kind(struct_item.syntax(), InkArgKind::Anonymous)
        .or_else(|| {
            ink_analyzer_ir::ink_arg_by_kind(struct_item.syntax(), InkArgKind::SignatureTopic)
        })
        .map(|arg| format!("({arg})"));
    let event_attr = format!("#[ink::event{}]", event_arg.as_deref().unwrap_or_default());
    let struct_indent = utils::item_indenting(struct_item.syntax());
    let mutable_struct = struct_item.clone_for_update();
    mutable_struct.remove_attrs_and_docs();
    let mut struct_content = mutable_struct.syntax().to_string();
    if let Some(struct_indent) = struct_indent {
        struct_content = utils::reduce_indenting(&struct_content, &struct_indent);
    }
    let content = [comments, non_ink_attrs, event_attr, struct_content]
        .iter()
        .filter(|item| !item.is_empty())
        .join("\n");

    // Composes edit for removing event `struct` from contract.
    let remove_struct_edit = format_edit(TextEdit::delete(struct_item.syntax().text_range()), file);

    // Returns extraction.
    Some(Extraction {
        name,
        content,
        edits: vec![remove_struct_edit],
        import_offset,
        import_indent,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::{parse_offset_at, quote_as_pretty_string};

    #[test]
    fn extract_works() {
        for (
            code,
            (start_pat, end_pat),
            (name, output, (edit_start_pat, edit_end_pat), import_offset_pat, import_indent),
        ) in [
            (
                // code: String
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event]
                        pub struct Event {
                            /// ink! topic.
                            #[ink(topic)]
                            flag: bool,
                        }
                    }
                },
                // range -> (start_pat: Option<&str>, end_pat: Option<&str>)
                (Some("<-#[ink::event]"), Some("#[ink::event]")),
                // Extraction:
                // (
                // name: &str,
                // content: String,
                // edit range -> (edit_start_pat: Option<&str>, edit_end_pat: Option<&str>),
                // import_offset_pat: Option<&str>
                // import_indent: &str
                // )
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event]
                        pub struct Event {
                            /// ink! topic.
                            #[ink(topic)]
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            (
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(anonymous)]
                        pub struct Event {
                            flag: bool,
                        }
                    }
                },
                (
                    Some("<-#[ink::event(anonymous)]"),
                    Some("#[ink::event(anonymous)]"),
                ),
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(anonymous)]
                        pub struct Event {
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            (
                // We omit signature_topic value to make comparisons easier due to formatting.
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(signature_topic = "")]
                        pub struct Event {
                            flag: bool,
                        }
                    }
                },
                (Some("<-#[ink::event("), Some("#[ink::event(")),
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(signature_topic = "")]
                        pub struct Event {
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            // Event 1.0 syntax.
            (
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink(event)]
                        pub struct Event {
                            flag: bool,
                        }
                    }
                },
                (Some("<-#[ink(event)]"), Some("#[ink(event)]")),
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event]
                        pub struct Event {
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            (
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink(event, anonymous)]
                        pub struct Event {
                            flag: bool,
                        }
                    }
                },
                (
                    Some("<-#[ink(event, anonymous)]"),
                    Some("#[ink(event, anonymous)]"),
                ),
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(anonymous)]
                        pub struct Event {
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            (
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink(event)]
                        #[ink(anonymous)]
                        pub struct Event {
                            flag: bool,
                        }
                    }
                },
                (Some("<-#[ink(event)]"), Some("#[ink(event)]")),
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(anonymous)]
                        pub struct Event {
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            (
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink(event, signature_topic = "")]
                        pub struct Event {
                            flag: bool,
                        }
                    }
                },
                (
                    Some(r#"<-#[ink(event, signature_topic = "")]"#),
                    Some(r#"#[ink(event, signature_topic = "")]"#),
                ),
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(signature_topic = "")]
                        pub struct Event {
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            (
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod contract {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink(event)]
                        #[ink(signature_topic = "")]
                        pub struct Event {
                            flag: bool,
                        }
                    }
                },
                (Some("<-#[ink(event)]"), Some("#[ink(event)]")),
                (
                    "Event",
                    quote_as_pretty_string! {
                        /// ink! event.
                        #[derive(Debug)]
                        #[ink::event(signature_topic = "")]
                        pub struct Event {
                            flag: bool,
                        }
                    },
                    (Some("<-/// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
            // Regular comments.
            // `prettyplease` seems to remove regular comments, so we use raw strings for this one.
            (
                r"
#[ink::contract]
mod contract {
    // ink! event.
    #[derive(Debug)]
    #[ink(event)]
    pub struct Event {
        // ink! topic.
        #[ink(topic)]
        flag: bool,
    }
}"
                .to_owned(),
                (Some("<-#[ink(event)]"), Some("#[ink(event)]")),
                (
                    "Event",
                    r"
// ink! event.
#[derive(Debug)]
#[ink::event]
pub struct Event {
    // ink! topic.
    #[ink(topic)]
    flag: bool,
}"
                    .to_owned(),
                    (Some("<-// ink! event."), Some("<-\n}")),
                    Some("mod contract {"),
                    "\n    ",
                ),
            ),
        ] {
            let file = InkFile::parse(&code);
            let range = TextRange::new(
                TextSize::from(parse_offset_at(&code, start_pat).unwrap() as u32),
                TextSize::from(parse_offset_at(&code, end_pat).unwrap() as u32),
            );
            let result = extract(&file, range);

            // Verify results.
            assert!(result.is_some());
            let expected_result = Extraction {
                name: name.to_owned(),
                content: output.trim().to_owned(),
                edits: vec![TextEdit::delete(TextRange::new(
                    TextSize::from(parse_offset_at(&code, edit_start_pat).unwrap() as u32),
                    TextSize::from(parse_offset_at(&code, edit_end_pat).unwrap() as u32),
                ))],
                import_offset: TextSize::from(
                    parse_offset_at(&code, import_offset_pat).unwrap() as u32
                ),
                import_indent: import_indent.to_owned(),
            };
            assert_eq!(result.unwrap(), expected_result);
        }
    }
}
