//! ink! event diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasAttrs, HasGenericParams};
use ink_analyzer_ir::{ast, Event, InkArgKind, InkAttributeKind, InkEntity, IsInkStruct};

use super::{topic, utils};
use crate::analysis::text_edit::TextEdit;
use crate::{Action, ActionKind, Diagnostic, Severity};

const EVENT_SCOPE_NAME: &str = "event";

/// Runs all ink! event diagnostics.
///
/// The entry point for finding ink! event semantic rules is the event module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86-L148>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, event: &Event) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, event);

    // Ensures that ink! event is a `struct` with `pub` visibility, see `utils::ensure_pub_struct` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L105>.
    if let Some(diagnostic) = utils::ensure_pub_struct(event, EVENT_SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! event is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L475>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L79>.
    if let Some(diagnostic) = utils::ensure_contract_parent(event, EVENT_SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! event struct has no generic parameters, see `ensure_no_generics_on_struct` doc.
    if let Some(diagnostic) = ensure_no_generics_on_struct(event) {
        results.push(diagnostic);
    }

    // Ensures that ink! event `struct` fields have no other ink! annotations other than ink! topic, see `ensure_only_ink_topic_fields` doc.
    ensure_only_ink_topic_descendants(results, event);

    // Runs ink! topic diagnostics, see `topic::diagnostics` doc.
    for item in event.topics() {
        topic::diagnostics(results, item);
    }

    // Ensures that ink! event fields are not annotated with `cfg` attributes, see `ensure_no_cfg_event_fields` doc.
    ensure_no_cfg_event_fields(results, event);
}

/// Ensures that ink! event `struct` has no generic parameters.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L99-L104>.
fn ensure_no_generics_on_struct(event: &Event) -> Option<Diagnostic> {
    event
        .struct_item()?
        .generic_param_list()
        .map(|generics| Diagnostic {
            message: "Generic types on ink! event `struct` items are not currently supported."
                .to_owned(),
            range: generics.syntax().text_range(),
            severity: Severity::Error,
            quickfixes: Some(vec![Action {
                label: "Remove generic types.".to_owned(),
                kind: ActionKind::QuickFix,
                range: generics.syntax().text_range(),
                edits: vec![TextEdit::delete(generics.syntax().text_range())],
            }]),
        })
}

/// Ensures that ink! event has only ink! topic annotations (if any) on it's descendants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L126-L139>.
fn ensure_only_ink_topic_descendants(results: &mut Vec<Diagnostic>, item: &Event) {
    for attr in item.tree().ink_attrs_descendants() {
        if *attr.kind() != InkAttributeKind::Arg(InkArgKind::Topic) {
            results.push(Diagnostic {
                message: format!("`{}` can't be used inside an ink! event.", attr.syntax()),
                range: attr.syntax().text_range(),
                severity: Severity::Error,
                quickfixes: Some(vec![Action::remove_attribute(&attr)]),
            });
        }
    }
}

/// Ensures that ink! event fields are not annotated with cfg attributes.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L112-L117>.
fn ensure_no_cfg_event_fields(results: &mut Vec<Diagnostic>, event: &Event) {
    if let Some(struct_item) = event.struct_item() {
        if let Some(ast::FieldList::RecordFieldList(field_list)) = struct_item.field_list() {
            for field in field_list.fields() {
                for attr in field.attrs() {
                    if let Some(path) = attr.path() {
                        if path.to_string() == "cfg" {
                            results.push(Diagnostic {
                                message: "`cfg` attributes on event fields are not supported."
                                    .to_owned(),
                                range: attr.syntax().text_range(),
                                severity: Severity::Error,
                                quickfixes: Some(vec![Action {
                                    label: format!("Remove `{attr}` attribute."),
                                    kind: ActionKind::QuickFix,
                                    range: attr.syntax().text_range(),
                                    edits: vec![TextEdit::delete(attr.syntax().text_range())],
                                }]),
                            });
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::Severity;
    use ink_analyzer_ir::syntax::{TextRange, TextSize};
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_event(code: &str) -> Event {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! events used for positive(`works`) tests for ink! event verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L251-L257>.
    macro_rules! valid_events {
        () => {
            [quote! {
                pub struct MyEvent {
                    #[ink(topic)]
                    field_1: i32,
                    field_2: bool,
                }
            }]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #[ink(event)]
                        #code
                    },
                    // Anonymous.
                    quote! {
                        #[ink(event, anonymous)]
                        #code
                    },
                    quote! {
                        #[ink(event)]
                        #[ink(anonymous)]
                        #code
                    },
                ]
            })
            // Wrap in contract for context sensitive tests.
            .map(|items| {
                quote! {
                    #[ink::contract]
                    mod my_contract {
                        #items
                    }
                }
            })
        };
    }

    #[test]
    fn pub_struct_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let result = utils::ensure_pub_struct(&event, EVENT_SCOPE_NAME);
            assert!(result.is_none(), "event: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L346-L359>.
    fn non_pub_struct_fails() {
        for (vis, expected_quickfixes) in vec![
            (
                quote! {},
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-struct"),
                        end_pat: Some("<-struct"),
                    }],
                }],
            ), // no visibility
            (
                quote! { pub(crate) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(crate)"),
                        end_pat: Some("pub(crate)"),
                    }],
                }],
            ),
            (
                quote! { pub(self) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(self)"),
                        end_pat: Some("pub(self)"),
                    }],
                }],
            ),
            (
                quote! { pub(super) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(super)"),
                        end_pat: Some("pub(super)"),
                    }],
                }],
            ),
            (
                quote! { pub(in my::path) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(in my::path)"),
                        end_pat: Some("pub(in my::path)"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(event)]
                #vis struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            };
            let event = parse_first_event(&code);

            let result = utils::ensure_pub_struct(&event, EVENT_SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some());
            assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
            // Verifies quickfixes.
            verify_actions(
                &code,
                result.as_ref().unwrap().quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn contract_parent_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let result = utils::ensure_contract_parent(&event, EVENT_SCOPE_NAME);
            assert!(result.is_none(), "event: {code}");
        }
    }

    #[test]
    fn non_contract_parent_fails() {
        for (code, expected_quickfixes) in [
            // Unannotated parent.
            (
                quote_as_pretty_string! {
                    mod my_contract {
                        #[ink(event)]
                        pub struct MyEvent {
                            #[ink(topic)]
                            value: bool,
                        }
                    }
                },
                vec![],
            ),
            // Contract ancestor.
            (
                quote_as_pretty_string! {
                    #[ink::contract]
                    mod my_contract {
                        mod my_event_mod {
                            #[ink(event)]
                            pub struct MyEvent {
                                #[ink(topic)]
                                value: bool,
                            }
                        }
                    }
                },
                vec![TestResultAction {
                    label: "Move item",
                    edits: vec![
                        TestResultTextRange {
                            text: "pub struct MyEvent",
                            start_pat: Some("my_contract {"),
                            end_pat: Some("my_contract {"),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("}"),
                        },
                    ],
                }],
            ),
        ] {
            let event = parse_first_event(&code);

            let result = utils::ensure_contract_parent(&event, EVENT_SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some());
            assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
            // Verifies quickfixes.
            verify_actions(
                &code,
                result
                    .as_ref()
                    .unwrap()
                    .quickfixes
                    .as_ref()
                    .unwrap_or(&vec![]),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn struct_with_no_generics_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let result = ensure_no_generics_on_struct(&event);
            assert!(result.is_none(), "event: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L331-L344>.
    fn struct_with_generics_fails() {
        let code = quote_as_pretty_string! {
            #[ink(event)]
            pub struct MyEvent<T> {
                #[ink(topic)]
                value: T,
            }
        };
        let event = parse_first_event(&code);

        let result = ensure_no_generics_on_struct(&event);

        // Verifies diagnostics.
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
        // Verifies quickfixes.
        let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
        assert!(fix.label.contains("Remove generic"));
        assert_eq!(&fix.edits[0].text, "");
        assert_eq!(
            fix.edits[0].range,
            TextRange::new(
                TextSize::from(parse_offset_at(&code, Some("<-<T>")).unwrap() as u32),
                TextSize::from(parse_offset_at(&code, Some("<T>")).unwrap() as u32)
            )
        );
    }

    #[test]
    fn ink_topic_field_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_only_ink_topic_descendants(&mut results, &event);
            assert!(results.is_empty(), "event: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn non_topic_ink_field_fails() {
        let code = quote_as_pretty_string! {
            #[ink(event)]
            pub struct MyEvent {
                #[ink(message)]
                value: bool,
            }
        };
        let event = parse_first_event(&code);

        let mut results = Vec::new();
        ensure_only_ink_topic_descendants(&mut results, &event);

        // Verifies diagnostics.
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
        // Verifies quickfixes.
        let fix = &results[0].quickfixes.as_ref().unwrap()[0];
        assert!(fix.label.contains("Remove `#[ink(message)]`"));
        assert_eq!(&fix.edits[0].text, "");
        assert_eq!(
            fix.edits[0].range,
            TextRange::new(
                TextSize::from(parse_offset_at(&code, Some("<-#[ink(message)]")).unwrap() as u32),
                TextSize::from(parse_offset_at(&code, Some("#[ink(message)]")).unwrap() as u32)
            )
        );
    }

    #[test]
    fn non_cfg_field_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_no_cfg_event_fields(&mut results, &event);
            assert!(results.is_empty(), "event: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn cfg_field_fails() {
        let code = quote_as_pretty_string! {
            #[ink(event)]
            pub struct MyEvent {
                #[cfg(test)]
                value: bool,
            }
        };
        let event = parse_first_event(&code);

        let mut results = Vec::new();
        ensure_no_cfg_event_fields(&mut results, &event);

        // Verifies diagnostics.
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
        // Verifies quickfixes.
        let fix = &results[0].quickfixes.as_ref().unwrap()[0];
        assert!(fix.label.contains("Remove `#[cfg(test)]`"));
        assert!(fix.edits[0].text.is_empty());
        assert_eq!(
            fix.edits[0].range,
            TextRange::new(
                TextSize::from(parse_offset_at(&code, Some("<-#[cfg(test)]")).unwrap() as u32),
                TextSize::from(parse_offset_at(&code, Some("#[cfg(test)]")).unwrap() as u32)
            )
        );
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L249-L260>.
    fn compound_diagnostic_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &event);
            assert!(results.is_empty(), "event: {code}");
        }
    }
}
