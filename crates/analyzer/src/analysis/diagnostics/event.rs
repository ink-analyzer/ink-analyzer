//! ink! event diagnostics.

mod topic;

use ink_analyzer_ir::ast::{AstNode, HasAttrs, HasGenericParams};
use ink_analyzer_ir::{ast, InkArgKind, InkAttributeKind, IsInkEvent};

use super::common;
use crate::analysis::text_edit::TextEdit;
use crate::{Action, ActionKind, Diagnostic, Severity, Version};

const SCOPE_NAME: &str = "event";

/// Runs all ink! event diagnostics.
///
/// The entry point for finding ink! event semantic rules is the event module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86-L148>.
pub fn diagnostics<T>(results: &mut Vec<Diagnostic>, event: &T, version: Version)
where
    T: IsInkEvent,
{
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, event, version);

    // Ensures that ink! event is a `struct` with `pub` visibility, see `utils::ensure_pub_struct` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L105>.
    if let Some(diagnostic) = common::ensure_pub_struct(event, SCOPE_NAME) {
        results.push(diagnostic);
    }

    if version == Version::Legacy
        || event
            .ink_attr()
            .is_some_and(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Event))
    {
        // Ensures that ink! event is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L475>.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L79>.
        if let Some(diagnostic) = common::ensure_contract_parent(event, SCOPE_NAME) {
            results.push(diagnostic);
        }
    }

    // Ensures that ink! event struct has no generic parameters, see `ensure_no_generics_on_struct` doc.
    if let Some(diagnostic) = ensure_no_generics_on_struct(event) {
        results.push(diagnostic);
    }

    // Ensures that ink! event `struct` fields have no other ink! annotations other than ink! topic, see `ensure_only_ink_topic_fields` doc.
    ensure_only_ink_topic_descendants(results, event);

    // Runs ink! topic diagnostics, see `topic::diagnostics` doc.
    for item in event.topics() {
        topic::diagnostics(results, item, version);
    }

    // Ensures that ink! event fields are not annotated with `cfg` attributes, see `ensure_no_cfg_event_fields` doc.
    ensure_no_cfg_event_fields(results, event);
}

/// Ensures that ink! event `struct` has no generic parameters.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L99-L104>.
fn ensure_no_generics_on_struct<T>(event: &T) -> Option<Diagnostic>
where
    T: IsInkEvent,
{
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
fn ensure_only_ink_topic_descendants<T>(results: &mut Vec<Diagnostic>, item: &T)
where
    T: IsInkEvent,
{
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
fn ensure_no_cfg_event_fields<T>(results: &mut Vec<Diagnostic>, event: &T)
where
    T: IsInkEvent,
{
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
    use ink_analyzer_ir::{Event, EventV2, MinorVersion};
    use quote::quote;
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

    fn parse_first_event(code: &str) -> Event {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! events used for positive(`works`) tests for ink! event verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L251-L257>.
    macro_rules! valid_events {
        () => {
            valid_events!(v4)
        };
        (v4) => {
            valid_events!([
                quote! { #[ink(event)] },
                quote! { #[ink(event, anonymous)] },
                quote! {
                    #[ink(event)]
                    #[ink(anonymous)]
                },
            ])
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
        (v5) => {
            valid_events!([
                quote! { #[ink::event] },
                quote! { #[ink::event(anonymous)] },
                quote! { #[ink::event(signature_topic = "1111111111111111111111111111111111111111111111111111111111111111")] },
            ]).chain(
                valid_events!([
                    quote! { #[ink(event)] },
                    quote! { #[ink(event, anonymous)] },
                    quote! {
                        #[ink(event)]
                        #[ink(signature_topic = "1111111111111111111111111111111111111111111111111111111111111111")]
                    },
                    quote! { #[ink(event, anonymous)] },
                    quote! {
                        #[ink(event)]
                        #[ink(signature_topic = "1111111111111111111111111111111111111111111111111111111111111111")]
                    },
                ])
                // Wrap in contract for context sensitive tests.
                .map(|items| {
                    quote! {
                        #[ink::contract]
                        mod my_contract {
                            #items
                        }
                    }
                })
            )
        };
        ($attrs: expr) => {
            [quote! {
                pub struct MyEvent {
                    #[ink(topic)]
                    field_1: i32,
                    field_2: bool,
                }
            }]
            .iter()
            .flat_map(|code| {
                $attrs.into_iter().map(move |attr| {
                    quote! {
                        #attr
                        #code
                    }
                })
            })
        };
    }

    macro_rules! is_event_v2 {
        ($version: ident, $code: ident) => {
            // We check with `(event` because, unlike `::event`, it doesn't need prettyfying
            // to remove extra spaces.
            $version.is_gte_v5() && !$code.to_string().contains("(event")
        };
    }

    #[test]
    fn pub_struct_works() {
        for (version, events) in versioned_fixtures!(valid_events) {
            for code in events {
                let result = if is_event_v2!(version, code) {
                    let event: EventV2 = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_pub_struct(&event, SCOPE_NAME)
                } else {
                    let event: Event = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    common::ensure_pub_struct(&event, SCOPE_NAME)
                };
                assert!(result.is_none(), "event: {code}, version: {:?}", version);
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L346-L359>.
    fn non_pub_struct_fails() {
        for (version, attr) in [
            (Version::Legacy, quote! { #[ink(event)] }),
            (Version::V5(MinorVersion::V5_0), quote! { #[ink::event] }),
        ] {
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
                    #attr
                    #vis struct MyEvent {
                        #[ink(topic)]
                        value: bool,
                    }
                };

                let result = if is_event_v2!(version, code) {
                    let event: EventV2 = parse_first_ink_entity_of_type(&code);
                    common::ensure_pub_struct(&event, SCOPE_NAME)
                } else {
                    let event: Event = parse_first_ink_entity_of_type(&code);
                    common::ensure_pub_struct(&event, SCOPE_NAME)
                };

                // Verifies diagnostics.
                assert!(result.is_some(), "event: {code}, version: {:?}", version);
                assert_eq!(
                    result.as_ref().unwrap().severity,
                    Severity::Error,
                    "event: {code}, version: {:?}",
                    version
                );
                // Verifies quickfixes.
                verify_actions(
                    &code,
                    result.as_ref().unwrap().quickfixes.as_ref().unwrap(),
                    &expected_quickfixes,
                );
            }
        }
    }

    #[test]
    fn contract_parent_works() {
        for (version, events) in versioned_fixtures!(valid_events) {
            for code in events {
                if !is_event_v2!(version, code) {
                    let event: Event = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    let result = common::ensure_contract_parent(&event, SCOPE_NAME);
                    assert!(result.is_none(), "event: {code}, version: {:?}", version);
                }
            }
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

            let result = common::ensure_contract_parent(&event, SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some(), "event: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "event: {code}"
            );
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
        for (version, events) in versioned_fixtures!(valid_events) {
            for code in events {
                let result = if is_event_v2!(version, code) {
                    let event: EventV2 = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_no_generics_on_struct(&event)
                } else {
                    let event: Event = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_no_generics_on_struct(&event)
                };
                assert!(result.is_none(), "event: {code}, version: {:?}", version);
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L331-L344>.
    fn struct_with_generics_fails() {
        for (version, attr) in [
            (Version::Legacy, quote! { #[ink(event)] }),
            (Version::V5(MinorVersion::V5_0), quote! { #[ink::event] }),
        ] {
            let code = quote_as_pretty_string! {
                #attr
                pub struct MyEvent<T> {
                    #[ink(topic)]
                    value: T,
                }
            };

            let result = if is_event_v2!(version, code) {
                let event: EventV2 = parse_first_ink_entity_of_type(&code);
                ensure_no_generics_on_struct(&event)
            } else {
                let event: Event = parse_first_ink_entity_of_type(&code);
                ensure_no_generics_on_struct(&event)
            };

            // Verifies diagnostics.
            assert!(result.is_some(), "event: {code}, version: {:?}", version);
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "event: {code}, version: {:?}",
                version
            );
            // Verifies quickfixes.
            let expected_quickfixes = [TestResultAction {
                label: "Remove generic",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-<T>"),
                    end_pat: Some("<T>"),
                }],
            }];
            let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn ink_topic_field_works() {
        for (version, events) in versioned_fixtures!(valid_events) {
            for code in events {
                let mut results = Vec::new();
                if is_event_v2!(version, code) {
                    let event: EventV2 = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_only_ink_topic_descendants(&mut results, &event);
                } else {
                    let event: Event = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_only_ink_topic_descendants(&mut results, &event);
                }
                assert!(results.is_empty(), "event: {code}, version: {:?}", version);
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn non_topic_ink_field_fails() {
        for (version, attr) in [
            (Version::Legacy, quote! { #[ink(event)] }),
            (Version::V5(MinorVersion::V5_0), quote! { #[ink::event] }),
        ] {
            let code = quote_as_pretty_string! {
                #attr
                pub struct MyEvent {
                    #[ink(message)]
                    value: bool,
                }
            };

            let mut results = Vec::new();
            if is_event_v2!(version, code) {
                let event: EventV2 = parse_first_ink_entity_of_type(&code);
                ensure_only_ink_topic_descendants(&mut results, &event);
            } else {
                let event: Event = parse_first_ink_entity_of_type(&code);
                ensure_only_ink_topic_descendants(&mut results, &event);
            }

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "event: {code}, version: {:?}", version);
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "event: {code}, version: {:?}",
                version
            );
            // Verifies quickfixes.
            let expected_quickfixes = [TestResultAction {
                label: "Remove `#[ink(message)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[ink(message)]"),
                    end_pat: Some("#[ink(message)]"),
                }],
            }];
            let quickfixes = results[0].quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn non_cfg_field_works() {
        for (version, events) in versioned_fixtures!(valid_events) {
            for code in events {
                let mut results = Vec::new();
                if is_event_v2!(version, code) {
                    let event: EventV2 = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_no_cfg_event_fields(&mut results, &event);
                } else {
                    let event: Event = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    ensure_no_cfg_event_fields(&mut results, &event);
                }
                assert!(results.is_empty(), "event: {code}, version: {:?}", version);
            }
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn cfg_field_fails() {
        for (version, attr) in [
            (Version::Legacy, quote! { #[ink(event)] }),
            (Version::V5(MinorVersion::V5_0), quote! { #[ink::event] }),
        ] {
            let code = quote_as_pretty_string! {
                #attr
                pub struct MyEvent {
                    #[cfg(test)]
                    value: bool,
                }
            };

            let mut results = Vec::new();
            if is_event_v2!(version, code) {
                let event: EventV2 = parse_first_ink_entity_of_type(&code);
                ensure_no_cfg_event_fields(&mut results, &event);
            } else {
                let event: Event = parse_first_ink_entity_of_type(&code);
                ensure_no_cfg_event_fields(&mut results, &event);
            }

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "event: {code}, version: {:?}", version);
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "event: {code}, version: {:?}",
                version
            );
            // Verifies quickfixes.
            let expected_quickfixes = [TestResultAction {
                label: "Remove `#[cfg(test)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[cfg(test)]"),
                    end_pat: Some("#[cfg(test)]"),
                }],
            }];
            let quickfixes = results[0].quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L249-L260>.
    fn compound_diagnostic_works() {
        for (version, events) in versioned_fixtures!(valid_events) {
            for code in events {
                let mut results = Vec::new();
                if is_event_v2!(version, code) {
                    let event: EventV2 = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    diagnostics(&mut results, &event, version);
                } else {
                    let event: Event = parse_first_ink_entity_of_type(quote_as_str! {
                        #code
                    });
                    diagnostics(&mut results, &event, version);
                }
                assert!(results.is_empty(), "event: {code}, version: {:?}", version);
            }
        }
    }
}
