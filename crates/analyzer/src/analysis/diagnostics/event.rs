//! ink! event diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasAttrs, HasGenericParams};
use ink_analyzer_ir::{
    ast, Event, FromSyntax, InkArgKind, InkAttributeKind, IsInkEntity, IsInkStruct,
};

use super::{topic, utils};
use crate::{Diagnostic, Severity};

const EVENT_SCOPE_NAME: &str = "event";

/// Runs all ink! event diagnostics.
///
/// The entry point for finding ink! event semantic rules is the event module of the ink_ir crate.
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
    event
        .topics()
        .iter()
        .for_each(|item| topic::diagnostics(results, item));

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
                .to_string(),
            range: generics.syntax().text_range(),
            severity: Severity::Error,
        })
}

/// Ensures that ink! event has only ink! topic annotations (if any) on it's descendants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L126-L139>.
fn ensure_only_ink_topic_descendants(results: &mut Vec<Diagnostic>, item: &Event) {
    item.tree().ink_attrs_descendants().for_each(|attr| {
        (*attr.kind() != InkAttributeKind::Arg(InkArgKind::Topic)).then(|| {
            results.push(Diagnostic {
                message: format!("`{}` can't be used inside an ink! event.", attr.syntax()),
                range: attr.syntax().text_range(),
                severity: Severity::Error,
            })
        });
    });
}

/// Ensures that ink! event fields are not annotated with cfg attributes.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L112-L117>.
fn ensure_no_cfg_event_fields(results: &mut Vec<Diagnostic>, event: &Event) {
    if let Some(struct_item) = event.struct_item() {
        if let Some(ast::FieldList::RecordFieldList(field_list)) = struct_item.field_list() {
            field_list.fields().for_each(|field| {
                field.attrs().for_each(|attr| {
                    if let Some(path) = attr.path() {
                        (path.to_string() == "cfg").then(|| {
                            results.push(Diagnostic {
                                message: format!(
                                    "`{}` attributes on event fields are not supported.",
                                    attr
                                ),
                                range: attr.syntax().text_range(),
                                severity: Severity::Error,
                            });
                        });
                    }
                });
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Severity;
    use ink_analyzer_ir::{FromInkAttribute, InkArgKind, InkAttributeKind, InkFile, IsInkEntity};
    use quote::quote;
    use test_utils::quote_as_str;

    fn parse_first_event(code: &str) -> Event {
        Event::cast(
            InkFile::parse(code)
                .tree()
                .ink_attrs_in_scope()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Event))
                .unwrap(),
        )
        .unwrap()
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
            assert!(result.is_none(), "event: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L346-L359>.
    fn non_pub_struct_fails() {
        for vis in vec![
            quote! {}, // no visibility
            quote! { crate },
            quote! { pub(crate) },
            quote! { pub(self) },
            quote! { pub(super) },
            quote! { pub(in my::path) },
        ] {
            let event = parse_first_event(quote_as_str! {
                #[ink(event)]
                #vis struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            });

            let result = utils::ensure_pub_struct(&event, EVENT_SCOPE_NAME);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Error);
        }
    }

    #[test]
    fn contract_parent_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let result = utils::ensure_contract_parent(&event, EVENT_SCOPE_NAME);
            assert!(result.is_none(), "event: {}", code);
        }
    }

    #[test]
    fn non_contract_parent_fails() {
        for code in [
            // Unannotated parent.
            quote_as_str! {
                mod my_contract {
                    #[ink(event)]
                    pub struct MyEvent {
                        #[ink(topic)]
                        value: bool,
                    }
                }
            },
            // Contract ancestor.
            quote_as_str! {
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
        ] {
            let event = parse_first_event(code);

            let result = utils::ensure_contract_parent(&event, EVENT_SCOPE_NAME);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Error);
        }
    }

    #[test]
    fn struct_with_no_generics_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let result = ensure_no_generics_on_struct(&event);
            assert!(result.is_none(), "event: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L331-L344>.
    fn struct_with_generics_fails() {
        let event = parse_first_event(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent<T> {
                #[ink(topic)]
                value: T,
            }
        });

        let result = ensure_no_generics_on_struct(&event);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn ink_topic_field_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_only_ink_topic_descendants(&mut results, &event);
            assert!(results.is_empty(), "event: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn non_topic_ink_field_fails() {
        let event = parse_first_event(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[ink(message)]
                value: bool,
            }
        });

        let mut results = Vec::new();
        ensure_only_ink_topic_descendants(&mut results, &event);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
    }

    #[test]
    fn non_cfg_field_works() {
        for code in valid_events!() {
            let event = parse_first_event(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_no_cfg_event_fields(&mut results, &event);
            assert!(results.is_empty(), "event: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn cfg_field_fails() {
        let event = parse_first_event(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[cfg(test)]
                value: bool,
            }
        });

        let mut results = Vec::new();
        ensure_no_cfg_event_fields(&mut results, &event);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
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
            assert!(results.is_empty(), "event: {}", code);
        }
    }
}
