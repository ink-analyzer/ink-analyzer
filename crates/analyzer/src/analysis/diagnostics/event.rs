//! ink! event diagnostics.

use ink_analyzer_ir::ast::{AstNode, FieldList, HasAttrs, HasGenericParams};
use ink_analyzer_ir::{
    AsInkStruct, Event, FromInkAttribute, FromSyntax, IRItem, InkArgKind, InkAttributeKind,
};

use super::{topic, utils};
use crate::{Diagnostic, Severity};

/// Runs all ink! event diagnostics.
///
/// The entry point for finding ink! event semantic rules is the event module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86-L148>.
pub fn diagnostics(event: &Event) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(event));

    // Ensure ink! event is a `struct` with `pub` visibility, see `utils::ensure_pub_struct` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L105>.
    if let Some(diagnostic) = utils::ensure_pub_struct(event) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! event is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L475>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L79>.
    if let Some(diagnostic) = utils::ensure_contract_parent(event) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! event struct has no generic parameters, see `ensure_no_generics_on_struct` doc.
    if let Some(diagnostic) = ensure_no_generics_on_struct(event) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! event `struct` fields have no other ink! annotations other than ink! topic, see `ensure_only_ink_topic_fields` doc.
    utils::append_diagnostics(&mut results, &mut ensure_only_ink_topic_descendants(event));

    // Run ink! topic diagnostics, see `topic::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut event.topics().iter().flat_map(topic::diagnostics).collect(),
    );

    // Ensure ink! event fields are not annotated with cfg attributes, see `ensure_no_cfg_event_fields` doc.
    utils::append_diagnostics(&mut results, &mut ensure_no_cfg_event_fields(event));

    results
}

/// Ensure ink! event `struct` has no generic parameters.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L99-L104>.
fn ensure_no_generics_on_struct(event: &Event) -> Option<Diagnostic> {
    let ink_attr = event.ink_attr();

    if let Some(struct_item) = event.struct_item() {
        if let Some(generics) = struct_item.generic_param_list() {
            return Some(Diagnostic {
                message: format!(
                    "Generic types on a `struct` annotated with `{}` are not currently supported.",
                    ink_attr.syntax()
                ),
                range: generics.syntax().text_range(),
                severity: Severity::Error,
            });
        }
    }

    None
}

/// Ensure ink! event has only ink! topic annotations (if any) on it's descendants.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L126-L139>.
fn ensure_only_ink_topic_descendants(item: &Event) -> Vec<Diagnostic> {
    item.ink_attrs_descendants()
        .iter()
        .filter_map(|attr| {
            if *attr.kind() != InkAttributeKind::Arg(InkArgKind::Topic) {
                return Some(Diagnostic {
                    message: format!("`{}` can't be used inside an ink! event.", attr.syntax()),
                    range: attr.syntax().text_range(),
                    severity: Severity::Error,
                });
            }

            None
        })
        .collect()
}

/// Ensure ink! event fields are not annotated with cfg attributes.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L112-L117>.
fn ensure_no_cfg_event_fields(event: &Event) -> Vec<Diagnostic> {
    if let Some(struct_item) = event.struct_item() {
        if let Some(FieldList::RecordFieldList(field_list)) = struct_item.field_list() {
            return field_list
                .fields()
                .flat_map(|field| {
                    field
                        .attrs()
                        .filter_map(|attr| {
                            if let Some(path) = attr.path() {
                                if path.to_string() == "cfg" {
                                    return Some(Diagnostic {
                                        message: format!(
                                            "`{}` attributes on event fields are not supported.",
                                            attr
                                        ),
                                        range: attr.syntax().text_range(),
                                        severity: Severity::Error,
                                    });
                                }
                            }
                            None
                        })
                        .collect::<Vec<Diagnostic>>()
                })
                .collect();
        }
    }

    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Severity;
    use ink_analyzer_ir::{
        quote_as_str, FromInkAttribute, IRItem, InkArgKind, InkAttributeKind, InkFile,
    };
    use quote::quote;

    fn parse_first_event_item(code: &str) -> Event {
        Event::cast(
            InkFile::parse(code)
                .ink_attrs_in_scope()
                .into_iter()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Event))
                .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn pub_struct_works() {
        let event = parse_first_event_item(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[ink(topic)]
                value: bool,
            }
        });

        let result = utils::ensure_pub_struct(&event);
        assert!(result.is_none());
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
            let event = parse_first_event_item(quote_as_str! {
                #[ink(event)]
                #vis struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            });

            let result = utils::ensure_pub_struct(&event);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Error);
        }
    }

    #[test]
    fn contract_parent_works() {
        let event = parse_first_event_item(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                #[ink(event)]
                pub struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        });

        let result = utils::ensure_contract_parent(&event);
        assert!(result.is_none());
    }

    #[test]
    fn non_contract_parent_fails() {
        let event = parse_first_event_item(quote_as_str! {
            mod my_contract {
                #[ink(event)]
                pub struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        });

        let result = utils::ensure_contract_parent(&event);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn contract_ancestor_fails() {
        let event = parse_first_event_item(quote_as_str! {
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
        });

        let result = utils::ensure_contract_parent(&event);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn struct_with_no_generics_works() {
        let event = parse_first_event_item(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[ink(topic)]
                value: bool,
            }
        });

        let result = ensure_no_generics_on_struct(&event);
        assert!(result.is_none());
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L331-L344>.
    fn struct_with_generics_fails() {
        let event = parse_first_event_item(quote_as_str! {
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
        let event = parse_first_event_item(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[ink(topic)]
                value: bool,
            }
        });

        let results = ensure_only_ink_topic_descendants(&event);
        assert!(results.is_empty());
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn non_topic_ink_field_fails() {
        let event = parse_first_event_item(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[ink(message)]
                value: bool,
            }
        });

        let results = ensure_only_ink_topic_descendants(&event);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
    }

    #[test]
    fn non_cfg_field_works() {
        let event = parse_first_event_item(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[ink(topic)]
                value: bool,
            }
        });

        let results = ensure_no_cfg_event_fields(&event);
        assert!(results.is_empty());
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L377-L390>.
    fn cfg_field_fails() {
        let event = parse_first_event_item(quote_as_str! {
            #[ink(event)]
            pub struct MyEvent {
                #[cfg(test)]
                value: bool,
            }
        });

        let results = ensure_no_cfg_event_fields(&event);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
    }
}
