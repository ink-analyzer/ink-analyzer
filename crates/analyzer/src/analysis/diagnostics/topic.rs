//! ink! topic diagnostics.

use ink_analyzer_ir::{InkEntity, Topic};

use super::utils;
use crate::{Action, Diagnostic, Severity};

const TOPIC_SCOPE_NAME: &str = "topic";

/// Runs all ink! topic diagnostics.
///
/// The entry point for finding ink! topic semantic rules is the event module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86-L148>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, topic: &Topic) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, topic);

    // Ensures that ink! topic is a `struct` field, see `ensure_struct_field` doc.
    if let Some(diagnostic) = ensure_struct_field(topic) {
        results.push(diagnostic);
    }

    // Ensures that ink! topic has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::ensure_no_ink_descendants(results, topic, TOPIC_SCOPE_NAME);
}

/// Ensures that ink! topic is a `struct` field.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L106-L140>.
fn ensure_struct_field(topic: &Topic) -> Option<Diagnostic> {
    let ink_attr = topic.ink_attr()?;
    topic.field().is_none().then_some(Diagnostic {
        message: format!(
            "`{}` can only be applied to a `struct` field.",
            ink_attr.syntax()
        ),
        range: topic.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: Some(vec![Action::remove_attribute(ink_attr)]),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use ink_analyzer_ir::syntax::{TextRange, TextSize};
    use quote::quote;
    use test_utils::{parse_offset_at, quote_as_pretty_string, quote_as_str};

    fn parse_first_topic_field(code: &str) -> Topic {
        parse_first_ink_entity_of_type(code)
    }

    #[test]
    fn struct_field_works() {
        let topic = parse_first_topic_field(quote_as_str! {
            pub struct MyEvent {
                #[ink(topic)]
                value: bool,
            }
        });

        let result = ensure_struct_field(&topic);
        assert!(result.is_none());
    }

    #[test]
    fn non_struct_field_fails() {
        for item in vec![
            quote! { mod my_topic; },
            quote! {
                pub struct MyTopic {
                    value: bool,
                }
            },
            quote! { enum MyTopic {
                This,
                That,
            } },
        ] {
            let code = quote_as_pretty_string! {
                #[ink(topic)]
                #item
            };
            let topic = parse_first_topic_field(&code);

            let result = ensure_struct_field(&topic);

            // Verifies diagnostics.
            assert!(result.is_some());
            assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
            // Verifies quickfixes.
            let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
            assert!(fix.label.contains("Remove `#[ink(topic)]`"));
            assert!(fix.edits[0].text.is_empty());
            assert_eq!(
                fix.edits[0].range,
                TextRange::new(
                    TextSize::from(parse_offset_at(&code, Some("<-#[ink(topic)]")).unwrap() as u32),
                    TextSize::from(parse_offset_at(&code, Some("#[ink(topic)]")).unwrap() as u32)
                )
            );
        }
    }

    #[test]
    fn compound_diagnostic_works() {
        let topic = parse_first_topic_field(quote_as_str! {
            pub struct MyEvent {
                #[ink(topic)]
                value: bool,
            }
        });

        let mut results = Vec::new();
        diagnostics(&mut results, &topic);
        assert!(results.is_empty());
    }
}
