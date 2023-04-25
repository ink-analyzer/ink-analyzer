//! ink! topic diagnostics.

use ink_analyzer_ir::{FromInkAttribute, FromSyntax, Topic};

use super::utils;
use crate::{Diagnostic, Severity};

/// Runs all ink! topic diagnostics.
///
/// The entry point for finding ink! topic semantic rules is the event module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L86-L148>.
pub fn diagnostics(topic: &Topic) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(topic));

    // Ensure ink! topic is a `struct` field, see `ensure_struct_field` doc.
    if let Some(diagnostic) = ensure_struct_field(topic) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! topic has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_no_ink_descendants(topic, "topic"),
    );

    results
}

/// Ensure ink! topic is a `struct` field.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/event.rs#L106-L140>.
fn ensure_struct_field(topic: &Topic) -> Option<Diagnostic> {
    let ink_attr = topic.ink_attr();
    topic.field().is_none().then_some(Diagnostic {
        message: format!(
            "`{}` can only be applied to a `struct` field.",
            ink_attr.syntax()
        ),
        range: topic.syntax().text_range(),
        severity: Severity::Error,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{
        quote_as_str, FromInkAttribute, IRItem, InkArgKind, InkAttributeKind, InkFile,
    };
    use quote::quote;

    fn parse_first_topic_field(code: &str) -> Topic {
        Topic::cast(
            InkFile::parse(code)
                .ink_attrs_in_scope()
                .into_iter()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Topic))
                .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn struct_field_works() {
        let topic = parse_first_topic_field(quote_as_str! {
            #[ink(event)]
            pub struct Flip {
                #[ink(topic)]
                flipped: bool,
            }
        });

        let result = ensure_struct_field(&topic);
        assert!(result.is_none());
    }

    #[test]
    fn non_struct_field_fails() {
        for item in vec![
            quote! { mod flipper; },
            quote! {
                pub struct Flip {
                    flipped: bool,
                }
            },
            quote! { enum Flipper {
                This,
                That,
            } },
        ] {
            let topic = parse_first_topic_field(quote_as_str! {
                #[ink(topic)]
                #item
            });

            let result = ensure_struct_field(&topic);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Error);
        }
    }
}