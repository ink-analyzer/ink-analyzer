//! ink! attribute meta item IR.

use ra_ap_syntax::{AstToken, SyntaxElement, TextRange, TextSize};

pub use name::MetaName;
pub use option::MetaOption;
pub use separator::MetaSeparator;
pub use value::MetaValue;

mod name;
mod option;
mod separator;
mod value;

/// An ink! attribute meta item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ast/attr_args.rs#L40-L49>.
///
/// Ref: <https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax>.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetaNameValue {
    /// Name of the meta item.
    name: MetaOption<MetaName>,
    /// Name-value pair separator (i.e `=` symbol).
    eq: Option<MetaSeparator>,
    /// Value of meta item.
    value: MetaOption<MetaValue>,
    /// Offset of meta item.
    // Useful in case where the meta item is empty.
    offset: TextSize,
}

impl MetaNameValue {
    /// Creates a meta item.
    pub fn new(
        name: MetaOption<MetaName>,
        eq: Option<MetaSeparator>,
        value: MetaOption<MetaValue>,
        offset: TextSize,
    ) -> Self {
        Self {
            name,
            eq,
            value,
            offset,
        }
    }

    /// Create an empty meta item.
    pub fn empty(offset: TextSize) -> Self {
        Self::new(MetaOption::None, None, MetaOption::None, offset)
    }

    /// Returns true if the meta item is empty.
    pub fn is_empty(&self) -> bool {
        self.name.is_none() && self.eq.is_none() && self.value.is_none()
    }

    /// Returns the name of meta item.
    pub fn name(&self) -> &MetaOption<MetaName> {
        &self.name
    }

    /// Returns the name-value pair separator (if any) of meta item.
    pub fn eq(&self) -> Option<&MetaSeparator> {
        self.eq.as_ref()
    }

    /// Returns the value of meta item.
    pub fn value(&self) -> &MetaOption<MetaValue> {
        &self.value
    }

    /// Returns the text range of meta item.
    pub fn text_range(&self) -> TextRange {
        let mut start: Option<TextSize> = None;
        let mut end: Option<TextSize> = None;

        let mut update_start_and_end = |range: TextRange| {
            if start.is_none() {
                start = Some(range.start());
            }
            end = Some(range.end());
        };

        let get_items_text_range = |items: &[SyntaxElement]| -> Option<TextRange> {
            Some(TextRange::new(
                items.first()?.text_range().start(),
                items.last()?.text_range().end(),
            ))
        };

        // Parse start and end from name field.
        match &self.name {
            MetaOption::Ok(meta_name) => {
                update_start_and_end(meta_name.syntax().text_range());
            }
            MetaOption::Err(items) => {
                if let Some(range) = get_items_text_range(items) {
                    update_start_and_end(range);
                }
            }
            MetaOption::None => (),
        }

        // Parse start and end from eq token field.
        if let Some(token) = &self.eq {
            update_start_and_end(token.syntax().text_range());
        }

        // Parse start and end from value field.
        match &self.value {
            MetaOption::Ok(meta_value) => {
                update_start_and_end(meta_value.text_range());
            }
            MetaOption::Err(items) => {
                if let Some(range) = get_items_text_range(items) {
                    update_start_and_end(range);
                }
            }
            MetaOption::None => (),
        }

        // Fallback to using the separator offset if the meta item is empty.
        TextRange::new(
            start.unwrap_or(self.offset),
            end.unwrap_or(self.offset + TextSize::from(1)),
        )
    }
}
