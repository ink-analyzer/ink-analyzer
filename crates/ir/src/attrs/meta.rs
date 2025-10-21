//! ink! attribute meta item IR.

mod name;
mod option;
mod separator;
mod value;

use std::fmt;

use itertools::Itertools;
use ra_ap_syntax::{ast, AstNode, AstToken, SyntaxElement, TextRange, TextSize};

use crate::closest_ancestor_ast_type;

pub use name::MetaName;
pub use option::MetaOption;
pub use separator::MetaSeparator;
pub use value::MetaValue;

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
    /// Nested meta item.
    nested: Option<Box<MetaNameValue>>,
    /// Start offset of meta item.
    ///
    /// Useful in cases where the meta item is empty.
    start: TextSize,
    /// End offset of meta item.
    ///
    /// Useful in cases where the meta item is "semantically" empty,
    /// but still covers a non-zero range (e.g. representing incomplete meta items like `(`).
    end: Option<TextSize>,
    /// Parent token tree of meta item.
    ///
    /// Only set when the meta item is "semantically" empty, to allow traversal back to parent items.
    parent_tt: Option<ast::TokenTree>,
}

impl MetaNameValue {
    /// Creates a meta item.
    ///
    /// # Panics
    ///
    /// This method panics if the name, eq, value and nested arguments are all empty,
    /// Use the [`Self::empty`] method instead in these cases.
    pub fn new(
        name: MetaOption<MetaName>,
        eq: Option<MetaSeparator>,
        value: MetaOption<MetaValue>,
        nested: Option<Box<MetaNameValue>>,
        start: TextSize,
    ) -> Self {
        assert!(name.is_some() || eq.is_some() || value.is_some() || nested.is_some());
        Self {
            name,
            eq,
            value,
            nested,
            start,
            end: None,
            parent_tt: None,
        }
    }

    /// Create an empty meta item.
    ///
    /// # Panics
    ///
    /// This method panics if the `start` offset (and `end` offset, if set) is not contained in `parent_tt`.
    pub fn empty(start: TextSize, end: Option<TextSize>, parent_tt: ast::TokenTree) -> Self {
        assert!(parent_tt.syntax().text_range().contains_inclusive(start));
        if let Some(ref end) = end {
            assert!(parent_tt.syntax().text_range().contains_inclusive(*end));
        }
        Self {
            name: MetaOption::None,
            eq: None,
            value: MetaOption::None,
            nested: None,
            start,
            end,
            parent_tt: Some(parent_tt),
        }
    }

    /// Returns true if the meta item is empty.
    pub fn is_empty(&self) -> bool {
        self.name.is_none() && self.eq.is_none() && self.value.is_none() && self.nested.is_none()
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

    /// Returns the nested meta item.
    pub fn nested(&self) -> Option<&MetaNameValue> {
        self.nested.as_ref().map(Box::as_ref)
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

        // Parse start and end from nested meta item.
        if let Some(nested_meta) = &self.nested {
            update_start_and_end(nested_meta.text_range());
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

        // Fallback to using the separator start and end offsets if the meta item is "semantically" empty.
        TextRange::new(
            start.unwrap_or(self.start),
            end.or(self.end).unwrap_or(self.start),
        )
    }

    /// Returns parent attribute (if any).
    pub fn parent_attr(&self) -> Option<ast::Attr> {
        self.elements()
            .next()
            .as_ref()
            .and_then(closest_ancestor_ast_type)
            .or_else(|| {
                self.parent_tt
                    .as_ref()
                    .map(ast::TokenTree::syntax)
                    .and_then(closest_ancestor_ast_type)
            })
    }

    /// Returns all the elements of the meta item.
    pub fn elements(&self) -> impl Iterator<Item = SyntaxElement> {
        let mut elems = Vec::new();
        match &self.name {
            MetaOption::Ok(name) => elems.push(SyntaxElement::from(name.syntax().clone())),
            MetaOption::Err(name_elems) => elems.extend(name_elems.iter().cloned()),
            MetaOption::None => (),
        };

        if let Some(eq) = &self.eq {
            elems.push(SyntaxElement::from(eq.syntax().clone()));
        }

        match &self.value {
            MetaOption::Ok(name) => elems.extend(name.elements().iter().cloned()),
            MetaOption::Err(value_elems) => elems.extend(value_elems.iter().cloned()),
            MetaOption::None => (),
        }

        if let Some(nested) = &self.nested {
            elems.extend(nested.elements());
        }

        elems
            .into_iter()
            .sorted_by_key(|elem| elem.text_range().start())
    }
}

impl fmt::Display for MetaNameValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.name,
            match &self.nested {
                Some(nested_meta) => {
                    format!("({nested_meta})")
                }
                None => "".to_owned(),
            },
            match self.eq {
                Some(_) => " = ",
                None => "",
            },
            self.value
        )
    }
}
