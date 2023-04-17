//! ink! attribute meta IR.

use ra_ap_syntax::ast::Ident;

pub use option::MetaOption;
pub use separator::MetaSeparator;
pub use value::MetaValue;

mod option;
mod separator;
mod value;

/// An ink! attribute meta item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ast/attr_args.rs#L40-L49>.
/// Ref: <https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax>.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MetaNameValue {
    /// Name of the meta item.
    pub name: MetaOption<Ident>,
    /// Name-value pair separator (i.e `=` symbol).
    pub eq: Option<MetaSeparator>,
    /// Value of meta item.
    pub value: MetaOption<MetaValue>,
}

impl MetaNameValue {
    /// Returns an empty meta item.
    pub fn new(
        name: MetaOption<Ident>,
        eq: Option<MetaSeparator>,
        value: MetaOption<MetaValue>,
    ) -> Self {
        Self { name, eq, value }
    }

    /// Returns true if the meta item is empty.
    pub fn is_empty(&self) -> bool {
        self.name.is_none() && self.eq.is_none() && self.value.is_none()
    }
}
