//! ink! attribute arguments IR.

use ra_ap_syntax::ast::Ident;

pub use option::MetaOption;
pub use separator::MetaSeparator;
pub use value::MetaValue;

mod option;
mod separator;
mod value;

/// An ink! attribute argument.
// See https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax for grammar
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MetaArg {
    /// Name of the argument.
    pub name: MetaOption<Ident>,
    /// Eq (=) token/symbol.
    pub eq: Option<MetaSeparator>,
    /// Value of the argument.
    pub value: MetaOption<MetaValue>,
}

impl MetaArg {
    /// Returns an empty Arg.
    pub fn new(
        name: MetaOption<Ident>,
        eq: Option<MetaSeparator>,
        value: MetaOption<MetaValue>,
    ) -> Self {
        Self { name, eq, value }
    }

    /// Returns true if the Arg empty.
    pub fn is_empty(&self) -> bool {
        self.name.is_none() && self.eq.is_none() && self.value.is_none()
    }
}
