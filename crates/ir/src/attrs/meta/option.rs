//! ink! attribute argument meta item option.

use ra_ap_syntax::SyntaxElement;

/// An ink! attribute argument item option.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum MetaOption<T> {
    /// A valid item for the argument item type.
    Ok(T),
    /// A invalid item(s) for the argument item type.
    Err(Vec<SyntaxElement>),
    /// A missing item for the argument item type.
    #[default]
    None,
}

impl<T> MetaOption<T> {
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok(_))
    }

    pub fn is_err(&self) -> bool {
        matches!(self, Self::Err(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn is_some(&self) -> bool {
        !self.is_none()
    }

    pub fn value(&self) -> Option<Result<&T, &Vec<SyntaxElement>>> {
        match self {
            Self::Ok(value) => Some(Ok(value)),
            Self::Err(value) => Some(Err(value)),
            Self::None => None,
        }
    }
}
