//! ink! attribute meta item option.

use ra_ap_syntax::SyntaxElement;

/// An ink! attribute meta item option.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum MetaOption<T> {
    /// A valid item for the meta item type.
    Ok(T),
    /// An invalid item(s) for the meta item type.
    Err(Vec<SyntaxElement>),
    /// A missing meta item.
    #[default]
    None,
}

impl<T> MetaOption<T> {
    /// Returns true if meta item is valid.
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok(_))
    }

    /// Returns true if meta item is invalid.
    pub fn is_err(&self) -> bool {
        matches!(self, Self::Err(_))
    }

    /// Returns true if meta item is missing.
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    /// Returns true if meta item is present (i.e not None).
    pub fn is_some(&self) -> bool {
        !self.is_none()
    }

    /// `Option<Result<T, E>>` wrapper for meta item.
    pub fn option(&self) -> Option<Result<&T, &Vec<SyntaxElement>>> {
        match self {
            Self::Ok(value) => Some(Ok(value)),
            Self::Err(value) => Some(Err(value)),
            Self::None => None,
        }
    }

    /// `Result<T, Option<E>>` wrapper for meta item.
    pub fn result(&self) -> Result<&T, Option<&Vec<SyntaxElement>>> {
        match self {
            Self::Ok(value) => Ok(value),
            Self::Err(value) => Err(Some(value)),
            Self::None => Err(None),
        }
    }

    /// `Result<Option<T>, E>` wrapper for meta item.
    pub fn result_option(&self) -> Result<Option<&T>, &Vec<SyntaxElement>> {
        match self {
            Self::Ok(value) => Ok(Some(value)),
            Self::Err(value) => Err(value),
            Self::None => Ok(None),
        }
    }
}
