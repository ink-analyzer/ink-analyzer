//! ink! attribute meta item option.

use ra_ap_syntax::SyntaxElement;
use std::fmt;

/// An ink! attribute meta item option.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum MetaOption<T: fmt::Display> {
    /// A valid item.
    Ok(T),
    /// An invalid item(s).
    Err(Vec<SyntaxElement>),
    /// A missing item.
    #[default]
    None,
}

impl<T: fmt::Display> MetaOption<T> {
    /// Returns true if variant is valid.
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok(_))
    }

    /// Returns true if variant is invalid.
    pub fn is_err(&self) -> bool {
        matches!(self, Self::Err(_))
    }

    /// Returns true if variant is missing (i.e None).
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    /// Returns true if meta item is present (i.e not None).
    pub fn is_some(&self) -> bool {
        !self.is_none()
    }

    /// `Option<Result<T, E>>` wrapper.
    pub fn option(&self) -> Option<Result<&T, &[SyntaxElement]>> {
        match self {
            Self::Ok(value) => Some(Ok(value)),
            Self::Err(value) => Some(Err(value)),
            Self::None => None,
        }
    }

    /// `Result<T, Option<E>>` wrapper.
    pub fn result(&self) -> Result<&T, Option<&[SyntaxElement]>> {
        match self {
            Self::Ok(value) => Ok(value),
            Self::Err(value) => Err(Some(value)),
            Self::None => Err(None),
        }
    }

    /// `Result<Option<T>, E>` wrapper.
    pub fn result_option(&self) -> Result<Option<&T>, &[SyntaxElement]> {
        match self {
            Self::Ok(value) => Ok(Some(value)),
            Self::Err(value) => Err(value),
            Self::None => Ok(None),
        }
    }
}

impl<T: fmt::Display> fmt::Display for MetaOption<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ok(value) => value.fmt(f),
            Self::Err(value) => write!(
                f,
                "{}",
                value.iter().map(ToString::to_string).collect::<String>()
            ),
            Self::None => write!(f, ""),
        }
    }
}
