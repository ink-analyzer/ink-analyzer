//! ink! selector IR.

use blake2::digest::consts::U32;
use blake2::digest::Digest;
use blake2::Blake2b;
use ra_ap_syntax::ast::{HasName, Type};
use ra_ap_syntax::{AstNode, SyntaxKind, TextRange};

use crate::{utils, InkArg, InkArgKind, InkCallable, InkImplItem};

/// The selector of an ink! callable entity.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/selector.rs#L21-L29>.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Selector {
    /// The four byte array representation of the selector.
    bytes: [u8; 4],
}

impl Selector {
    /// Returns the composed selector of the ink! callable entity.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/selector.rs#L74-L126>.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/callable.rs#L203-L371>.
    pub fn compose<T>(callable: &T) -> Option<Self>
    where
        T: InkCallable,
    {
        let mut selector_bytes: Option<[u8; 4]> = None;

        // Manually provided integer selector is converted into bytes.
        if let Some(arg) = callable.selector_arg() {
            if let Some(manual_int_selector) = arg.as_u32() {
                selector_bytes = Some(manual_int_selector.to_be_bytes())
            }
        }

        // Otherwise the selector has to be computed, but only if the callable is a valid `fn` item.
        if selector_bytes.is_none() {
            let callable_ident = Self::get_ident(callable)?;
            let trait_ident = Self::get_trait_ident(callable);
            let namespace = Self::get_namespace(callable);

            let pre_hash_bytes = [namespace, trait_ident, Some(callable_ident)]
                .into_iter()
                .flatten()
                .collect::<Vec<String>>()
                .join("::")
                .into_bytes();

            let mut hasher = <Blake2b<U32>>::new();
            hasher.update(pre_hash_bytes);
            let hashed_bytes = hasher.finalize();

            selector_bytes = Some([
                hashed_bytes[0],
                hashed_bytes[1],
                hashed_bytes[2],
                hashed_bytes[3],
            ]);
        }

        selector_bytes.map(|value| Self { bytes: value })
    }

    /// Returns the underlying four bytes.
    pub fn to_bytes(&self) -> [u8; 4] {
        self.bytes
    }

    /// Returns the big-endian `u32` representation of the selector bytes.
    pub fn into_be_u32(self) -> u32 {
        u32::from_be_bytes(self.bytes)
    }

    /// Returns the identifier for the callable as a string.
    fn get_ident<T>(callable: &T) -> Option<String>
    where
        T: InkCallable,
    {
        callable.fn_item()?.name().map(|name| name.to_string())
    }

    /// Returns the effective identifier for callable's parent trait (if any).
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/callable.rs#L346-L368>.
    fn get_trait_ident<T>(callable: &T) -> Option<String>
    where
        T: InkCallable,
    {
        if let Type::PathType(trait_path_type) = callable.impl_item()?.trait_()? {
            let trait_path = trait_path_type.path()?;
            let is_full_path = trait_path.to_string().starts_with("::");
            let trait_ident = if is_full_path {
                let mut full_path = trait_path.to_string();
                full_path.retain(|c| !c.is_whitespace());
                full_path
            } else {
                trait_path
                    .segments()
                    .last()
                    .map(|segment| segment.to_string())
                    .unwrap_or(String::new())
            };
            (!trait_ident.is_empty()).then_some(trait_ident)
        } else {
            None
        }
    }

    /// Returns the identifier for callable's parent ink! impl namespace argument (if any).
    fn get_namespace<T>(callable: &T) -> Option<String>
    where
        T: InkCallable,
    {
        utils::ink_arg_by_kind(callable.impl_item()?.syntax(), InkArgKind::Namespace)?
            .value()?
            .as_string()
    }
}

/// An ink! selector argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorArg {
    /// The kind of the ink! selector.
    kind: SelectorArgKind,
    /// The ink! attribute argument for the selector.
    arg: InkArg,
}

impl SelectorArg {
    /// Returns true if the syntax node can be converted into an ink! impl item.
    pub fn can_cast(arg: &InkArg) -> bool {
        *arg.kind() == InkArgKind::Selector
    }

    /// Convert an ink! attribute argument into a Selector IR type.
    pub fn cast(arg: InkArg) -> Option<Self> {
        Self::can_cast(&arg).then_some(Self {
            kind: if let Some(value) = arg.value() {
                match value.kind() {
                    SyntaxKind::INT_NUMBER => SelectorArgKind::Integer,
                    SyntaxKind::UNDERSCORE | SyntaxKind::UNDERSCORE_EXPR => {
                        SelectorArgKind::Wildcard
                    }
                    _ => SelectorArgKind::Other,
                }
            } else {
                SelectorArgKind::Other
            },
            arg,
        })
    }

    /// Returns the ink! selector argument kind.
    pub fn kind(&self) -> &SelectorArgKind {
        &self.kind
    }

    /// Returns the ink! attribute argument for ink! selector.
    pub fn arg(&self) -> &InkArg {
        &self.arg
    }

    /// Returns true if the value is a wildcard/underscore expression.
    pub fn is_wildcard(&self) -> bool {
        self.kind == SelectorArgKind::Wildcard
    }

    /// Converts the value if it's an integer literal (decimal or hexadecimal) into a `u32`.
    pub fn as_u32(&self) -> Option<u32> {
        self.arg.value()?.as_u32()
    }

    /// Returns the text range of the ink! selector argument.
    pub fn text_range(&self) -> TextRange {
        self.arg.text_range()
    }
}

/// The ink! selector argument kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelectorArgKind {
    Integer,
    Wildcard,
    Other,
}
