//! ink! selector IR.

use blake2::digest::consts::U32;
use blake2::digest::Digest;
use blake2::Blake2b;
use ra_ap_syntax::ast::HasName;
use ra_ap_syntax::{ast, AstNode, SyntaxKind, TextRange};

use crate::tree::utils;
use crate::{InkArg, InkArgKind, InkCallable, InkImplItem};

/// The selector of an ink! callable entity.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/selector.rs#L21-L29>.
///
/// The selector is four byte array.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Selector([u8; 4]);

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
        let selector_bytes: Option<[u8; 4]> = match Self::provided_int_selector(callable) {
            // Manually provided integer selector is converted into bytes.
            Some(manual_int_selector) => Some(manual_int_selector.to_be_bytes()),
            // Otherwise the selector has to be computed, but only if the callable is a valid `fn` item.
            None => {
                Self::get_ident(callable).map(|callable_ident| {
                    let trait_ident = Self::get_trait_ident(callable);
                    let namespace = Self::get_namespace(callable);

                    let pre_hash_bytes = [namespace, trait_ident, Some(callable_ident)]
                        .into_iter()
                        .flatten()
                        .collect::<Vec<String>>()
                        .join("::")
                        .into_bytes();

                    // Computes the BLAKE-2b 256-bit hash for the given input and stores it in output.
                    let mut hasher = <Blake2b<U32>>::new();
                    hasher.update(pre_hash_bytes);
                    let hashed_bytes = hasher.finalize();

                    [
                        hashed_bytes[0],
                        hashed_bytes[1],
                        hashed_bytes[2],
                        hashed_bytes[3],
                    ]
                })
            }
        };

        selector_bytes.map(Self)
    }

    /// Returns the underlying four bytes.
    pub fn to_bytes(&self) -> [u8; 4] {
        self.0
    }

    /// Returns the big-endian `u32` representation of the selector bytes.
    pub fn into_be_u32(self) -> u32 {
        u32::from_be_bytes(self.0)
    }

    /// Returns the manually provided integer selector (if any).
    fn provided_int_selector<T>(callable: &T) -> Option<u32>
    where
        T: InkCallable,
    {
        callable.selector_arg()?.as_u32()
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
        match callable.impl_item()?.trait_()? {
            ast::Type::PathType(trait_path_type) => {
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
            }
            _ => None,
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

    /// Converts an ink! attribute argument into a Selector IR type.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::{Constructor, FromInkAttribute, InkAttribute, Message};
    use ra_ap_syntax::ast;
    use ra_ap_syntax::SourceFile;
    use test_utils::quote_as_str;

    fn first_ink_entity_of_type<T>(code: &str) -> T
    where
        T: FromInkAttribute + InkCallable,
    {
        SourceFile::parse(code)
            .tree()
            .syntax()
            .descendants()
            .find_map(|node| T::cast(InkAttribute::cast(ast::Attr::cast(node)?)?))
            .unwrap()
    }

    #[test]
    fn compose_works() {
        for (code, expected_constructor_selector, expected_message_selector) in [
            (
                quote_as_str! {
                    impl MyContract {
                        #[ink(constructor, selector=10)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message, selector=10)]
                        pub fn my_message(&self) {}
                    }
                },
                0x0000000A,
                0x0000000A,
            ),
            (
                quote_as_str! {
                    impl MyContract {
                        #[ink(constructor, selector=0xA)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message, selector=0xA)]
                        pub fn my_message(&self) {}
                    }
                },
                0x0000000A,
                0x0000000A,
            ),
            (
                quote_as_str! {
                    impl MyContract {
                        #[ink(constructor, selector=_)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message, selector=_)]
                        pub fn my_message(&self) {}
                    }
                },
                0xE11C2FAF, // First 4-bytes of Blake2b-256 hash of "my_constructor"
                0x6A469E03, // First 4-bytes of Blake2b-256 hash of "my_message"
            ),
            (
                quote_as_str! {
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message)]
                        pub fn my_message(&self) {}
                    }
                },
                0xE11C2FAF, // First 4-bytes of Blake2b-256 hash of "my_constructor"
                0x6A469E03, // First 4-bytes of Blake2b-256 hash of "my_message"
            ),
            (
                quote_as_str! {
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message)]
                        pub fn my_message(&self) {}
                    }
                },
                0xE11C2FAF, // First 4-bytes of Blake2b-256 hash of "my_constructor"
                0x6A469E03, // First 4-bytes of Blake2b-256 hash of "my_message"
            ),
            (
                quote_as_str! {
                    impl MyTrait for MyContract {
                        #[ink(constructor)]
                        fn my_constructor() -> Self {}

                        #[ink(message)]
                        fn my_message(&self) {}
                    }
                },
                0x235E720C, // First 4-bytes of Blake2b-256 hash of "MyTrait::my_constructor"
                0x04C49446, // First 4-bytes of Blake2b-256 hash of "MyTrait::my_message"
            ),
            (
                quote_as_str! {
                    impl ::my_full::long_path::MyTrait for MyContract {
                        #[ink(constructor)]
                        fn my_constructor() -> Self {}

                        #[ink(message)]
                        fn my_message(&self) {}
                    }
                },
                0x647E8959, // First 4-bytes of Blake2b-256 hash of "::my_full::long_path::MyTrait::my_constructor"
                0x2EC56327, // First 4-bytes of Blake2b-256 hash of "::my_full::long_path::MyTrait::my_message"
            ),
            (
                quote_as_str! {
                    impl relative_path::MyTrait for MyContract {
                        #[ink(constructor)]
                        fn my_constructor() -> Self {}

                        #[ink(message)]
                        fn my_message(&self) {}
                    }
                },
                0x235E720C, // First 4-bytes of Blake2b-256 hash of "MyTrait::my_constructor"
                0x04C49446, // First 4-bytes of Blake2b-256 hash of "MyTrait::my_message"
            ),
            (
                quote_as_str! {
                    #[ink(namespace="my_namespace")]
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message)]
                        pub fn my_message(&self) {}
                    }
                },
                0x3a739109, // First 4-bytes of Blake2b-256 hash of "my_namespace::my_constructor"
                0xABE89C04, // First 4-bytes of Blake2b-256 hash of "my_namespace::my_message"
            ),
        ] {
            // Parse ink! constructor and ink! message.
            let constructor: Constructor = first_ink_entity_of_type(code);
            let message: Message = first_ink_entity_of_type(code);

            // Check selectors.
            assert_eq!(
                Selector::compose(&constructor).unwrap().into_be_u32(),
                expected_constructor_selector
            );
            assert_eq!(
                Selector::compose(&message).unwrap().into_be_u32(),
                expected_message_selector
            );
        }
    }

    #[test]
    fn cast_arg_works() {
        for (code, expected_kind, expected_is_wildcard, expected_u32_value) in [
            (
                quote_as_str! {
                    #[ink(selector=10)]
                },
                SelectorArgKind::Integer,
                false,
                Some(10u32),
            ),
            (
                quote_as_str! {
                    #[ink(selector=0xA)]
                },
                SelectorArgKind::Integer,
                false,
                Some(10u32),
            ),
            (
                quote_as_str! {
                    #[ink(selector=_)]
                },
                SelectorArgKind::Wildcard,
                true,
                None,
            ),
        ] {
            // Parse ink! selector argument.
            let selector_arg = SelectorArg::cast(
                parse_first_ink_attribute(code)
                    .args()
                    .iter()
                    .find(|arg| *arg.kind() == InkArgKind::Selector)
                    .unwrap()
                    .to_owned(),
            )
            .unwrap();

            assert_eq!(*selector_arg.kind(), expected_kind);

            assert_eq!(selector_arg.is_wildcard(), expected_is_wildcard);

            assert_eq!(selector_arg.as_u32(), expected_u32_value);
        }
    }
}
