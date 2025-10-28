//! ink! entity traits for callables (i.e. ink! constructors and ink! messages).

use super::{IsInkFn, IsInkStruct};
use crate::tree::{ast_ext, utils};
use crate::{EnvArg, Environment, InkArg, InkArgKind, InkEntity, Selector, SelectorArg, Topic};

/// Implemented by ink! entities that represent an ink! callable entity
/// (i.e. an ink! constructor or ink! message).
#[allow(unused_imports)]
pub trait IsInkCallable: IsInkFn {
    impl_ink_arg_getter!(default_arg, Default, default);

    impl_ink_arg_getter!(payable_arg, Payable, payable);

    impl_ink_arg_getter!(name_arg, Name, name);

    /// Returns the ink! selector argument (if any).
    fn selector_arg(&self) -> Option<SelectorArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Selector).and_then(SelectorArg::cast)
    }

    /// Returns the composed selector (if any).
    fn composed_selector(&self) -> Option<Selector>
    where
        Self: Sized,
    {
        Selector::compose(self)
    }
}

/// Convenience trait for unified handling of both macro (i.e. `#[ink::event]` in v5) and
/// argument (i.e. `#[ink(event)]`) based ink! events.
#[allow(unused_imports)]
pub trait IsInkEvent: IsInkStruct {
    impl_ink_arg_getter!(anonymous_arg, Anonymous, anonymous);

    impl_ink_arg_getter!(signature_arg, SignatureTopic, signature_topic);

    /// Returns ink! topics.
    fn topics(&self) -> &[Topic];
}

/// Implemented by ink! entities that accept an `Environment` configuration
/// (i.e. an ink! contract or ink! e2e test).
pub trait HasInkEnvironment: InkEntity {
    const ENV_ARG_KIND: InkArgKind;

    /// Returns the ink! environment argument (if any).
    fn env_arg(&self) -> Option<EnvArg> {
        utils::ink_arg_by_kind(self.syntax(), Self::ENV_ARG_KIND).and_then(EnvArg::cast)
    }

    /// Returns the ink! environment (if any).
    fn environment(&self) -> Option<Environment> {
        self.env_arg()
            .as_ref()
            .and_then(EnvArg::as_path_with_inaccurate_text_range)
            .and_then(|path| ast_ext::resolve_item(&path, self.syntax()))
            .map(Environment::new)
    }
}

/// Convenience trait for unified handling by ink! entities that represent an associated function
/// of a chain extension (i.e. an ink! extension for v4 or ink! function in v5).
#[allow(unused_imports)]
pub trait IsChainExtensionFn: IsInkFn {
    const ID_ARG_KIND: InkArgKind;

    /// Returns the id of the chain extension function (if any).
    fn id<T>(&self) -> Option<T>
    where
        T: IsIntId,
    {
        self.id_arg()?.value()?.as_int()
    }

    /// Returns the chain extension function's id argument (if any)
    /// (i.e. `function` for ink! v5 and `extension` for ink! v4).
    fn id_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), Self::ID_ARG_KIND)
    }

    impl_ink_arg_getter!(handle_status_arg, HandleStatus, handle_status);
}

/// Convenience trait for unified handling of ink! integer ids.
pub trait IsIntId:
    std::ops::Add
    + std::ops::AddAssign
    + Eq
    + std::hash::Hash
    + std::str::FromStr
    + std::fmt::Display
    + From<u8>
    + Copy
    + Sized
{
    /// The largest value that can be represented by this integer id type.
    const MAX: Self;

    /// Converts a string slice in a given base to an integer.
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, std::num::ParseIntError>;
}

impl_is_int_id!(u16);
impl_is_int_id!(u32);
