//! ink! entity traits for callables (i.e ink! constructors and ink! messages).

use super::IsInkFn;
use crate::tree::utils;
use crate::{EnvironmentArg, InkArgKind, InkEntity, Selector, SelectorArg};

/// Implemented by ink! entities that represent an ink! callable entity
/// (i.e. an ink! constructor or ink! message).
#[allow(unused_imports)]
pub trait IsInkCallable: InkEntity + IsInkFn {
    impl_ink_arg_getter!(default_arg, Default, default);

    impl_ink_arg_getter!(payable_arg, Payable, payable);

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

/// Implemented by ink! entities that accept an `Environment` configuration
/// (i.e. an ink! contract or ink! e2e test).
pub trait HasInkEnvironment: InkEntity {
    const ENV_ARG_KIND: InkArgKind;

    /// Returns the ink! selector argument (if any).
    fn environment_arg(&self) -> Option<EnvironmentArg> {
        utils::ink_arg_by_kind(self.syntax(), Self::ENV_ARG_KIND).and_then(EnvironmentArg::cast)
    }
}
