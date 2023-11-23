//! ink! entity traits for callables (i.e ink! constructors and ink! messages).

use super::IsInkFn;
use crate::tree::utils;
use crate::{InkArg, InkArgKind, InkEntity, Selector, SelectorArg};

/// Implemented by ink! entities that represent an ink! callable entity (i.e an ink! constructor or ink! message).
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
