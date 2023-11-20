//! ink! entity traits for callables (i.e ink! constructors and ink! messages).

use super::IsInkFn;
use crate::tree::utils;
use crate::{InkArg, InkArgKind, InkEntity, Selector, SelectorArg};

/// Implemented by ink! entities that represent an ink! callable entity (i.e an ink! constructor or ink! message).
pub trait IsInkCallable: InkEntity + IsInkFn {
    /// Returns the ink! payable argument (if any) for the ink! callable entity.
    fn payable_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Payable)
    }

    /// Returns the ink! selector argument (if any) for the ink! callable entity.
    fn selector_arg(&self) -> Option<SelectorArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Selector).and_then(SelectorArg::cast)
    }

    /// Returns the ink! default argument (if any) for the ink! callable entity.
    fn default_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Default)
    }

    /// Returns the composed selector for the ink! callable entity.
    fn composed_selector(&self) -> Option<Selector>
    where
        Self: Sized,
    {
        Selector::compose(self)
    }
}
