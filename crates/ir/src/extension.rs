//! ink! extension IR.

use ra_ap_syntax::ast;

use crate::traits::{InkEntity, IsInkFn};
use crate::tree::utils;
use crate::{InkArg, InkArgKind};

/// An ink! extension.
#[ink_analyzer_macro::entity(arg_kind = Extension)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Extension {
    // ASTNode type.
    ast: ast::Fn,
}

impl IsInkFn for Extension {
    fn fn_item(&self) -> Option<&ast::Fn> {
        self.ast.as_ref()
    }
}

impl Extension {
    /// Returns the id (if any) of the ink! extension.
    pub fn id(&self) -> Option<u32> {
        self.extension_arg()?.value()?.as_u32()
    }

    /// Returns the ink! extension argument (if any) for the ink! extension.
    pub fn extension_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Extension)
    }

    /// Returns the ink! `handle_status` argument (if any) for the ink! extension.
    pub fn handle_status_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::HandleStatus)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, has_handle_status) in [
            (
                quote_as_str! {
                    #[ink(extension=1)]
                    fn my_extension();
                },
                false,
            ),
            (
                quote_as_str! {
                    #[ink(extension=0x1)]
                    fn my_extension();
                },
                false,
            ),
            (
                quote_as_str! {
                    #[ink(extension=1, handle_status=false)]
                    fn my_extension();
                },
                true,
            ),
            (
                quote_as_str! {
                    #[ink(extension=1)]
                    #[ink(handle_status=true)]
                    fn my_extension();
                },
                true,
            ),
        ] {
            let node = parse_first_syntax_node(code);

            let extension = Extension::cast(node).unwrap();

            // `extension_arg` argument exists.
            assert!(extension.extension_arg().is_some());

            // `handle_status` argument exists.
            assert_eq!(extension.handle_status_arg().is_some(), has_handle_status);

            // `fn` item exists.
            assert!(extension.fn_item().is_some());
        }
    }
}
