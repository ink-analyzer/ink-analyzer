//! ink! extension IR.

use ra_ap_syntax::ast;

use crate::traits::InkEntity;
use crate::InkArg;

/// An ink! extension.
#[ink_analyzer_macro::entity(arg_kind = Extension)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Extension {
    // ASTNode type.
    ast: ast::Fn,
}

impl_ast_type_trait!(Extension, IsInkFn);

impl Extension {
    /// Returns the extension id (if any).
    pub fn id(&self) -> Option<u32> {
        self.extension_arg()?.value()?.as_u32()
    }

    impl_pub_ink_arg_getter!(extension_arg, Extension, extension);

    impl_pub_ink_arg_getter!(handle_status_arg, HandleStatus, handle_status);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::IsInkFn;
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
