//! ink! scale derive IR.

use ra_ap_syntax::ast;

/// An ink! scale derive.
#[ink_analyzer_macro::entity(macro_kind = ScaleDerive)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScaleDerive {
    // ASTNode type.
    ast: ast::Adt,
}

impl ScaleDerive {
    impl_pub_ast_type_getter!(adt, Adt);

    impl_pub_ink_arg_getter!(decode_arg, Decode, Decode);

    impl_pub_ink_arg_getter!(encode_arg, Encode, Encode);

    impl_pub_ink_arg_getter!(type_info_arg, TypeInfo, TypeInfo);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, derives_encode, derives_decode, derives_type_info) in [
            (
                quote_as_str! {
                    #[ink::scale_derive(Encode, Decode, TypeInfo)]
                    pub struct MyType {}
                },
                true,
                true,
                true,
            ),
            (
                quote_as_str! {
                    #[ink::scale_derive(Encode)]
                    pub struct MyType {}
                },
                true,
                false,
                false,
            ),
            (
                quote_as_str! {
                    #[ink::scale_derive(Decode)]
                    pub struct MyType {}
                },
                false,
                true,
                false,
            ),
            (
                quote_as_str! {
                    #[ink::scale_derive(TypeInfo)]
                    pub struct MyType {}
                },
                false,
                false,
                true,
            ),
        ] {
            let node = parse_first_syntax_node(code);

            let scale_derive = ScaleDerive::cast(node).unwrap();

            // `Encode` argument exists.
            assert_eq!(scale_derive.encode_arg().is_some(), derives_encode);

            // `Decode` argument exists.
            assert_eq!(scale_derive.decode_arg().is_some(), derives_decode);

            // `TypeInfo` argument exists.
            assert_eq!(scale_derive.type_info_arg().is_some(), derives_type_info);

            // ADT item exists.
            assert!(scale_derive.adt().is_some());
        }
    }
}
