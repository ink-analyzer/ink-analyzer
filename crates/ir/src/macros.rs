//! Declarative macros used throughout the crate.

macro_rules! impl_ast_type_getter {
    ($fn_name: ident, $ast_type: ident $(, $vis: vis, $doc: ident)?) => {
        $(#[doc = concat!("Returns the `", stringify!($doc), "` item (if any).")])?
        $($vis)? fn $fn_name(&self) -> Option<&ra_ap_syntax::ast::$ast_type> {
            self.ast.as_ref()
        }
    };
}

macro_rules! impl_pub_ast_type_getter {
    ($fn_name: ident, $ast_type: ident) => {
        impl_ast_type_getter!($fn_name, $ast_type, pub, $ast_type);
    };
}

macro_rules! impl_ast_type_trait {
    ($entity: ty, IsInkStruct) => {
        impl_ast_type_trait_base!($entity, IsInkStruct, Struct, struct_item);
    };
    ($entity: ty, IsInkFn) => {
        impl_ast_type_trait_base!($entity, IsInkFn, Fn, fn_item);
    };
    ($entity: ty, IsInkTrait) => {
        impl_ast_type_trait_base!($entity, IsInkTrait, Trait, trait_item);
    };
}

macro_rules! impl_ast_type_trait_base {
    ($entity: ty, $trait_name: ident, $ast_type: ident, $fn_name: ident) => {
        impl $crate::traits::$trait_name for $entity {
            impl_ast_type_getter!($fn_name, $ast_type);
        }
    };
}

macro_rules! impl_ink_arg_getter {
    ($name: ident, $variant: ident, $doc: ident $(, $vis: vis)?) => {
        #[doc = concat!("Returns the ink! `", stringify!($doc), "` argument (if any).")]
        $($vis)? fn $name(&self) -> Option<$crate::InkArg> {
            use crate::traits::InkEntity;
            $crate::tree::utils::ink_arg_by_kind(self.syntax(), $crate::InkArgKind::$variant)
        }
    };
}

macro_rules! impl_pub_ink_arg_getter {
    ($name: ident, $variant: ident, $doc: ident) => {
        impl_ink_arg_getter!($name, $variant, $doc, pub);
    };
}

macro_rules! impl_is_ink_event {
    ($entity: ty) => {
        impl $crate::traits::IsInkEvent for $entity {
            fn topics(&self) -> &[Topic] {
                &self.topics
            }
        }
    };
}

macro_rules! impl_has_ink_environment {
    ($entity: ty, $variant: ident) => {
        impl $crate::traits::HasInkEnvironment for $entity {
            const ENV_ARG_KIND: $crate::InkArgKind = $crate::InkArgKind::$variant;
        }
    };
}

macro_rules! impl_is_chain_extension_fn {
    ($entity: ty, $variant: ident) => {
        impl $crate::traits::IsChainExtensionFn for $entity {
            const ID_ARG_KIND: $crate::InkArgKind = $crate::InkArgKind::$variant;
        }
    };
}

macro_rules! impl_is_int_id {
    ($entity: ty) => {
        impl $crate::traits::IsIntId for $entity {
            const MAX: Self = <$entity>::MAX;

            fn from_str_radix(src: &str, radix: u32) -> Result<Self, std::num::ParseIntError> {
                <$entity>::from_str_radix(src, radix)
            }
        }
    };
}
