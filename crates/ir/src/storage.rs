//! ink! storage IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, InkStruct};

/// An ink! storage definition.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Storage {
    /// ink! attribute IR data.
    #[arg_kind(Storage)]
    ink_attr: InkAttrData<Struct>,
}

impl InkStruct for Storage {
    fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quote_as_str;
    use crate::test_utils::*;

    #[test]
    fn cast_works() {
        let ink_attr = parse_first_ink_attribute(quote_as_str! {
            #[ink(storage)]
            pub struct MyContract {}
        });

        let storage = Storage::cast(ink_attr).unwrap();

        // `struct` item exists.
        assert!(storage.struct_item().is_some());
    }
}
