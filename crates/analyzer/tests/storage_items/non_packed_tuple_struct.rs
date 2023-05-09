// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/tests/ui/storage_item/pass/non_packed_tuple_struct.rs>.
use ink_primitives::KeyComposer;
use ink_storage::{
    traits::StorageKey,
    Lazy,
    Mapping,
};

#[ink::storage_item]
#[derive(Default)]
struct Contract(Mapping<u128, String>, Lazy<u128>);
