use ink_primitives::KeyComposer;
use ink_storage::{
    traits::StorageKey,
    Lazy,
    Mapping,
};

#[ink::storage_item]
#[derive(Default)]
struct Contract(Mapping<u128, String>, Lazy<u128>);
