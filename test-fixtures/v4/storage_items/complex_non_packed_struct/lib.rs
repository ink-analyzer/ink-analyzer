use ink::prelude::vec::Vec;
use ink::primitives::KeyComposer;
use ink::storage::{
    traits::{
        AutoKey,
        StorageKey,
    },
    Lazy,
    Mapping,
};

#[derive(Default, scale::Encode, scale::Decode)]
#[cfg_attr(
    feature = "std",
    derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
struct Packed {
    a: u8,
    b: u16,
    c: u32,
    d: u64,
    e: u128,
    f: String,
}

#[ink::storage_item]
#[derive(Default)]
struct NonPacked<KEY: StorageKey = AutoKey> {
    a: Mapping<u128, Packed>,
    b: Lazy<u128>,
    c: Lazy<Packed>,
    d: Lazy<Vec<Packed>>,
}

#[ink::storage_item]
#[derive(Default)]
struct Contract {
    a: Lazy<NonPacked>,
    b: Mapping<u128, Packed>,
    c: (Packed, Packed),
}
