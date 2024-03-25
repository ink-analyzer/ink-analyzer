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
enum Packed {
    #[default]
    None,
    A(u8),
    B(u16),
    C(u32),
    D(u64),
    E(u128),
    F(String),
    G {
        a: u8,
        b: String,
    },
    H((u16, u32)),
}

#[ink::storage_item]
#[derive(Default)]
enum NonPacked<KEY: StorageKey = AutoKey> {
    #[default]
    None,
    A(Mapping<u128, Packed>),
    B(Lazy<u128>),
    C(Lazy<Packed>),
    D(Lazy<Vec<Packed>>),
    E(Mapping<String, Packed>),
    F {
        a: Mapping<String, Packed>,
    },
}

#[ink::storage_item]
#[derive(Default)]
struct Contract {
    a: Lazy<NonPacked>,
}
