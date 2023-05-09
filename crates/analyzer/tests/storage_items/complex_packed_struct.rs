// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/tests/ui/storage_item/pass/complex_packed_struct.rs>.
use ink_prelude::{
    collections::{
        BTreeMap,
        BTreeSet,
    },
    vec::Vec,
};
use ink::storage::traits::Storable;

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, scale::Encode, scale::Decode)]
#[cfg_attr(
    feature = "std",
    derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
struct Deep2 {
    a: u8,
    b: u16,
    c: u32,
    d: u64,
    e: u128,
    f: String,
}

#[derive(Default, scale::Encode, scale::Decode)]
#[cfg_attr(
    feature = "std",
    derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
struct Deep1 {
    a: Deep2,
    b: (Deep2, Deep2),
    c: Vec<Deep2>,
    d: BTreeMap<Deep2, Deep2>,
    e: BTreeSet<Deep2>,
}

#[derive(Default, scale::Encode, scale::Decode)]
#[cfg_attr(
    feature = "std",
    derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
struct Contract {
    a: Deep1,
    b: Deep2,
    c: (Deep1, Deep2),
}
