use ink::storage::traits::{
    ManualKey,
    StorageKey,
};

#[ink::storage_item]
struct Contract<KEY: StorageKey = ManualKey<123>> {
    a: u16,
    b: u16,
    c: u16,
}
