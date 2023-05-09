// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/tests/ui/storage_item/pass/default_storage_key_1.rs>.
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
