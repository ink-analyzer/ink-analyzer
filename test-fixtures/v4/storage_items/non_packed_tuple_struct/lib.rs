use ink::storage::{Lazy, Mapping};

#[ink::storage_item]
#[derive(Default)]
struct Contract(Mapping<u128, String>, Lazy<u128>);
