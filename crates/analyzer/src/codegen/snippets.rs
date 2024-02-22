//! ink! entity snippets.

pub const STORAGE_PLAIN: &str = r#"#[ink(storage)]
pub struct Storage {}"#;
pub const STORAGE_SNIPPET: &str = r#"#[ink(storage)]
pub struct ${1:Storage} {
    $2
}"#;

pub const EVENT_PLAIN: &str = r#"#[ink(event)]
pub struct Event {}"#;
pub const EVENT_SNIPPET: &str = r#"#[ink(event)]
pub struct ${1:Event} {
    $2
}"#;

pub const TOPIC_PLAIN: &str = r#"#[ink(topic)]
my_topic: bool,"#;
pub const TOPIC_SNIPPET: &str = r#"#[ink(topic)]
${1:my_topic}: ${2:bool},"#;

pub const CONSTRUCTOR_PLAIN: &str = r#"#[ink(constructor)]
pub fn new() -> Self {
    todo!()
}"#;
pub const CONSTRUCTOR_SNIPPET: &str = r#"#[ink(constructor)]
pub fn ${1:new}() -> ${2:Self} {
    ${3:todo!()}
}"#;

pub const MESSAGE_PLAIN: &str = r#"#[ink(message)]
pub fn my_message(&self) {
    todo!()
}"#;
pub const MESSAGE_SNIPPET: &str = r#"#[ink(message)]
pub fn ${1:my_message}(&${2:self}) {
    ${3:todo!()}
}"#;

pub const TRAIT_MESSAGE_PLAIN: &str = r#"#[ink(message)]
fn my_message(&self);"#;
pub const TRAIT_MESSAGE_SNIPPET: &str = r#"#[ink(message)]
fn ${1:my_message}(&${2:self});"#;

pub const ERROR_CODE_PLAIN: &str = r#"type ErrorCode = ();"#;
pub const ERROR_CODE_SNIPPET: &str = r#"type ErrorCode = ${1:()};"#;

pub const EXTENSION_PLAIN: &str = r#"#[ink(extension = 1)]
fn my_extension();"#;
pub const EXTENSION_SNIPPET: &str = r#"#[ink(extension = ${1:1})]
fn ${2:my_extension}();"#;

pub const INK_TEST_PLAIN: &str = r#"#[ink::test]
pub fn it_works() {
    todo!()
}"#;
pub const INK_TEST_SNIPPET: &str = r#"#[ink::test]
pub fn ${1:it_works}() {
    ${2:todo!()}
}"#;

pub const INK_E2E_TEST_PLAIN: &str = r#"#[ink_e2e::test]
pub fn it_works(mut client: ink_e2e::Client<C, E>) -> std::result::Result<(), Box<dyn std::error::Error>> {
    todo!();

    Ok(())
}"#;
pub const INK_E2E_TEST_SNIPPET: &str = r#"#[ink_e2e::test]
pub fn ${1:it_works}(${2:mut client: ink_e2e::Client<C, E>})${3: -> std::result::Result<(), Box<dyn std::error::Error>>} {
    ${4:todo!();}

    Ok(())
}"#;

pub const CONTRACT_PLAIN: &str = r#"#![cfg_attr(not(feature = "std"), no_std)]

#[ink::contract]
pub mod my_contract {
    #[ink(storage)]
    pub struct MyContract {}

    impl MyContract {
        #[ink(constructor)]
        pub fn new() -> Self {
            todo!()
        }

        #[ink(message)]
        pub fn my_message(&self) {
            todo!()
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[ink::test]
        pub fn it_works() {
            todo!()
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        pub fn it_works(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {
            todo!();

            Ok(())
        }
    }
}"#;
pub const CONTRACT_SNIPPET: &str = r#"#![cfg_attr(not(feature = "std"), no_std)]

#[ink::contract]
pub mod ${1:my_contract} {
    #[ink(storage)]
    pub struct ${2:MyContract} {
        $3
    }

    impl ${2:MyContract} {
        #[ink(constructor)]
        pub fn ${4:new}() -> ${5:Self} {
            ${6:todo!()}
        }

        #[ink(message)]
        pub fn ${7:my_message}(&${8:self}) {
            ${9:todo!()}
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[ink::test]
        pub fn ${10:it_works}() {
            ${11:todo!()}
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        pub fn ${12:it_works}(${13:mut client: ink_e2e::Client<C, E>})${14: -> E2EResult<()>} {
            ${15:todo!();}

            Ok(())
        }
    }
}"#;

pub const TRAIT_DEFINITION_PLAIN: &str = r#"#[ink::trait_definition]
pub trait TraitDefinition {
    #[ink(message)]
    fn my_message(&self);
}"#;
pub const TRAIT_DEFINITION_SNIPPET: &str = r#"#[ink::trait_definition]
pub trait ${1:TraitDefinition} {
    #[ink(message)]
    fn ${2:my_message}(&${3:self});
}"#;

pub const CHAIN_EXTENSION_PLAIN: &str = r#"#[ink::chain_extension]
pub trait ChainExtension {
    type ErrorCode = CustomErrorCode;

    #[ink(extension = 1)]
    fn my_extension();
}

#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
pub enum CustomErrorCode {
  CustomError,
}

impl ink::env::chain_extension::FromStatusCode for CustomErrorCode {
  fn from_status_code(status_code: u32) -> Result<(), Self> {
    match status_code {
      0 => Ok(()),
      1 => Err(Self::CustomError),
      _ => panic!("encountered unknown status code"),
    }
  }
}"#;
pub const CHAIN_EXTENSION_SNIPPET: &str = r#"#[ink::chain_extension]
pub trait ${1:ChainExtension} {
    type ErrorCode = ${2:CustomErrorCode};

    #[ink(extension = ${3:1})]
    fn ${4:my_extension}();
}

#[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
pub enum ${2:CustomErrorCode} {
  ${5:CustomError},
}

impl ink::env::chain_extension::FromStatusCode for ${2:CustomErrorCode} {
  fn from_status_code(${6:status_code}: u32) -> Result<(), Self> {
    match ${6:status_code} {
      0 => Ok(()),
      1 => Err(Self::${5:CustomError}),
      _ => panic!("encountered unknown status code"),
    }
  }
}"#;

pub const STORAGE_ITEM_PLAIN: &str = r#"#[ink::storage_item]
pub struct StorageItem {}"#;
pub const STORAGE_ITEM_SNIPPET: &str = r#"#[ink::storage_item]
pub ${1:struct} ${2:StorageItem} {
    $3
}"#;

pub const ENVIRONMENT_DEF: &str = r#"#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub enum MyEnvironment {}"#;

pub const ENVIRONMENT_IMPL_PLAIN: &str = r#"impl ink::env::Environment for MyEnvironment {
    const MAX_EVENT_TOPICS: usize = 4;

    type AccountId = ::ink::primitives::AccountId;
    type Balance = u128;
    type Hash = ::ink::primitives::Hash;
    type Timestamp = u64;
    type BlockNumber = u32;
    type ChainExtension = ::ink::env::NoChainExtension;
}"#;

pub const ENVIRONMENT_IMPL_SNIPPET: &str = r#"impl ink::env::Environment for MyEnvironment {
    const MAX_EVENT_TOPICS: usize = ${1:4};

    type AccountId = ${2:::ink::primitives::AccountId};
    type Balance = ${3:u128};
    type Hash = ${4:::ink::primitives::Hash};
    type Timestamp = ${5:u64};
    type BlockNumber = ${6:u32};
    type ChainExtension = ${7:::ink::env::NoChainExtension};
}"#;

pub const FROM_STATUS_CODE_IMPL_PLAIN: &str = r#"impl ink::env::chain_extension::FromStatusCode for MyErrorCode {
    fn from_status_code(status_code: u32) -> Result<(), Self> {
        todo!()
    }
}"#;

pub const FROM_STATUS_CODE_IMPL_SNIPPET: &str = r#"impl ink::env::chain_extension::FromStatusCode for MyErrorCode {
    fn from_status_code(status_code: u32) -> Result<(), Self> {
        ${1:todo!()}
    }
}"#;

pub const CARGO_TOML_PLAIN: &str = r#"[package]
name = "my_contract"
version = "0.1.0"
authors = ["[your_name] <[your_email]>"]
edition = "2021"

[dependencies]
ink = { version = "4.3.0", default-features = false }

scale = { package = "parity-scale-codec", version = "3", default-features = false, features = ["derive"] }
scale-info = { version = "2.6", default-features = false, features = ["derive"], optional = true }

[dev-dependencies]
ink_e2e = "4.3.0"

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
    "scale/std",
    "scale-info/std",
]
ink-as-dependency = []
e2e-tests = []"#;
pub const CARGO_TOML_SNIPPET: &str = r#"[package]
name = "${1:my_contract}"
version = "0.1.0"
authors = ["${2:[your_name]} <${3:[your_email]}>"]
edition = "2021"

[dependencies]
ink = { version = "4.3.0", default-features = false }

scale = { package = "parity-scale-codec", version = "3", default-features = false, features = ["derive"] }
scale-info = { version = "2.6", default-features = false, features = ["derive"], optional = true }

[dev-dependencies]
ink_e2e = "4.3.0"

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
    "scale/std",
    "scale-info/std",
]
ink-as-dependency = []
e2e-tests = []"#;

pub const CARGO_TOML_PLAIN_V5: &str = r#"[package]
name = "my_contract"
version = "0.1.0"
authors = ["[your_name] <[your_email]>"]
edition = "2021"

[dependencies]
ink = { version = "5.0.0-rc.1", default-features = false }

scale = { package = "parity-scale-codec", version = "3", default-features = false, features = ["derive"] }
scale-info = { version = "2.6", default-features = false, features = ["derive"], optional = true }

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
    "scale/std",
    "scale-info/std",
]
ink-as-dependency = []
e2e-tests = []"#;
pub const CARGO_TOML_SNIPPET_V5: &str = r#"[package]
name = "${1:my_contract}"
version = "0.1.0"
authors = ["${2:[your_name]} <${3:[your_email]}>"]
edition = "2021"

[dependencies]
ink = { version = "5.0.0-rc.1", default-features = false }

scale = { package = "parity-scale-codec", version = "3", default-features = false, features = ["derive"] }
scale-info = { version = "2.6", default-features = false, features = ["derive"], optional = true }

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
    "scale/std",
    "scale-info/std",
]
ink-as-dependency = []
e2e-tests = []"#;
