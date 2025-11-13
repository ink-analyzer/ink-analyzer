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

pub const EVENT_PLAIN_V2: &str = r#"#[ink::event]
pub struct Event {}"#;
pub const EVENT_SNIPPET_V2: &str = r#"#[ink::event]
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

pub const CONTRACT_REF_MESSAGE_PLAIN: &str = r#"#[ink(message)]
fn message(&self);"#;
pub const CONTRACT_REF_MESSAGE_SNIPPET: &str = r#"#[ink(message)]
fn ${1:message}(&${2:self});"#;

pub const ERROR_CODE_PLAIN: &str = r#"type ErrorCode = ();"#;
pub const ERROR_CODE_SNIPPET: &str = r#"type ErrorCode = ${1:()};"#;

pub const EXTENSION_FN_PLAIN_V4: &str = r#"#[ink(extension = 1)]
fn my_extension();"#;
pub const EXTENSION_FN_SNIPPET_V4: &str = r#"#[ink(extension = ${1:1})]
fn ${2:my_extension}();"#;

pub const EXTENSION_FN_PLAIN: &str = r#"#[ink(function = 1)]
fn my_function();"#;
pub const EXTENSION_FN_SNIPPET: &str = r#"#[ink(function = ${1:1})]
fn ${2:my_function}();"#;

pub const INK_TEST_PLAIN: &str = r#"#[ink::test]
fn it_works() {
    todo!()
}"#;
pub const INK_TEST_SNIPPET: &str = r#"#[ink::test]
fn ${1:it_works}() {
    ${2:todo!()}
}"#;

pub const INK_E2E_TEST_PLAIN: &str = r#"#[ink_e2e::test]
async fn it_works<Client: E2EBackend>(mut client: Client) -> std::result::Result<(), Box<dyn std::error::Error>> {
    todo!();

    Ok(())
}"#;
pub const INK_E2E_TEST_SNIPPET: &str = r#"#[ink_e2e::test]
async fn ${1:it_works}<${2:Client: E2EBackend}>(${3:mut client: Client}) -> ${4:std::result::Result<(), Box<dyn std::error::Error>>} {
    ${5:todo!();}

    Ok(())
}"#;

pub const INK_E2E_TEST_PLAIN_V4: &str = r#"#[ink_e2e::test]
async fn it_works(mut client: ink_e2e::Client<C, E>) -> std::result::Result<(), Box<dyn std::error::Error>> {
    todo!();

    Ok(())
}"#;
pub const INK_E2E_TEST_SNIPPET_V4: &str = r#"#[ink_e2e::test]
async fn ${1:it_works}(${2:mut client: ink_e2e::Client<C, E>}) -> ${3:std::result::Result<(), Box<dyn std::error::Error>>} {
    ${4:todo!();}

    Ok(())
}"#;

pub const CONTRACT_PLAIN: &str = r#"#![cfg_attr(not(feature = "std"), no_std, no_main)]

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
        fn it_works() {
            todo!()
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        async fn it_works<Client: E2EBackend>(mut client: Client) -> E2EResult<()> {
            todo!();

            Ok(())
        }
    }
}"#;
pub const CONTRACT_SNIPPET: &str = r#"#![cfg_attr(not(feature = "std"), no_std, no_main)]

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
        fn ${10:it_works}() {
            ${11:todo!()}
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        async fn ${12:it_works}<${13:Client: E2EBackend}>(${14:mut client: Client}) -> ${15:E2EResult<()>} {
            ${16:todo!();}

            Ok(())
        }
    }
}"#;

pub const CONTRACT_PLAIN_V5: &str = r#"#![cfg_attr(not(feature = "std"), no_std)]

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
        fn it_works() {
            todo!()
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        async fn it_works<Client: E2EBackend>(mut client: Client) -> E2EResult<()> {
            todo!();

            Ok(())
        }
    }
}"#;
pub const CONTRACT_SNIPPET_V5: &str = r#"#![cfg_attr(not(feature = "std"), no_std)]

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
        fn ${10:it_works}() {
            ${11:todo!()}
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        async fn ${12:it_works}<${13:Client: E2EBackend}>(${14:mut client: Client}) -> ${15:E2EResult<()>} {
            ${16:todo!();}

            Ok(())
        }
    }
}"#;

pub const CONTRACT_PLAIN_V4: &str = r#"#![cfg_attr(not(feature = "std"), no_std)]

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
        fn it_works() {
            todo!()
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        async fn it_works(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {
            todo!();

            Ok(())
        }
    }
}"#;
pub const CONTRACT_SNIPPET_V4: &str = r#"#![cfg_attr(not(feature = "std"), no_std)]

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
        fn ${10:it_works}() {
            ${11:todo!()}
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;

        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        async fn ${12:it_works}(${13:mut client: ink_e2e::Client<C, E>}) -> ${14:E2EResult<()>} {
            ${15:todo!();}

            Ok(())
        }
    }
}"#;

pub const CONTRACT_REF_PLAIN: &str = r#"#[ink::contract_ref]
pub trait Callee {
    #[ink(message)]
    fn message(&self);
}"#;
pub const CONTRACT_REF_SNIPPET: &str = r#"#[ink::contract_ref]
pub trait ${1:Callee} {
    #[ink(message)]
    fn ${2:message}(&${3:self});
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

pub const CHAIN_EXTENSION_PLAIN_V5: &str = r#"#[ink::chain_extension(extension = 1)]
pub trait ChainExtension {
    type ErrorCode = CustomErrorCode;

    #[ink(function = 1)]
    fn my_function();
}

#[ink::scale_derive(Encode, Decode, TypeInfo)]
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
pub const CHAIN_EXTENSION_SNIPPET_V5: &str = r#"#[ink::chain_extension(extension = ${1:1})]
pub trait ${2:ChainExtension} {
    type ErrorCode = ${3:CustomErrorCode};

    #[ink(function = ${4:1})]
    fn ${5:my_function}();
}

#[ink::scale_derive(Encode, Decode, TypeInfo)]
pub enum ${3:CustomErrorCode} {
  ${6:CustomError},
}

impl ink::env::chain_extension::FromStatusCode for ${3:CustomErrorCode} {
  fn from_status_code(${7:status_code}: u32) -> Result<(), Self> {
    match ${7:status_code} {
      0 => Ok(()),
      1 => Err(Self::${6:CustomError}),
      _ => panic!("encountered unknown status code"),
    }
  }
}"#;

pub const CHAIN_EXTENSION_PLAIN_V4: &str = r#"#[ink::chain_extension]
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
pub const CHAIN_EXTENSION_SNIPPET_V4: &str = r#"#[ink::chain_extension]
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

pub const COMBINE_EXTENSIONS_PLAIN: &str = r#"ink::combine_extensions! {
    pub struct CombinedChainExtension {
        pub ext1: Extension1,
        pub ext2: Extension2,
    }
}"#;
pub const COMBINE_EXTENSIONS_SNIPPET: &str = r#"ink::combine_extensions! {
    pub struct ${1:CombinedChainExtension} {
        pub ${2:ext1}: ${3:Extension1},
        pub ${4:ext2}: ${5:Extension2},
    }
}"#;

pub const STORAGE_ITEM_PLAIN: &str = r#"#[ink::storage_item]
pub struct StorageItem {}"#;
pub const STORAGE_ITEM_SNIPPET: &str = r#"#[ink::storage_item]
pub ${1:struct} ${2:StorageItem} {
    $3
}"#;

pub const ENVIRONMENT_PLAIN: &str = r#"#[derive(Debug, Clone, PartialEq, Eq)]
#[ink::scale_derive(TypeInfo)]
pub enum MyEnvironment {}

impl ink::env::Environment for MyEnvironment {
    const NATIVE_TO_ETH_RATIO: u32 = 100_000_000;

    type AccountId = ::ink::primitives::AccountId;
    type Balance = u128;
    type Hash = ::ink::primitives::Hash;
    type Timestamp = u64;
    type BlockNumber = u32;
    type EventRecord = ();
}"#;

pub const ENVIRONMENT_SNIPPET: &str = r#"#[derive(Debug, Clone, PartialEq, Eq)]
#[ink::scale_derive(TypeInfo)]
pub ${1:enum} ${2:MyEnvironment} {
    $3
}

impl ink::env::Environment for ${4:MyEnvironment} {
    const NATIVE_TO_ETH_RATIO: u32 = ${5:100_000_000};

    type AccountId = ${6:::ink::primitives::AccountId};
    type Balance = ${7:u128};
    type Hash = ${8:::ink::primitives::Hash};
    type Timestamp = ${9:u64};
    type BlockNumber = ${10:u32};
    type EventRecord = ${11:()};
}"#;

pub const ENVIRONMENT_IMPL_PLAIN: &str = r#"impl ink::env::Environment for MyEnvironment {
    const NATIVE_TO_ETH_RATIO: u32 = 100_000_000;

    type AccountId = ::ink::primitives::AccountId;
    type Balance = u128;
    type Hash = ::ink::primitives::Hash;
    type Timestamp = u64;
    type BlockNumber = u32;
    type EventRecord = ();
}"#;

pub const ENVIRONMENT_IMPL_SNIPPET: &str = r#"impl ink::env::Environment for ${1:MyEnvironment} {
    const NATIVE_TO_ETH_RATIO: u32 = ${2:100_000_000};

    type AccountId = ${3:::ink::primitives::AccountId};
    type Balance = ${4:u128};
    type Hash = ${5:::ink::primitives::Hash};
    type Timestamp = ${6:u64};
    type BlockNumber = ${7:u32};
    type EventRecord = ${8:()};
}"#;

pub const ENVIRONMENT_PLAIN_V5: &str = r#"#[derive(Debug, Clone, PartialEq, Eq)]
#[ink::scale_derive(TypeInfo)]
pub enum MyEnvironment {}

impl ink::env::Environment for MyEnvironment {
    const MAX_EVENT_TOPICS: usize = 4;

    type AccountId = ::ink::primitives::AccountId;
    type Balance = u128;
    type Hash = ::ink::primitives::Hash;
    type Timestamp = u64;
    type BlockNumber = u32;
    type ChainExtension = ::ink::env::NoChainExtension;
}"#;

pub const ENVIRONMENT_SNIPPET_V5: &str = r#"#[derive(Debug, Clone, PartialEq, Eq)]
#[ink::scale_derive(TypeInfo)]
pub ${1:enum} ${2:MyEnvironment} {
    $3
}

impl ink::env::Environment for ${4:MyEnvironment} {
    const MAX_EVENT_TOPICS: usize = ${5:4};

    type AccountId = ${6:::ink::primitives::AccountId};
    type Balance = ${7:u128};
    type Hash = ${8:::ink::primitives::Hash};
    type Timestamp = ${9:u64};
    type BlockNumber = ${10:u32};
    type ChainExtension = ${11:::ink::env::NoChainExtension};
}"#;

pub const ENVIRONMENT_PLAIN_V4: &str = r#"#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub enum MyEnvironment {}

impl ink::env::Environment for MyEnvironment {
    const MAX_EVENT_TOPICS: usize = 4;

    type AccountId = ::ink::primitives::AccountId;
    type Balance = u128;
    type Hash = ::ink::primitives::Hash;
    type Timestamp = u64;
    type BlockNumber = u32;
    type ChainExtension = ::ink::env::NoChainExtension;
}"#;

pub const ENVIRONMENT_SNIPPET_V4: &str = r#"#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub ${1:enum} ${2:MyEnvironment} {
    $3
}

impl ink::env::Environment for ${4:MyEnvironment} {
    const MAX_EVENT_TOPICS: usize = ${5:4};

    type AccountId = ${6:::ink::primitives::AccountId};
    type Balance = ${7:u128};
    type Hash = ${8:::ink::primitives::Hash};
    type Timestamp = ${9:u64};
    type BlockNumber = ${10:u32};
    type ChainExtension = ${11:::ink::env::NoChainExtension};
}"#;

pub const ENVIRONMENT_IMPL_PLAIN_LTE_V5: &str = r#"impl ink::env::Environment for MyEnvironment {
    const MAX_EVENT_TOPICS: usize = 4;

    type AccountId = ::ink::primitives::AccountId;
    type Balance = u128;
    type Hash = ::ink::primitives::Hash;
    type Timestamp = u64;
    type BlockNumber = u32;
    type ChainExtension = ::ink::env::NoChainExtension;
}"#;

pub const ENVIRONMENT_IMPL_SNIPPET_LTE_V5: &str = r#"impl ink::env::Environment for ${1:MyEnvironment} {
    const MAX_EVENT_TOPICS: usize = ${2:4};

    type AccountId = ${3:::ink::primitives::AccountId};
    type Balance = ${4:u128};
    type Hash = ${5:::ink::primitives::Hash};
    type Timestamp = ${6:u64};
    type BlockNumber = ${7:u32};
    type ChainExtension = ${8:::ink::env::NoChainExtension};
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

pub const ERROR_ENUM_PLAIN: &str = r#"#[ink::error]
pub enum Error {}"#;
pub const ERROR_ENUM_SNIPPET: &str = r#"#[ink::error]
pub ${1:enum} ${2:Error} {
    $3
}"#;

pub const ERROR_STRUCT_PLAIN: &str = r#"#[ink::error]
pub struct Error {}"#;
pub const ERROR_STRUCT_SNIPPET: &str = r#"#[ink::error]
pub ${1:struct} ${2:Error} {
    $3
}"#;

pub const CARGO_TOML_PLAIN: &str = r#"[package]
name = "my_contract"
version = "0.1.0"
authors = ["[your_name] <[your_email]>"]
edition = "2024"

[dependencies]
ink = { version = "6.0.0-beta.1", default-features = false, features = ["unstable-hostfn"] }

[dev-dependencies]
ink_e2e = "6.0.0-beta.1"

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
e2e-tests = []

[package.metadata.ink-lang]
abi = "ink"

[lints.rust.unexpected_cfgs]
level = "warn"
check-cfg = [
    'cfg(ink_abi, values("ink", "sol", "all"))'
]"#;
pub const CARGO_TOML_SNIPPET: &str = r#"[package]
name = "${1:my_contract}"
version = "0.1.0"
authors = ["${2:[your_name]} <${3:[your_email]}>"]
edition = "2024"

[dependencies]
ink = { version = "6.0.0-beta.1", default-features = false, features = ["unstable-hostfn"] }

[dev-dependencies]
ink_e2e = "6.0.0-beta.1"

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
e2e-tests = []

[package.metadata.ink-lang]
abi = "${4:ink}"

[lints.rust.unexpected_cfgs]
level = "warn"
check-cfg = [
    'cfg(ink_abi, values("ink", "sol", "all"))'
]"#;

pub const CARGO_TOML_PLAIN_V5: &str = r#"[package]
name = "my_contract"
version = "0.1.0"
authors = ["[your_name] <[your_email]>"]
edition = "2021"

[dependencies]
ink = { version = "5.1.1", default-features = false }

[dev-dependencies]
ink_e2e = { version = "5.1.1" }

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
]
ink-as-dependency = []
e2e-tests = []"#;
pub const CARGO_TOML_SNIPPET_V5: &str = r#"[package]
name = "${1:my_contract}"
version = "0.1.0"
authors = ["${2:[your_name]} <${3:[your_email]}>"]
edition = "2021"

[dependencies]
ink = { version = "5.1.1", default-features = false }

[dev-dependencies]
ink_e2e = { version = "5.1.1" }

[lib]
path = "lib.rs"

[features]
default = ["std"]
std = [
    "ink/std",
]
ink-as-dependency = []
e2e-tests = []"#;

pub const CARGO_TOML_PLAIN_V4: &str = r#"[package]
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

pub const CARGO_TOML_SNIPPET_V4: &str = r#"[package]
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
