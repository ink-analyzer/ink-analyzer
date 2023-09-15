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

pub const CONSTRUCTOR_PLAIN: &str = r#"#[ink(constructor)]
pub fn new() -> Self {
    todo!()
}"#;
pub const CONSTRUCTOR_SNIPPET: &str = r#"#[ink(constructor)]
pub fn ${1:new}() -> ${2:Self} {
    ${3:todo!()}
}"#;

pub const MESSAGE_PLAIN: &str = r#"#[ink(message)]
pub fn message(&self) {
    todo!()
}"#;
pub const MESSAGE_SNIPPET: &str = r#"#[ink(message)]
pub fn ${1:message}(&${2:self}) {
    ${3:todo!()}
}"#;

pub const TRAIT_MESSAGE_PLAIN: &str = r#"#[ink(message)]
fn message(&self);"#;
pub const TRAIT_MESSAGE_SNIPPET: &str = r#"#[ink(message)]
fn ${1:message}(&${2:self});"#;

pub const ERROR_CODE_PLAIN: &str = r#"type ErrorCode = ();"#;
pub const ERROR_CODE_SNIPPET: &str = r#"type ErrorCode = ${1:()};"#;

pub const EXTENSION_PLAIN: &str = r#"#[ink(extension=1)]
fn extension();"#;
pub const EXTENSION_SNIPPET: &str = r#"#[ink(extension=${1:1})]
fn ${2:extension}();"#;

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
    pub struct Storage {}

    impl MyContract {
        #[ink(constructor)]
        pub fn new() -> Self {
            todo!()
        }

        #[ink(message)]
        pub fn message(&self) {
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
        pub fn ${7:message}(&${8:self}) {
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
    fn message(&self);
}"#;
pub const TRAIT_DEFINITION_SNIPPET: &str = r#"#[ink::trait_definition]
pub trait ${1:TraitDefinition} {
    #[ink(message)]
    fn ${2:message}(&${3:self});
}"#;

pub const CHAIN_EXTENSION_PLAIN: &str = r#"#[ink::chain_extension]
pub trait ChainExtension {
    type ErrorCode = CustomErrorCode;

    #[ink(extension=1)]
    fn extension();
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

    #[ink(extension=${3:1})]
    fn ${4:extension}();
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
