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
pub fn it_works(mut client: ink_e2e::Client<C, E>) -> std::result::Result<T, Box<dyn std::error::Error>> {
    todo!()
}"#;
pub const INK_E2E_TEST_SNIPPET: &str = r#"#[ink_e2e::test]
pub fn ${1:it_works}(${2:mut client: ink_e2e::Client<C, E>})${3: -> std::result::Result<T, Box<dyn std::error::Error>>} {
    ${4:todo!()}
}"#;
