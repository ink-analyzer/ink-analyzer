//! ink! entity snippets.

pub const STORAGE_PLAIN: &str = r#"
#[ink(storage)]
pub struct Storage {}

"#;
pub const STORAGE_SNIPPET: &str = r#"
#[ink(storage)]
pub struct ${1:Storage} {
    $2
}

"#;

pub const CONSTRUCTOR_PLAIN: &str = r#"
#[ink(constructor)]
pub fn new() -> Self {
    todo!()
}

"#;
pub const CONSTRUCTOR_SNIPPET: &str = r#"
#[ink(constructor)]
pub fn ${1:new}() -> ${2:Self} {
    ${3:todo!()}
}

"#;

pub const MESSAGE_PLAIN: &str = r#"
#[ink(message)]
pub fn message(&self) {
    todo!()
}

"#;
pub const MESSAGE_SNIPPET: &str = r#"
#[ink(message)]
pub fn ${1:message}(&${2:self}) {
    ${3:todo!()}
}

"#;

pub const TRAIT_MESSAGE_PLAIN: &str = r#"
#[ink(message)]
fn message(&self);

"#;
pub const TRAIT_MESSAGE_SNIPPET: &str = r#"
#[ink(message)]
fn ${1:message}(&${2:self});

"#;

pub const ERROR_CODE_PLAIN: &str = r#"
type ErrorCode = ();

"#;
pub const ERROR_CODE_SNIPPET: &str = r#"
type ErrorCode = ${1:()};

"#;
