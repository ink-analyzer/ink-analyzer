//! Hover content for ink! attribute arguments.

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L29-L30>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L41-L45>.
///
/// Ref: <https://paritytech.github.io/ink/ink_e2e_macro/attr.test.html>.
pub const ABI_DOC: &str = r#"
# Attribute

`#[ink::contract_ref(abi = S: string)]`

# Description

Specifies the ABI (Application Binary Interface) of the "callee" contract.

# Usage

Applicable to ink! contract references.

# Example

```
#[ink::contract_ref(abi = "sol")]
pub trait Callee {
    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L29-L30>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L41-L45>.
///
/// Ref: <https://paritytech.github.io/ink/ink_e2e_macro/attr.test.html>.
pub const ADDITIONAL_CONTRACTS_DOC_DEPRECATED: &str = r#"
ink! attribute argument `additional_contracts` is deprecated. See https://github.com/paritytech/ink/pull/2098 for details.
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L29-L30>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L41-L45>.
///
/// Ref: <https://paritytech.github.io/ink/ink_e2e_macro/attr.test.html>.
pub const ADDITIONAL_CONTRACTS_DOC_V4: &str = r#"
# Attribute

`#[ink_e2e::test(additional_contracts = S: string)]`

# Description

Tells the ink! e2e test runner which additional contracts to build before executing the test.

# Usage

Additional contracts that have to be built before executing the test.

# Example

```
#[ink_e2e::test(additional_contracts = "adder/Cargo.toml flipper/Cargo.toml")]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v5.0.0-rc.1#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L656-L692>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.event.html>.
pub const ANONYMOUS_DOC: &str = r#"
# Attribute

`#[ink::event(anonymous)]` or `#[ink(anonymous)]`

# Description

Tells the ink! codegen to treat the ink! event as anonymous which omits the event signature as topic upon emitting.

Very similar to anonymous events in Solidity.

# Usage

Applicable to ink! events.

# Example
```
#[ink::event(anonymous)]
pub struct MyEvent {
    value: bool,
}
```

OR

```
#[ink::contract]
mod my_contract {
    #[ink(event, anonymous)]
    pub struct MyEvent {
        value: bool,
    }

    // --snip--
}
```

OR

```
#[ink::contract]
mod my_contract {
    #[ink(event)]
    #[ink(anonymous)]
    pub struct MyEvent {
        value: bool,
    }

    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const ANONYMOUS_DOC_V4: &str = r#"
# Attribute

`#[ink(anonymous)]`

# Description

Tells the ink! codegen to treat the ink! event as anonymous which omits the event signature as topic upon emitting.

Very similar to anonymous events in Solidity.

# Usage

Applicable to ink! events.

# Example
```
#[ink::contract]
mod my_contract {
    #[ink(event, anonymous)]
    pub struct MyEvent {
        value: bool,
    }

    // --snip--
}
```

OR

```
#[ink::contract]
mod my_contract {
    #[ink(event)]
    #[ink(anonymous)]
    pub struct MyEvent {
        value: bool,
    }

    // --snip--
}
```
"#;

/// Ref: <https://github.com/use-ink/ink/releases/tag/v5.1.0>.
///
/// Ref: <https://github.com/use-ink/ink/blob/v5.1.0/crates/e2e/macro/src/config.rs#L95-L97>.
///
/// Ref: <https://github.com/use-ink/ink/pull/2158>.
pub const BACKEND_DOC: &str = r#"
# Attribute

`#[ink_e2e::test(backend(node|runtime_only))]` or `#[ink_e2e::test(backend(node(url = U: string))]` or `#[ink_e2e::test(backend(runtime_only(sandbox = S: impl ink_sandbox::Sandbox))]`

# Description

Tells the ink! e2e test runner which type of architecture to use to execute the test.

- node: Tells the ink! e2e test runner to use the standard approach of running dedicated single-node blockchain in a background process to execute the test.
- runtime_only: Tells the ink! e2e test runner to use the lightweight approach of skipping the node layer by running a runtime emulator within `TestExternalities` in the same process as the test.

In the case of `#[ink_e2e::test(backend(node))]`, a fresh node instance will be spawned for the lifetime of the test.

In the case of `#[ink_e2e::test(backend(node(url = U: string))]`, the test will run against an already running node at the supplied URL.

In the case of `#[ink_e2e::test(backend(runtime_only))]`, the `ink_e2e::DefaultSandbox` runtime is used.

In the case of `#[ink_e2e::test(backend(runtime_only(sandbox = S: impl ink_sandbox::Sandbox))]`, the runtime must implement the `ink_sandbox::Sandbox` trait.

# Usage

Additional argument for ink! e2e test attribute macro.

**Default value:** `node`.

# Example

```
#[ink_e2e::test(backend(node)]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```

OR

```
#[ink_e2e::test(backend(node(url = "ws://127.0.0.1:8000")]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```

OR

```
#[ink_e2e::test(backend(runtime_only)]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```

OR

```
#[ink_e2e::test(backend(runtime_only(sandbox = ink_e2e::DefaultSandbox))]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/e2e/macro/src/config.rs#L94-L96>.
pub const BACKEND_DOC_V5_0: &str = r#"
# Attribute

`#[ink_e2e::test(backend(node|runtime_only))]` or `#[ink_e2e::test(backend(node(url = U: string))]` or `#[ink_e2e::test(backend(runtime_only(sandbox = S: impl drink::Sandbox))]`

# Description

Tells the ink! e2e test runner which type of architecture to use to execute the test.

- node: Tells the ink! e2e test runner to use the standard approach of running dedicated single-node blockchain in a background process to execute the test.
- runtime_only: Tells the ink! e2e test runner to use the lightweight approach of skipping the node layer by running a runtime emulator within `TestExternalities` (using drink! library) in the same process as the test.

In the case of `#[ink_e2e::test(backend(node))]`, a fresh node instance will be spawned for the lifetime of the test.

In the case of `#[ink_e2e::test(backend(node(url = U: string))]`, the test will run against an already running node at the supplied URL.

In the case of `#[ink_e2e::test(backend(runtime_only))]`, the `ink_e2e::MinimalSandbox` runtime (which is a re-export of `drink::MinimalRuntime`) is used.

In the case of `#[ink_e2e::test(backend(runtime_only(sandbox = S: impl drink::Sandbox))]`, the runtime must implement the `drink::Sandbox` trait.

# Usage

Additional argument for ink! e2e test attribute macro.

**Default value:** `node`.

# Example

```
#[ink_e2e::test(backend(node)]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```

OR

```
#[ink_e2e::test(backend(node(url = "ws://127.0.0.1:8000")]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```

OR

```
#[ink_e2e::test(backend(runtime_only)]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```

OR

```
#[ink_e2e::test(backend(runtime_only(sandbox = ink_e2e::MinimalSandbox))]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L235-L263>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const CONSTRUCTOR_DOC: &str = r#"
# Attribute

`#[ink(constructor)]`

# Description

Flags a function for the ink! storage `struct` as a constructor making it available to the API for instantiating the contract.

# Usage

Applicable to functions.

# Example

```
#[ink::contract]
mod my_contract {
    // --snip--
    impl MyContract {
        #[ink(constructor)]
        pub fn new(initial_value: bool) -> Self {
            MyContract { value: false }
        }
        // --snip--
    }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L1598-L1625>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.scale_derive.html>.
pub const DECODE_DOC: &str = r#"
# Attribute

`#[ink::scale_derive(Decode)]`

# Description

Derive the re-exported `ink::scale::Decode` trait.
It enables using the built in derive macro for the `ink::scale::Decode` trait
without depending directly on the `parity-scale-codec` crate.

# Usage

Applicable to ink! scale derive attribute macro.

# Example

```
#[ink::scale_derive(Decode)]
pub enum Error {}
```

This is a convenience macro that expands to include the additional `crate` attributes
required for the path of the re-exported crates.

```
#[derive(::ink::scale::Decode)]
#[codec(crate = ::ink::scale)]
pub enum Error {}
```
"#;

/// Ref: <https://github.com/paritytech/ink/issues/1703>.
pub const DEFAULT_DOC: &str = r#"
# Attribute

`#[ink(default)]`

# Description

Tells UI to treat the ink! message or ink! constructor as the default choice in selection widgets (e.g dropdowns).

It can be used exactly once for a constructor and once for a message.

# Usage

Applicable to ink! messages and ink! constructors.

# Example

```
#[ink::contract]
mod my_contract {
    // --snip--

    impl MyContract {
        // --snip--

        /// Update the current value.
        #[ink(message)]
        #[ink(default)] // You can either specify default out-of-line.
        pub fn set(&mut self) {
            self.value = !self.value;
        }
    }
}
```

OR

```
#[ink::contract]
mod my_contract {
    // --snip--

    impl MyContract {
        // --snip--

        /// Update the current value.
        #[ink(message, default)] // ...or specify default inline.
        pub fn set(&mut self) {
            self.value = !self.value;
        }
    }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L777-L799>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.storage_item.html>.
pub const DERIVE_DOC: &str = r#"
# Attribute

`#[ink::storage_item(derive = flag: bool)]`

# Description

A configuration parameter used to enable/disable auto deriving of all required storage traits.

# Usage

Additional argument for ink! storage item attribute macro.

**Default value:** `true`.

# Example

```
use ink::storage::Mapping;
use ink::storage::traits::{
    StorableHint,
    StorageKey,
    Storable,
};

#[ink::storage_item(derive = false)]
#[derive(StorableHint, Storable, StorageKey)]
struct NonPackedGeneric<T: ink::storage::traits::Packed> {
    s1: u32,
    s2: Mapping<u128, T>,
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L1598-L1625>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.scale_derive.html>.
pub const ENCODE_DOC: &str = r#"
# Attribute

`#[ink::scale_derive(Encode)]`

# Description

Derive the re-exported `ink::scale::Encode` trait.
It enables using the built in derive macros for the `ink::scale::Encode` trait
without depending directly on the `parity-scale-codec` crate.

# Usage

Applicable to ink! scale derive attribute macro.

# Example

```
#[ink::scale_derive(Encode)]
pub enum Error {}
```

This is a convenience macro that expands to include the additional `crate` attributes
required for the path of the re-exported crates.

```
#[derive(::ink::scale::Encode)]
#[codec(crate = ::ink::scale)]
pub enum Error {}
```
"#;

/// # References
///
/// - <https://github.com/use-ink/ink/blob/v6.0.0-alpha.4/crates/ink/macro/src/lib.rs#L151-L207>.
/// - <https://use-ink.github.io/ink/ink/attr.contract.html>.
/// - <https://github.com/use-ink/ink/blob/v6.0.0-alpha.4/crates/e2e/macro/src/config.rs#L89-L96>.
/// - <https://github.com/use-ink/ink/blob/v6.0.0-alpha.4/crates/e2e/macro/src/lib.rs#L41-L53>.
/// - <https://use-ink.github.io/ink/ink_e2e_macro/attr.test.html>.
pub const ENV_DOC: &str = r#"
# Attribute

`#[ink::contract(env = E: impl Environment)]` or `#[ink::contract_ref(env = E: impl Environment)]` or `#[ink_e2e::test(environment = E: impl Environment)]`

# Description

Tells the ink! code generator which environment to use for the ink! smart contract.

The environment must implement the `Environment` (defined in `ink_env`) trait and provides all the necessary fundamental type definitions for `Balance`, `AccountId` etc.

# Usage

Additional argument for ink! contract, ink! contract ref or ink! e2e test attribute macros.

When using a custom `Environment` implementation for a smart contract all types that it exposes to the ink! smart contract and the mirrored types used in the runtime must be aligned with respect to SCALE encoding and semantics.

**Default value:** `DefaultEnvironment` defined in `ink_env` crate.

# Example

Given a custom `Environment` implementation:

```
pub struct MyEnvironment;

impl ink_env::Environment for MyEnvironment {
    const NATIVE_TO_ETH_RATIO: u32 = 100_000_000;

    type AccountId = [u8; 16];
    type Balance = u128;
    type Hash = [u8; 32];
    type Timestamp = u64;
    type BlockNumber = u32;
    type EventRecord = ();
}
```

A user might implement their ink! smart contract using the above custom `Environment` implementation as demonstrated below:

```
#[ink::contract(env = MyEnvironment)]
mod my_contract {
    // --snip--
}
```

OR

A user might declare a smart contract reference using the above custom `Environment` implementation as demonstrated below:

```
#[ink::contract_ref(env = MyEnvironment)]
pub trait Callee {
    // --snip--
}
```

OR

A user might write an end-to-end test using the above custom `Environment` implementation as demonstrated below:

```
#[ink_e2e::test(environment = MyEnvironment)]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L143-L199>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L31-L37>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L41-L45>.
///
/// Ref: <https://paritytech.github.io/ink/ink_e2e_macro/attr.test.html>.
pub const ENV_DOC_LTE_V5: &str = r#"
# Attribute

`#[ink::contract(env = E: impl Environment)]` or `#[ink_e2e::test(environment = E: impl Environment)]`

# Description

Tells the ink! code generator which environment to use for the ink! smart contract.

The environment must implement the `Environment` (defined in `ink_env`) trait and provides all the necessary fundamental type definitions for `Balance`, `AccountId` etc.

# Usage

Additional argument for ink! contract or ink! e2e test attribute macros.

When using a custom `Environment` implementation for a smart contract all types that it exposes to the ink! smart contract and the mirrored types used in the runtime must be aligned with respect to SCALE encoding and semantics.

**Default value:** `DefaultEnvironment` defined in `ink_env` crate.

# Example

Given a custom `Environment` implementation:

```
pub struct MyEnvironment;

impl ink_env::Environment for MyEnvironment {
    const MAX_EVENT_TOPICS: usize = 3;
    type AccountId = [u8; 16];
    type Balance = u128;
    type Hash = [u8; 32];
    type Timestamp = u64;
    type BlockNumber = u32;
    type ChainExtension = ::ink::env::NoChainExtension;
}
```

A user might implement their ink! smart contract using the above custom `Environment` implementation as demonstrated below:

```
#[ink::contract(env = MyEnvironment)]
mod my_contract {
    // --snip--
}
```

OR

A user might write an end-to-end test using the above custom `Environment` implementation as demonstrated below:

```
#[ink_e2e::test(environment = MyEnvironment)]
async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L429-L475>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const EVENT_DOC: &str = r#"
# Attribute

`#[ink(event)]`

# Description

Defines an ink! event.

A contract can define multiple such ink! events.

# Usage

On `struct` definitions.

# Example

```
#[ink::contract]
mod erc20 {
     /// Defines an event that is emitted every time value is transferred.
     #[ink(event)]
     pub struct Transferred {
         from: Option<AccountId>,
         to: Option<AccountId>,
         value: Balance,
     }

     // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L877-L904>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html>.
pub const EXTENSION_DOC_V4: &str = r#"
# Attribute

`#[ink(extension = N: u32)]`

# Description

Determines the unique function ID of the chain extension function.

# Usage

Required attribute for chain extension functions.

# Example

```
type Access = i32;

#[ink::chain_extension]
pub trait MyChainExtension {
    type ErrorCode = i32;

    #[ink(extension = 5)]
    fn key_access_for_account(key: &[u8], account: &[u8]) -> Access;
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L921-L929>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html#macro-attributes>.
pub const EXTENSION_DOC: &str = r#"
# Attribute

`#[ink::chain_extension(extension = N: u16)]`

# Description

The runtime may have several chain extensions at the same time.
The `extension` identifier points to the corresponding chain extension in the runtime.
The value should be the same as during the definition of the chain extension.

# Usage

Required argument for ink! chain extension attribute macros.

# Example

```
#[ink::chain_extension(extension = 1)]
pub trait MyChainExtension {
    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L931-L963>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html#method-attributes>.
pub const FUNCTION_DOC: &str = r#"
# Attribute

`#[ink(function = N: u16)]`

# Description

Determines the unique function ID of the chain extension function.

# Usage

Required attribute for chain extension functions.

# Example

```
type Access = i32;

#[ink::chain_extension]
pub trait MyChainExtension {
    type ErrorCode = i32;

    #[ink(function = 5)]
    fn key_access_for_account(key: &[u8], account: &[u8]) -> Access;
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L906-L955>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html>.
pub const HANDLE_STATUS_DOC: &str = r#"
# Attribute

`#[ink(handle_status = flag: bool)]`

# Description

Assumes that the returned status code of the chain extension function always indicates success and therefore always loads and decodes the output buffer of the call.

# Usage

Applicable to chain extension functions.

**Default value:** `true`.

# Example

```
type Access = i32;

#[ink::chain_extension]
pub trait MyChainExtension {
    type ErrorCode = i32;

    #[ink(extension = 5, handle_status = false)]
    fn key_access_for_account(key: &[u8], account: &[u8]) -> Access;
}
```

OR

```
type Access = i32;

#[ink::chain_extension]
pub trait MyChainExtension {
    type ErrorCode = i32;

    #[ink(extension = 5)]
    #[ink(handle_status = false)]
    fn key_access_for_account(key: &[u8], account: &[u8]) -> Access;
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/ir/src/ir/item_impl/mod.rs#L122-L155>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L408-L470>.
pub const IMPL_DOC: &str = r#"
# Attribute

`#[ink(impl)]`

# Description

Tells the ink! codegen that some implementation block shall be granted access to ink! internals even without it containing any ink! messages or ink! constructors.

# Usage

Applicable to ink! implementation blocks.

# Example

```
#[ink(impl)]
impl MyContract {
    fn my_function(&self) {
        // inherent method implementation
        unimplemented!()
    }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L116-L138>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L622-L640>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.trait_definition.html>.
pub const KEEP_ATTR_DOC: &str = r#"
# Attribute

`#[ink::contract(keep_attr = N: string)]` or `#[ink::trait_definition(keep_attr = N: string)]`

# Description

Tells the ink! code generator which attributes should be passed to call builders.

Call builders are used to doing cross-contract calls and are automatically generated for contracts.

# Usage

Additional argument for ink! contract and ink! trait definition attribute macros.

**Allowed attributes by default:** `cfg`, `cfg_attr`, `allow`, `warn`, `deny`, `forbid`, `deprecated`, `must_use`, `doc`, `rustfmt`.

# Example

```
#[ink::contract(keep_attr = "foo, bar")]
mod my_contract {
    #[ink(storage)]
    pub struct MyContract;

    impl MyContract {
        #[ink(constructor)]
        #[bar]
        pub fn new() -> Self { MyContract {} }

        #[ink(message)]
        #[foo]
        pub fn message(&self) {}
    }

    // --snip--
}
```

OR

```
#[ink::trait_definition(keep_attr = "foo, bar")]
pub trait MyTrait {
    #[ink(message)]
    #[foo]
    fn message1(&self);

    #[ink(message)]
    #[bar]
    fn message2(&self);
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L27-L28>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L49-L50>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L41-L45>.
///
/// Ref: <https://paritytech.github.io/ink/ink_e2e_macro/attr.test.html>.
pub const KEEP_ATTR_E2E_DOC_DEPRECATED: &str = r#"
ink! argument `keep_attr` for `ink_e2e::test` attribute macro is deprecated. See https://github.com/paritytech/ink/pull/1830 for details.
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L116-L138>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L622-L640>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.trait_definition.html>.
pub const KEEP_ATTR_DOC_V4: &str = r#"
# Attribute

`#[ink::contract(keep_attr = N: string)]` or `#[ink::trait_definition(keep_attr = N: string)]` or `#[ink_e2e::test(keep_attr = N: string)]`

# Description

Tells the ink! code generator which attributes should be passed to call builders.

Call builders are used to doing cross-contract calls and are automatically generated for contracts.

# Usage

Additional argument for ink! contract, ink! trait definition and ink! e2e test attribute macros.

**Allowed attributes by default:** `cfg`, `cfg_attr`, `allow`, `warn`, `deny`, `forbid`, `deprecated`, `must_use`, `doc`, `rustfmt`.

# Example

```
#[ink::contract(keep_attr = "foo, bar")]
mod my_contract {
    #[ink(storage)]
    pub struct MyContract;

    impl MyContract {
        #[ink(constructor)]
        #[bar]
        pub fn new() -> Self { MyContract {} }

        #[ink(message)]
        #[foo]
        pub fn message(&self) {}
    }

    // --snip--
}
```

OR

```
#[ink::trait_definition(keep_attr = "foo, bar")]
pub trait MyTrait {
    #[ink(message)]
    #[foo]
    fn message1(&self);

    #[ink(message)]
    #[bar]
    fn message2(&self);
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L265-L308>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const MESSAGE_DOC: &str = r#"
# Attribute

`#[ink(message)]`

# Description

Flags a method for the ink! storage `struct` as a message making it available to the API for calling the contract.

# Usage

Applicable to methods.

# Example

```
#[ink::contract]
mod my_contract {
    // --snip--

    impl MyContract {
        // --snip--

        /// Updates the current value.
        #[ink(message)]
        pub fn set(&mut self) {
            self.value = !self.value;
        }

        /// Returns the current value.
        #[ink(message)]
        pub fn get(&self) -> bool {
            self.value
        }
    }
}
```
"#;

/// Ref: <https://use.ink/docs/v6/macros-attributes/name>.
///
/// Ref: <https://github.com/use-ink/ink/pull/2577/>.
pub const NAME_DOC: &str = r#"
# Attribute

`#[ink(constructor, name = N: string)]` or `#[ink(message, name = N: string)]` or `#[ink::event(name = N: string)]` or `#[ink(event, name = N: string)]`

# Description

Applicable to ink! messages, ink! constructors and ink! events.

Specifies a name/identifier override that is used in place of the item's name/identifier for:
- Selector computation for ink! messages and ink! constructors
- Signature topic computation for ink! events
- Contract metadata generation for the ink! messages, ink! constructors or ink! events

In general, `name` overrides should mainly be used to:
- Implement contracts compliant with Ethereum/Solidity contract standards (i.e. ERCs)
  that require/use overloaded interfaces
- Define interfaces (e.g. via trait definitions) for interacting with
  existing Solidity ABI encoded contracts with overloaded interfaces
- As a more transparent alternative to custom selectors for the
  "name-changing while maintaining the same selector" use case for ink! ABI contracts

# Example

```rust
#[ink(message, name = "myConstructor")]
fn my_constructor(&self) {}

#[ink(message, name = "myMessage")]
fn my_message(&self) {}

#[ink::event(name = "MyEvent")]
pub struct Event { ... }
```

This changes the resulting selectors of the ink! message and ink! constructor,
and the signature topic for the ink! event, as well as their names in contract metadata.
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L602-L620>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.trait_definition.html>.
pub const NAMESPACE_DOC: &str = r#"
# Attribute

`#[ink(namespace = N: string)]` or `#[ink::trait_definition(namespace = N: string)]`

# Description

Changes the resulting selectors of all the ink! messages and ink! constructors within the trait implementation.

Allows to disambiguate between trait implementations with overlapping message or constructor names.

**Use only with great care and consideration!**

# Usage

Applicable to ink! trait implementation blocks.

**Default value:** Empty.

# Example

```
#[ink::trait_definition(namespace = "foo")]
pub trait TraitDefinition {
    #[ink(message)]
    fn message1(&self);

    #[ink(message, selector = 42)]
    fn message2(&self);
}
```
"#;

/// Ref: <https://github.com/use-ink/ink/pull/2722>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.storage_item.html>.
pub const PACKED_DOC: &str = r#"
# Attribute

`#[ink::storage_item(packed)]`

# Description

A configuration flag for storage items that enables "packed" layout.

# Usage

Additional argument for ink! storage item attribute macro.

**Default value:** not set.

# Example

```
use ink::storage::Mapping;

#[ink::storage_item(packed)]
struct PackedGeneric<T: ink::storage::traits::Packed> {
    s1: (u128, bool),
    s2: Vec<T>,
    s3: String,
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L310-L345>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const PAYABLE_DOC: &str = r#"
# Attribute

`#[ink(payable)]`

# Description

Allows receiving value as part of the call of the ink! message.

ink! constructors are implicitly payable.

# Usage

Applicable to ink! messages.

# Example

```
#[ink::contract]
mod my_contract {
    // --snip--

    impl MyContract {
        // --snip--

        /// Update the current value.
        #[ink(message)]
        #[ink(payable)] // You can either specify payable out-of-line.
        pub fn set(&mut self) {
            self.value = !self.value;
        }

        /// Returns the current value.
        #[ink(message, payable)] // ...or specify payable inline.
        pub fn get(&self) -> bool {
            self.value
        }
    }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v5.0.0-rc.1#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L354-L391>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html#analysis>.
///
/// Ref: <https://github.com/paritytech/ink/pull/1708>.
pub const SELECTOR_DOC: &str = r#"
# Attribute

`#[ink(selector = S: u32 | _ | @)]`

# Description

The `#[ink(selector = S:u32)]` variant specifies a concrete dispatch selector for the flagged entity.
This allows a contract author to precisely control the selectors of their APIs making it possible to rename their API without breakage.

While the `#[ink(selector = _)]` variant specifies a fallback message that is invoked if no other ink! message matches a selector
and requires exactly one other message annotated with the complementary `#[ink(selector = @)]` variant to be defined.

# Usage

The `#[ink(selector = S:u32)]` and `#[ink(selector = _)]` variants are applicable to ink! messages and ink! constructors.

While the `#[ink(selector = @)]` variant is only applicable to ink! messages.

# Example

```
#[ink::contract]
mod my_contract {
    // --snip--

    impl MyContract {
        #[ink(constructor)]
        #[ink(selector = 0xDEADBEEF)] // Works on constructors as well.
        pub fn new(initial_value: bool) -> Self {
            MyContract { value: false }
        }

        /// Updates the current value.
        #[ink(message)]
        #[ink(selector = 0xCAFEBABE)] // You can either specify selector out-of-line.
        pub fn set(&mut self) {
            self.value = !self.value;
        }

        /// Returns the current value.
        #[ink(message, selector = 0xFEEDBEEF)] // ...or specify selector inline.
        pub fn get(&self) -> bool {
            self.value
        }
    }
}
```

OR

#[ink::contract]
mod my_contract {
    // --snip--

    impl MyContract {
        // --snip--

        /// Handles any message with no matching selector in this proxy contract
        #[ink(message, selector = _)]
        pub fn fallback(&self) {
          // forward call to the "logic" contract which actually executes the call
        }

        /// One other message allowed to handle messages.
        /// Fails to compile unless `selector = @` is used.
        /// This MUST be specified along with a wildcard selector.
        /// I've just used `@` for now as a complement to `_`
        #[ink(message, selector = @)]
        pub fn handler(&self, msg: ProxyMessage) {
            match msg {
                ProxyMessage(hash) => ...
            }
        }

        #[derive(Decode)]
        pub enum ProxyMessage {
            UpgradeContract(Hash),
        }

        // --snip--
    }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L347-L384>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const SELECTOR_DOC_V4: &str = r#"
# Attribute

`#[ink(selector = S: u32 | _)]`

# Description

The `#[ink(selector = S:u32)]` variant specifies a concrete dispatch selector for the flagged entity.
This allows a contract author to precisely control the selectors of their APIs making it possible to rename their API without breakage.

While the `#[ink(selector = _)]` variant specifies a fallback message that is invoked if no other ink! message matches a selector.

# Usage

Applicable to ink! messages and ink! constructors.

# Example

```
#[ink::contract]
mod my_contract {
    // --snip--

    impl MyContract {
        #[ink(constructor)]
        #[ink(selector = 0xDEADBEEF)] // Works on constructors as well.
        pub fn new(initial_value: bool) -> Self {
            MyContract { value: false }
        }

        /// Updates the current value.
        #[ink(message)]
        #[ink(selector = 0xCAFEBABE)] // You can either specify selector out-of-line.
        pub fn set(&mut self) {
            self.value = !self.value;
        }

        /// Returns the current value.
        #[ink(message, selector = 0xFEEDBEEF)] // ...or specify selector inline.
        pub fn get(&self) -> bool {
            self.value
        }
    }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v5.0.0-rc.1#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L656-L692>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.event.html>.
pub const SIGNATURE_TOPIC: &str = r#"
# Attribute

`#[ink::event(signature_topic = S: string)]` or `#[ink(signature_topic = S: string)]`

# Description

Specifies custom signature topic of the event that allows to use manually specify shared event definition.

By default, a signature topic will be generated for the event.
This allows consumers to filter and identify events of this type.
Marking an event with `anonymous` means no signature topic will be generated or emitted.

# Usage

Applicable to the ink! events attribute macro.

Custom signature topic can be specified with `signature_topic = <32 byte hex string>`.

`signature_topic` and `anonymous` are conflicting arguments.

# Example
```
#[ink::event(
    signature_topic = "1111111111111111111111111111111111111111111111111111111111111111"
)]
pub struct MyEvent {
    value: bool,
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L208-L233>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const STORAGE_DOC: &str = r#"
# Attribute

`#[ink(storage)]`

# Description

Defines the ink! storage `struct`.

There can only be one ink! storage definition per contract.

# Usage

On `struct` definitions.

# Example

```
#[ink::contract]
mod my_contract {
    #[ink(storage)]
    pub struct MyContract {
        value: bool,
    }

    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v5.0.0-rc.1#ink-macros--attributes-overview>.
pub const TOPIC_DOC: &str = r#"
# Attribute

`#[ink(topic)]`

# Description

Tells the ink! codegen to provide a topic hash for the given field.

Every ink! event can only have a limited number of such topic fields.
Similar semantics as to indexed event arguments in Solidity.

# Usage

Applicable on ink! event field.

# Example

```
#[ink::event]
pub struct MyEvent {
    #[ink(topic)]
    value: bool,
}
```

OR

```
#[ink::contract]
mod my_contract {
    #[ink(event)]
    pub struct MyEvent {
        #[ink(topic)]
        value: bool,
    }

    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/tree/v4.2.0#ink-macros--attributes-overview>.
pub const TOPIC_DOC_V4: &str = r#"
# Attribute

`#[ink(topic)]`

# Description

Tells the ink! codegen to provide a topic hash for the given field.

Every ink! event can only have a limited number of such topic fields.
Similar semantics as to indexed event arguments in Solidity.

# Usage

Applicable on ink! event field.

# Example

```
#[ink::contract]
mod my_contract {
    #[ink(event)]
    pub struct MyEvent {
        #[ink(topic)]
        value: bool,
    }

    // --snip--
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/macro/src/lib.rs#L1598-L1625>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.scale_derive.html>.
pub const TYPE_INFO_DOC: &str = r#"
# Attribute

`#[ink::scale_derive(TypeInfo)]`

# Description

Derive the re-exported `ink::scale_info::TypeInfo` trait.
It enables using the built in derive macros for the `ink::scale_info::TypeInfo` trait
without depending directly on the `scale-info` crate.

# Usage

Applicable to ink! scale derive attribute macro.

# Example

```
#[ink::scale_derive(TypeInfo)]
pub enum Error {}
```

This is a convenience macro that expands to include the additional `crate` attributes
required for the path of the re-exported crates.

```
#[cfg_attr(
  feature = "std",
  derive(::scale_info::TypeInfo),
  scale_info(crate = ::ink::scale_info)
)]
pub enum Error {}
```
"#;
