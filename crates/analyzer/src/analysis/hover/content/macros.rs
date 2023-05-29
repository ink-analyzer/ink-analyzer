//! Hover content for ink! attribute macros.

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L848-L1280>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.chain_extension.html>.
pub const CHAIN_EXTENSION_DOC: &str = r#"
# Attribute

`#[ink::chain_extension]`

# Description

Defines the interface for a chain extension.

# Structure

The interface consists of an error code that indicates lightweight errors
as well as the definition of some chain extension methods.

The overall structure follows that of a simple Rust trait definition.
The error code is defined as an associated type definition of the trait definition.
The methods are defined as associated trait methods without implementation.

Chain extension methods must not have a `self` receiver such as `&self` or `&mut self`
and must have inputs and output that implement the SCALE encoding and decoding.
Their return value follows specific rules that can be altered using the
`handle_status` attribute which is described in more detail below.

# Usage

Usually the chain extension definition using this procedural macro is provided
by the author of the chain extension in a separate crate.
ink! smart contracts using this chain extension simply depend on this crate
and use its associated environment definition in order to make use of
the methods provided by the chain extension.

# Attributes

There are three different attributes with which the chain extension methods
can be flagged:

| Attribute | Required | Default Value | Description |
|:----------|:--------:|:--------------|:-----------:|
| `ink(extension = N: u32)` | Yes | - | Determines the unique function ID of the chain
extension method. | | `ink(handle_status = flag: bool)` | Optional | `true` | Assumes
that the returned status code of the chain extension method always indicates success
and therefore always loads and decodes the output buffer of the call. |

As with all ink! attributes multiple of them can either appear in a contiguous list:
```
# type Access = i32;
# #[ink::chain_extension]
# pub trait MyChainExtension {
#     type ErrorCode = i32;
#[ink(extension = 5, handle_status = false)]
fn key_access_for_account(key: &[u8], account: &[u8]) -> Access;
# }
```
…or as multiple stand alone ink! attributes applied to the same item:
```
# type Access = i32;
# #[ink::chain_extension]
# pub trait MyChainExtension {
#     type ErrorCode = i32;
#[ink(extension = 5)]
#[ink(handle_status = false)]
fn key_access_for_account(key: &[u8], account: &[u8]) -> Access;
# }
```

## Details: `handle_status`

Default value: `true`

By default all chain extension methods should return a `Result<T, E>` where `E:
From<Self::ErrorCode>`. The `Self::ErrorCode` represents the error code of the chain
extension. This means that a smart contract calling such a chain extension method
first queries the returned status code of the chain extension method and only loads
and decodes the output if the returned status code indicates a successful call.
This design was chosen as it is more efficient when no output besides the error
code is required for a chain extension call. When designing a chain extension try to
utilize the error code to return errors and only use the output buffer for information
that does not fit in a single `u32` value.

A chain extension method that is flagged with `handle_status = false` assumes that the
returned error code will always indicate success. Therefore it will always load and
decode the output buffer and loses the `E: From<Self::ErrorCode>` constraint for the
call.

Note that if a chain extension method does not return `Result<T, E>` where `E:
From<Self::ErrorCode>` but `handle_status = true` it will still return a value of type
`Result<T, Self::ErrorCode>`.

## Usage: `handle_status`

Use both `handle_status = false` and non-`Result<T, E>` return type for the same chain
extension method if a call to it may never fail and never returns a `Result` type.

# Combinations

Due to the possibility to flag a chain extension method with `handle_status` and
return or not `Result<T, E>` there are 4 different cases with slightly varying
semantics:

| `handle_status` | Returns `Result<T, E>` | Effects |
|:---------------:|:----------------:|:--------|
| `true`  | `true`  | The chain extension method is required to return a value of type
`Result<T, E>` where `E: From<Self::ErrorCode>`. A call will always check if the
returned status code indicates success and only then will load and decode the value in
the output buffer. | | `true`  | `false` | The chain extension method may return any
non-`Result` type. A call will always check if the returned status code indicates
success and only then will load and decode the value in the output buffer. The actual
return type of the chain extension method is still `Result<T, Self::ErrorCode>` when
the chain extension method was defined to return a value of type `T`. | | `false` |
`true`  | The chain extension method is required to return a value of type `Result<T,
E>`. A call will always assume that the returned status code indicates success and
therefore always load and decode the output buffer directly. | | `false` | `false` |
The chain extension method may return any non-`Result` type. A call will always assume
that the returned status code indicates success and therefore always load and decode
the output buffer directly. |

# Error Code

Every chain extension defines exactly one `ErrorCode` using the following syntax:

```
#[ink::chain_extension]
pub trait MyChainExtension {
     type ErrorCode = MyErrorCode;

     // more definitions
}
```

The defined `ErrorCode` must implement `FromStatusCode` which should be implemented as
a more or less trivial conversion from the `u32` status code to a `Result<(),
Self::ErrorCode>`. The `Ok(())` value indicates that the call to the chain extension
method was successful.

By convention an error code of `0` represents success.
However, chain extension authors may use whatever suits their needs.

# Example: Definition

In the below example a chain extension is defined that allows its users to read and
write from and to the runtime storage using access privileges:

```
/// Custom chain extension to read to and write from the runtime.
#[ink::chain_extension]
pub trait RuntimeReadWrite {
     type ErrorCode = ReadWriteErrorCode;

     /// Reads from runtime storage.
     ///
     /// # Note
     ///
     /// Actually returns a value of type `Result<Vec<u8>, Self::ErrorCode>`.
     #[ink(extension = 1)]
     fn read(key: &[u8]) -> Vec<u8>;

     /// Reads from runtime storage.
     ///
     /// Returns the number of bytes read and up to 32 bytes of the
     /// read value. Unused bytes in the output are set to 0.
     ///
     /// # Errors
     ///
     /// If the runtime storage cell stores a value that requires more than
     /// 32 bytes.
     ///
     /// # Note
     ///
     /// This requires `ReadWriteError` to implement `From<ReadWriteErrorCode>`
     /// and may potentially return any `Self::ErrorCode` through its return value.
     #[ink(extension = 2)]
     fn read_small(key: &[u8]) -> Result<(u32, [u8; 32]), ReadWriteError>;

     /// Writes into runtime storage.
     ///
     /// # Note
     ///
     /// Actually returns a value of type `Result<(), Self::ErrorCode>`.
     #[ink(extension = 3)]
     fn write(key: &[u8], value: &[u8]);

     /// Returns the access allowed for the key for the caller.
     ///
     /// # Note
     ///
     /// Assumes to never fail the call and therefore always returns `Option<Access>`.
     #[ink(extension = 4, handle_status = false)]
     fn access(key: &[u8]) -> Option<Access>;

     /// Unlocks previously aquired permission to access key.
     ///
     /// # Errors
     ///
     /// If the permission was not granted.
     ///
     /// # Note
     ///
     /// Assumes the call to never fail and therefore does _NOT_ require `UnlockAccessError`
     /// to implement `From<Self::ErrorCode>` as in the `read_small` method above.
     #[ink(extension = 5, handle_status = false)]
     fn unlock_access(key: &[u8], access: Access) -> Result<(), UnlockAccessError>;
}
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub enum ReadWriteErrorCode {
#     InvalidKey,
#     CannotWriteToKey,
#     CannotReadFromKey,
# }
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub enum ReadWriteError {
#     ErrorCode(ReadWriteErrorCode),
#     BufferTooSmall { required_bytes: u32 },
# }
# impl From<ReadWriteErrorCode> for ReadWriteError {
#     fn from(error_code: ReadWriteErrorCode) -> Self {
#         Self::ErrorCode(error_code)
#     }
# }
# impl From<scale::Error> for ReadWriteError {
#     fn from(_: scale::Error) -> Self {
#         panic!("encountered unexpected invalid SCALE encoding")
#     }
# }
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub struct UnlockAccessError {
#     reason: String,
# }
# impl From<scale::Error> for UnlockAccessError {
#     fn from(_: scale::Error) -> Self {
#         panic!("encountered unexpected invalid SCALE encoding")
#     }
# }
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub enum Access {
#     ReadWrite,
#     ReadOnly,
#     WriteOnly,
# }
# impl ink_env::chain_extension::FromStatusCode for ReadWriteErrorCode {
#     fn from_status_code(status_code: u32) -> Result<(), Self> {
#         match status_code {
#             0 => Ok(()),
#             1 => Err(Self::InvalidKey),
#             2 => Err(Self::CannotWriteToKey),
#             3 => Err(Self::CannotReadFromKey),
#             _ => panic!("encountered unknown status code"),
#         }
#     }
# }
```

All the error types and other utility types used in the chain extension definition
above are often required to implement various traits such as SCALE's `Encode` and
`Decode` as well as `scale-info`'s `TypeInfo` trait.

A full example of the above chain extension definition can be seen
[here](https://github.com/paritytech/ink/blob/017f71d60799b764425334f86b732cc7b7065fe6/crates/lang/macro/tests/ui/chain_extension/simple.rs).

# Example: Environment

In order to allow ink! smart contracts to use the above defined chain extension it
needs to be integrated into an `Environment` definition as shown below:

```
# type RuntimeReadWrite = i32;
#
use ink_env::{
     DefaultEnvironment,
     Environment,
};

pub enum CustomEnvironment {}

impl Environment for CustomEnvironment {
     const MAX_EVENT_TOPICS: usize =
         <DefaultEnvironment as Environment>::MAX_EVENT_TOPICS;

     type AccountId = <DefaultEnvironment as Environment>::AccountId;
     type Balance = <DefaultEnvironment as Environment>::Balance;
     type Hash = <DefaultEnvironment as Environment>::Hash;
     type BlockNumber = <DefaultEnvironment as Environment>::BlockNumber;
     type Timestamp = <DefaultEnvironment as Environment>::Timestamp;

     type ChainExtension = RuntimeReadWrite;
}
```

Above we defined the `CustomEnvironment` which defaults to ink!'s `DefaultEnvironment`
for all constants and types but the `ChainExtension` type which is assigned to our
newly defined chain extension.

# Example: Usage

An ink! smart contract can use the above defined chain extension through the
`Environment` definition defined in the last example section using the `env` macro
parameter as shown below.

Note that chain extension methods are accessible through `Self::extension()` or
`self.extension()`. For example as in `Self::extension().read(...)` or
`self.extension().read(...)`.

```
#[ink::contract(env = CustomEnvironment)]
mod read_writer {
     #[ink(storage)]
     pub struct ReadWriter {}

     impl ReadWriter {
         #[ink(constructor)]
         pub fn new() -> Self { Self {} }

         #[ink(message)]
         pub fn read(&self, key: Vec<u8>) -> Result<Vec<u8>, ReadWriteErrorCode> {
             self.env()
                 .extension()
                 .read(&key)
         }

         #[ink(message)]
         pub fn read_small(&self, key: Vec<u8>) -> Result<(u32, [u8; 32]), ReadWriteError> {
             self.env()
                 .extension()
                 .read_small(&key)
         }

         #[ink(message)]
         pub fn write(
             &self,
             key: Vec<u8>,
             value: Vec<u8>,
         ) -> Result<(), ReadWriteErrorCode> {
             self.env()
                 .extension()
                 .write(&key, &value)
         }

         #[ink(message)]
         pub fn access(&self, key: Vec<u8>) -> Option<Access> {
             self.env()
                 .extension()
                 .access(&key)
         }

         #[ink(message)]
         pub fn unlock_access(&self, key: Vec<u8>, access: Access) -> Result<(), UnlockAccessError> {
             self.env()
                 .extension()
                 .unlock_access(&key, access)
         }
     }
# /// Custom chain extension to read to and write from the runtime.
# #[ink::chain_extension]
# pub trait RuntimeReadWrite {
#     type ErrorCode = ReadWriteErrorCode;
#     #[ink(extension = 1)]
#     fn read(key: &[u8]) -> Vec<u8>;
#     #[ink(extension = 2)]
#     fn read_small(key: &[u8]) -> Result<(u32, [u8; 32]), ReadWriteError>;
#     #[ink(extension = 3)]
#     fn write(key: &[u8], value: &[u8]);
#     #[ink(extension = 4, handle_status = false)]
#     fn access(key: &[u8]) -> Option<Access>;
#     #[ink(extension = 5, handle_status = false)]
#     fn unlock_access(key: &[u8], access: Access) -> Result<(), UnlockAccessError>;
# }
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub enum ReadWriteErrorCode {
#     InvalidKey,
#     CannotWriteToKey,
#     CannotReadFromKey,
# }
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub enum ReadWriteError {
#     ErrorCode(ReadWriteErrorCode),
#     BufferTooSmall { required_bytes: u32 },
# }
# impl From<ReadWriteErrorCode> for ReadWriteError {
#     fn from(error_code: ReadWriteErrorCode) -> Self {
#         Self::ErrorCode(error_code)
#     }
# }
# impl From<scale::Error> for ReadWriteError {
#     fn from(_: scale::Error) -> Self {
#         panic!("encountered unexpected invalid SCALE encoding")
#     }
# }
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub struct UnlockAccessError {
#     reason: String,
# }
# impl From<scale::Error> for UnlockAccessError {
#     fn from(_: scale::Error) -> Self {
#         panic!("encountered unexpected invalid SCALE encoding")
#     }
# }
# #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
# pub enum Access {
#     ReadWrite,
#     ReadOnly,
#     WriteOnly,
# }
# impl ink_env::chain_extension::FromStatusCode for ReadWriteErrorCode {
#     fn from_status_code(status_code: u32) -> Result<(), Self> {
#         match status_code {
#             0 => Ok(()),
#             1 => Err(Self::InvalidKey),
#             2 => Err(Self::CannotWriteToKey),
#             3 => Err(Self::CannotReadFromKey),
#             _ => panic!("encountered unknown status code"),
#         }
#     }
# }
# pub enum CustomEnvironment {}
# impl ink_env::Environment for CustomEnvironment {
#     const MAX_EVENT_TOPICS: usize =
#         <ink_env::DefaultEnvironment as ink_env::Environment>::MAX_EVENT_TOPICS;
#
#     type AccountId = <ink_env::DefaultEnvironment as ink_env::Environment>::AccountId;
#     type Balance = <ink_env::DefaultEnvironment as ink_env::Environment>::Balance;
#     type Hash = <ink_env::DefaultEnvironment as ink_env::Environment>::Hash;
#     type BlockNumber = <ink_env::DefaultEnvironment as ink_env::Environment>::BlockNumber;
#     type Timestamp = <ink_env::DefaultEnvironment as ink_env::Environment>::Timestamp;
#
#     type ChainExtension = RuntimeReadWrite;
# }
}
```

# Technical Limitations

- Due to technical limitations it is not possible to refer to the `ErrorCode`
   associated type using `Self::ErrorCode` anywhere within the chain extension and its
   defined methods. Instead chain extension authors should directly use the error code
   type when required. This limitation might be lifted in future versions of ink!.
- It is not possible to declare other chain extension traits as super traits or super
   chain extensions of another.
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L90-L520>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.contract.html>.
pub const CONTRACT_DOC: &str = r#"
# Attribute

`#[ink::contract]`

# Description

Entry point for writing ink! smart contracts.

If you are a beginner trying to learn ink! we recommend you to check out
our extensive [ink! workshop](https://docs.substrate.io/tutorials/v3/ink-workshop/pt1).

# Description

The macro does analysis on the provided smart contract code and generates
proper code.

ink! smart contracts can compile in several different modes.
There are two main compilation models using either
- on-chain mode: `no_std` and WebAssembly as target
- off-chain mode: `std`

We generally use the on-chain mode for actual smart contract instantiation
whereas we use the off-chain mode for smart contract testing using the
off-chain environment provided by the `ink_env` crate.

# Usage

## Header Arguments

The `#[ink::contract]` macro can be provided with some additional comma-separated
header arguments:

- `keep_attr: String`

     Tells the ink! code generator which attributes should be passed to call builders.
     Call builders are used to doing cross-contract calls and are automatically
     generated for contracts.

     **Usage Example:**
     ```
     #[ink::contract(keep_attr = "foo, bar")]
     mod my_contract {
         # #[ink(storage)]
         # pub struct MyStorage;
         # impl MyStorage {
         #     #[ink(constructor)]
         //    #[bar]
         #     pub fn construct() -> Self { MyStorage {} }
         #     #[ink(message)]
         //    #[foo]
         #     pub fn message(&self) {}
         # }
         // ...
     }
     ```

     **Allowed attributes by default:** `cfg`, `cfg_attr`, `allow`, `warn`, `deny`,
`forbid`,         `deprecated`, `must_use`, `doc`, `rustfmt`.

- `env: impl Environment`

     Tells the ink! code generator which environment to use for the ink! smart
contract.     The environment must implement the `Environment` (defined in `ink_env`)
trait and provides     all the necessary fundamental type definitions for `Balance`,
`AccountId` etc.

     When using a custom `Environment` implementation for a smart contract all types
     that it exposes to the ink! smart contract and the mirrored types used in the
runtime     must be aligned with respect to SCALE encoding and semantics.

     **Usage Example:**

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
     A user might implement their ink! smart contract using the above custom
`Environment`     implementation as demonstrated below:
     ```
     #[ink::contract(env = MyEnvironment)]
     mod my_contract {
         # pub struct MyEnvironment;
         #
         # impl ink_env::Environment for MyEnvironment {
         #     const MAX_EVENT_TOPICS: usize = 3;
         #     type AccountId = [u8; 16];
         #     type Balance = u128;
         #     type Hash = [u8; 32];
         #     type Timestamp = u64;
         #     type BlockNumber = u32;
         #     type ChainExtension = ::ink::env::NoChainExtension;
         # }
         #
         # #[ink(storage)]
         # pub struct MyStorage;
         # impl MyStorage {
         #     #[ink(constructor)]
         #     pub fn construct() -> Self { MyStorage {} }
         #     #[ink(message)]
         #     pub fn message(&self) {}
         # }
         // ...
     }
     ```

     **Default value:** `DefaultEnvironment` defined in `ink_env` crate.

## Analysis

The `#[ink::contract]` macro fully analyses its input smart contract
against invalid arguments and structure.

Some example rules include but are not limited to:

- There must be exactly one `#[ink(storage)]` struct.

     This struct defines the layout of the storage that the ink! smart contract
operates on.     The user is able to use a variety of built-in facilities, combine
them in various ways     or even provide their own implementations of storage data
structures.

     For more information visit the `ink::storage` crate documentation.

     **Example:**

     ```
     #[ink::contract]
     mod flipper {
         #[ink(storage)]
         pub struct Flipper {
             value: bool,
         }
         # impl Flipper {
         #     #[ink(constructor)]
         #     pub fn construct() -> Self { Flipper { value: false } }
         #     #[ink(message)]
         #     pub fn message(&self) {}
         # }
     }
     ```

- There must be at least one `#[ink(constructor)]` defined method.

     Methods flagged with `#[ink(constructor)]` are special in that they are
dispatchable     upon contract instantiation. A contract may define multiple such
constructors which     allow users of the contract to instantiate a contract in
multiple different ways.

     **Example:**

     Given the `Flipper` contract definition above we add an `#[ink(constructor)]`
     as follows:

     ```
     # #[ink::contract]
     # mod flipper {
         # #[ink(storage)]
         # pub struct Flipper {
         #     value: bool,
         # }
     impl Flipper {
         #[ink(constructor)]
         pub fn new(initial_value: bool) -> Self {
             Flipper { value: false }
         }
         # #[ink(message)]
         # pub fn message(&self) {}
     }
     # }
     ```

- There must be at least one `#[ink(message)]` defined method.

     Methods flagged with `#[ink(message)]` are special in that they are dispatchable
     upon contract invocation. The set of ink! messages defined for an ink! smart
contract     define its API surface with which users are allowed to interact.

     An ink! smart contract can have multiple such ink! messages defined.

     **Note:**

     - An ink! message with a `&self` receiver may only read state whereas an ink!
       message with a `&mut self` receiver may mutate the contract's storage.

     **Example:**

     Given the `Flipper` contract definition above we add some `#[ink(message)]`
definitions     as follows:

     ```
     # #[ink::contract]
     # mod flipper {
         # #[ink(storage)]
         # pub struct Flipper {
         #     value: bool,
         # }
     impl Flipper {
         # #[ink(constructor)]
         # pub fn new(initial_value: bool) -> Self {
         #     Flipper { value: false }
         # }
         /// Flips the current value.
         #[ink(message)]
         pub fn flip(&mut self) {
             self.value = !self.value;
         }

         /// Returns the current value.
         #[ink(message)]
         pub fn get(&self) -> bool {
             self.value
         }
     }
     # }
     ```

     **Payable Messages:**

     An ink! message by default will reject calls that additional fund the smart
contract.     Authors of ink! smart contracts can make an ink! message payable by
adding the `payable`     flag to it. An example below:

     Note that ink! constructors are always implicitly payable and thus cannot be
flagged     as such.

     ```
     # #[ink::contract]
     # mod flipper {
         # #[ink(storage)]
         # pub struct Flipper {
         #     value: bool,
         # }
     impl Flipper {
         # #[ink(constructor)]
         # pub fn new(initial_value: bool) -> Self {
         #     Flipper { value: false }
         # }
         /// Flips the current value.
         #[ink(message)]
         #[ink(payable)] // You can either specify payable out-of-line.
         pub fn flip(&mut self) {
             self.value = !self.value;
         }

         /// Returns the current value.
         #[ink(message, payable)] // ...or specify payable inline.
         pub fn get(&self) -> bool {
             self.value
         }
     }
     # }
     ```

     **Controlling the messages selector:**

     Every ink! message and ink! constructor has a unique selector with which the
     message or constructor can be uniquely identified within the ink! smart contract.
     These selectors are mainly used to drive the contract's dispatch upon calling it.

     An ink! smart contract author can control the selector of an ink! message or ink!
     constructor using the `selector` flag. An example is shown below:

     ```
     # #[ink::contract]
     # mod flipper {
         # #[ink(storage)]
         # pub struct Flipper {
         #     value: bool,
         # }
     impl Flipper {
         #[ink(constructor)]
         #[ink(selector = 0xDEADBEEF)] // Works on constructors as well.
         pub fn new(initial_value: bool) -> Self {
             Flipper { value: false }
         }

         /// Flips the current value.
         #[ink(message)]
         #[ink(selector = 0xCAFEBABE)] // You can either specify selector out-of-line.
         pub fn flip(&mut self) {
             self.value = !self.value;
         }

         /// Returns the current value.
         #[ink(message, selector = 0xFEEDBEEF)] // ...or specify selector inline.
         pub fn get(&self) -> bool {
             self.value
         }
     }
     # }
     ```

## Interacting with the Contract Executor

The `ink_env` crate provides facilities to interact with the contract executor that
connects ink! smart contracts with the outer world.

For example it is possible to query the current call's caller via:
```
# ink_env::test::run_test::<ink_env::DefaultEnvironment, _>(|_| {
let caller = ink_env::caller::<ink_env::DefaultEnvironment>();
# let _caller = caller;
# Ok(())
# }).unwrap();
```

However, ink! provides a much simpler way to interact with the contract executor
via its environment accessor. An example below:

```
#[ink::contract]
mod greeter {
     use ink_env::debug_println;

     #[ink(storage)]
     pub struct Greeter;

     impl Greeter {
         #[ink(constructor)]
         pub fn new() -> Self {
             let caller = Self::env().caller();
             debug_println!("thanks for instantiation {:?}", caller);
             Greeter {}
         }

         #[ink(message, payable)]
         pub fn fund(&self) {
             let caller = self.env().caller();
             let value = self.env().transferred_value();
             debug_println!("thanks for the funding of {:?} from {:?}", value, caller);
         }
     }
}
```

## Events

An ink! smart contract may define events that it can emit during contract execution.
Emitting events can be used by third party tools to query information about a
contract's execution and state.

The following example ink! contract shows how an event `Transferred` is defined and
emitted in the `#[ink(constructor)]`.

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

     #[ink(storage)]
     pub struct Erc20 {
         total_supply: Balance,
         // more fields...
     }

     impl Erc20 {
         #[ink(constructor)]
         pub fn new(initial_supply: Balance) -> Self {
             let caller = Self::env().caller();
             Self::env().emit_event(Transferred {
                 from: None,
                 to: Some(caller),
                 value: initial_supply,
             });
             Self {
                 total_supply: initial_supply,
             }
         }

         #[ink(message)]
         pub fn total_supply(&self) -> Balance {
             self.total_supply
         }
     }
}
```

## Example: Flipper

The below code shows the complete implementation of the so-called Flipper
ink! smart contract.
For us it acts as the "Hello, World!" of the ink! smart contracts because
it is minimal while still providing some more or less useful functionality.

It controls a single `bool` value that can be either `false` or `true`
and allows the user to flip this value using the `Flipper::flip` message
or retrieve the current value using `Flipper::get`.

```
#[ink::contract]
pub mod flipper {
     #[ink(storage)]
     pub struct Flipper {
         value: bool,
     }

     impl Flipper {
         /// Creates a new flipper smart contract initialized with the given value.
         #[ink(constructor)]
         pub fn new(init_value: bool) -> Self {
             Self { value: init_value }
         }

         /// Flips the current value of the Flipper's boolean.
         #[ink(message)]
         pub fn flip(&mut self) {
             self.value = !self.value;
         }

         /// Returns the current value of the Flipper's boolean.
         #[ink(message)]
         pub fn get(&self) -> bool {
             self.value
         }
     }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L649-L803>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.storage_item.html>.
pub const STORAGE_ITEM_DOC: &str = r#"
# Attribute

`#[ink::storage_item]`

# Description

Prepares the type to be fully compatible and usable with the storage.
It implements all necessary traits and calculates the storage key for types.
`Packed` types don't have a storage key, but non-packed types (like `Mapping`, `Lazy`
etc.) require calculating the storage key during compilation.

Consider annotating structs and enums that are intended to be a part of
the storage with this macro. If the type is packed then the usage of the
macro is optional.

If the type is non-packed it is best to rely on automatic storage key
calculation via `ink::storage_item`.

The usage of `KEY: StorageKey` generic allows to propagate the parent's storage key to
the type and offset the storage key of the type. It is helpful for non-packed types
that can be used several times in the contract. Each field should have a unique
storage key, so propagation of the parent's storage key allows one to achieve it.

The macro should be called before `derive` macros because it can change the type.

All required traits can be:
- Derived manually via `#[derive(...)]`.
- Derived automatically via deriving of `scale::Decode` and `scale::Encode`.
- Derived via this macro.

# Example

## Trait implementation

```
use ink_prelude::vec::Vec;
use ink::storage::{
     Lazy,
     Mapping,
};
use ink::storage::traits::{
     StorageKey,
     StorableHint,
};
use ink::storage::traits::Storable;

// Deriving `scale::Decode` and `scale::Encode` also derives blanket implementation of all
// required traits to be storable.
#[derive(scale::Decode, scale::Encode)]
#[cfg_attr(
     feature = "std",
     derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
#[derive(Default, Debug)]
struct Packed {
     s1: u128,
     s2: Vec<u128>,
     // Fails because `StorableHint` is only implemented for `Vec` where `T: Packed`.
     // s3: Vec<NonPacked>,
}

// Example of how to define the packed type with generic.
#[derive(scale::Decode, scale::Encode)]
#[cfg_attr(
     feature = "std",
     derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
#[derive(Default, Debug)]
struct PackedGeneric<T: ink::storage::traits::Packed> {
     s1: (u128, bool),
     s2: Vec<T>,
     s3: String,
}

// Example of how to define the non-packed type.
#[ink::storage_item]
#[derive(Default, Debug)]
struct NonPacked {
     s1: Mapping<u32, u128>,
     s2: Lazy<u128>,
}

// Example of how to define the non-packed generic type.
#[ink::storage_item(derive = false)]
#[derive(Storable, StorableHint, StorageKey)]
#[cfg_attr(
     feature = "std",
     derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
#[derive(Default, Debug)]
struct NonPackedGeneric<T>
where
     T: Default + core::fmt::Debug,
     T: ink::storage::traits::Packed,
{
     s1: u32,
     s2: T,
     s3: Mapping<u128, T>,
}

// Example of how to define a complex packed type.
#[derive(scale::Decode, scale::Encode)]
#[cfg_attr(
     feature = "std",
     derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
)]
#[derive(Default, Debug)]
struct PackedComplex {
     s1: u128,
     s2: Vec<u128>,
     s3: Vec<Packed>,
}

// Example of how to define a complex non-packed type.
#[ink::storage_item]
#[derive(Default, Debug)]
struct NonPackedComplex<KEY: StorageKey> {
     s1: (String, u128, Packed),
     s2: Mapping<u128, u128>,
     s3: Lazy<u128>,
     s4: Mapping<u128, Packed>,
     s5: Lazy<NonPacked>,
     s6: PackedGeneric<Packed>,
     s7: NonPackedGeneric<Packed>,
     // Fails because: the trait `ink::storage::traits::Packed` is not implemented for `NonPacked`
     // s8: Mapping<u128, NonPacked>,
}
```

## Header Arguments

The `#[ink::storage_item]` macro can be provided with an additional comma-separated
header argument:

- `derive: bool`

     The `derive` configuration parameter is used to enable/disable auto deriving of
     all required storage traits.

     **Usage Example:**
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

     **Default value:** true.
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L805-L846>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.test.html>.
pub const TEST_DOC: &str = r#"
# Attribute

`#[ink::test]`

# Description

Defines a unit test that makes use of ink!'s off-chain testing capabilities.

If your unit test does not require the existence of an off-chain environment
it is fine to not use this macro since it bears some overhead with the test.

Note that this macro is not required to run unit tests that require ink!'s
off-chain testing capabilities but merely improves code readability.

## How do you find out if your test requires the off-chain environment?

Normally if the test recursively uses or invokes some contract methods that
call a method defined in `self.env()` or `Self::env()`.

An examples is the following:

```no_compile
let caller: AccountId = self.env().caller();
```

# Example

```
#[cfg(test)]
mod tests {
     // Conventional unit test that works with assertions.
     #[ink::test]
     fn test1() {
         // test code comes here as usual
     }

     // Conventional unit test that returns some Result.
     // The test code can make use of operator-`?`.
     #[ink::test]
     fn test2() -> Result<(), ink_env::Error> {
         // test code that returns a Rust Result type
     }
}
```
"#;

/// Ref: <https://github.com/paritytech/ink/blob/v4.2.0/crates/ink/macro/src/lib.rs#L522-L647>.
///
/// Ref: <https://paritytech.github.io/ink/ink/attr.trait_definition.html>.
pub const TRAIT_DEFINITION_DOC: &str = r#"
# Attribute

`#[ink::trait_definition]`

# Description

Marks trait definitions to ink! as special ink! trait definitions.

There are some restrictions that apply to ink! trait definitions that
this macro checks. Also ink! trait definitions are required to have specialized
structure so that the main [`#[ink::contract]`](`macro@crate::contract`) macro can
properly generate code for its implementations.

# Example

# Trait definition:

```
# type Balance = <ink_env::DefaultEnvironment as ink_env::Environment>::Balance;
# type AccountId = <ink_env::DefaultEnvironment as ink_env::Environment>::AccountId;

#[ink::trait_definition]
pub trait Erc20 {
     /// Returns the total supply of the ERC-20 smart contract.
     #[ink(message)]
     fn total_supply(&self) -> Balance;

     /// Transfers balance from the caller to the given address.
     #[ink(message)]
     fn transfer(&mut self, amount: Balance, to: AccountId) -> bool;

     // etc.
}
```

# Trait implementation

Given the above trait definition you can implement it as shown below:

```
#[ink::contract]
mod base_erc20 {
#    // We somehow cannot put the trait in the doc-test crate root due to bugs.
#    #[ink::trait_definition]
#    pub trait Erc20 {
#       /// Returns the total supply of the ERC-20 smart contract.
#       #[ink(message)]
#       fn total_supply(&self) -> Balance;
#
#       /// Transfers balance from the caller to the given address.
#       #[ink(message)]
#       fn transfer(&mut self, amount: Balance, to: AccountId) -> bool;
#    }
#
     #[ink(storage)]
     pub struct BaseErc20 {
         total_supply: Balance,
     }

     impl BaseErc20 {
         #[ink(constructor)]
         pub fn new(initial_supply: Balance) -> Self {
             Self { total_supply: initial_supply }
         }
     }

     impl Erc20 for BaseErc20 {
         /// Returns the total supply of the ERC-20 smart contract.
         #[ink(message)]
         fn total_supply(&self) -> Balance {
             self.total_supply
         }

         #[ink(message)]
         fn transfer(&mut self, amount: Balance, to: AccountId) -> bool {
             unimplemented!()
         }
     }
}
```

## Header Arguments

The `#[ink::trait_definition]` macro can be provided with some additional
comma-separated header arguments:

- `namespace: String`

     The namespace configuration parameter is used to influence the generated
     selectors of the ink! trait messages. This is useful to disambiguate
     ink! trait definitions with equal names.

     **Usage Example:**
     ```
     #[ink::trait_definition(namespace = "foo")]
     pub trait TraitDefinition {
         #[ink(message)]
         fn message1(&self);

         #[ink(message, selector = 42)]
         fn message2(&self);
     }
     ```

     **Default value:** Empty.

- `keep_attr: String`

     Tells the ink! code generator which attributes should be passed to call builders.
     Call builders are used to doing cross-contract calls and are automatically
     generated for contracts.

     **Usage Example:**
     ```
     #[ink::trait_definition(keep_attr = "foo, bar")]
     pub trait Storage {
         #[ink(message)]
     //  #[foo]
         fn message1(&self);

         #[ink(message)]
     //  #[bar]
         fn message2(&self);
     }
     ```

     **Allowed attributes by default:** `cfg`, `cfg_attr`, `allow`, `warn`, `deny`,
`forbid`,         `deprecated`, `must_use`, `doc`, `rustfmt`.
"#;
