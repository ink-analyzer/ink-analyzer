//! ink! attribute argument IR.

use ra_ap_syntax::{AstToken, TextRange};
use std::cmp::Ordering;
use std::fmt;

use super::meta::{MetaName, MetaNameValue, MetaOption, MetaValue};

/// An ink! attribute argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InkArg {
    /// The kind of the ink! attribute argument.
    kind: InkArgKind,
    /// Meta item for ink! attribute argument.
    meta: MetaNameValue,
}

impl From<MetaNameValue> for InkArg {
    fn from(meta: MetaNameValue) -> Self {
        Self {
            kind: if let MetaOption::Ok(name) = meta.name() {
                InkArgKind::from(name.text())
            } else {
                InkArgKind::Unknown
            },
            meta,
        }
    }
}

impl InkArg {
    /// Returns the ink! attribute argument kind.
    pub fn kind(&self) -> &InkArgKind {
        &self.kind
    }

    /// Returns the meta item for ink! attribute argument.
    pub fn meta(&self) -> &MetaNameValue {
        &self.meta
    }

    /// Returns the text range of the ink! attribute argument.
    pub fn text_range(&self) -> TextRange {
        self.meta.text_range()
    }

    /// Returns meta name (if valid).
    ///
    /// Convenience method for cases when we only care about valid names.
    pub fn name(&self) -> Option<&MetaName> {
        self.meta.name().result().ok()
    }

    /// Returns meta value (if valid).
    ///
    /// Convenience method for cases when we only care about valid values.
    pub fn value(&self) -> Option<&MetaValue> {
        self.meta.value().result().ok()
    }

    /// Returns a nested ink! arg (if any).
    ///
    /// Convenience method for cases when we only care about nested ink! args.
    pub fn nested(&self) -> Option<InkArg> {
        self.meta.nested().map(|meta| InkArg::from(meta.to_owned()))
    }
}

impl fmt::Display for InkArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.meta.fmt(f)
    }
}

impl Ord for InkArg {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self.kind(), other.kind())
    }
}

impl PartialOrd for InkArg {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// The ink! attribute argument kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum InkArgKind {
    /// `#[ink(additional_contracts)]`
    AdditionalContracts,
    /// `#[ink(anonymous)]`
    Anonymous,
    /// `#[ink(backend)]`
    Backend,
    /// `#[ink(constructor)]`
    Constructor,
    /// `#[ink(Decode)]`
    Decode,
    /// `#[ink(default)]`
    Default,
    /// `#[ink(derive)]`
    Derive,
    /// `#[ink(Encode)]`
    Encode,
    /// `#[ink(env)]`
    Env,
    /// `#[ink(environment)]`
    Environment,
    /// `#[ink(event)]`
    Event,
    /// `#[ink(extension)]`
    Extension,
    /// `#[ink(function)]`
    Function,
    /// `#[ink(full)]`
    Full,
    /// `#[ink(handle_status)]`
    HandleStatus,
    /// `#[ink(impl)]`
    Impl,
    /// `#[ink(keep_attr)]`
    KeepAttr,
    /// `#[ink(message)]`
    Message,
    /// `#[ink(namespace)]`
    Namespace,
    /// `#[ink(node_url)]`
    NodeUrl,
    /// `#[ink(payable)]`
    Payable,
    /// `#[ink(runtime)]`
    Runtime,
    /// `#[ink(runtime_only)]`
    RuntimeOnly,
    /// `#[ink(selector)]`
    Selector,
    /// `#[ink(signature_topic)]`
    SignatureTopic,
    /// `#[ink(storage)]`
    Storage,
    /// `#[ink(topic)]`
    Topic,
    /// `#[ink(TypeInfo)]`
    TypeInfo,
    /// Unknown ink! attribute argument.
    Unknown,
}

impl From<&str> for InkArgKind {
    /// Converts a string slice representing a meta item name into an ink! attribute argument kind.
    fn from(arg_name: &str) -> Self {
        match arg_name {
            // `#[ink(additional_contracts)]`
            "additional_contracts" => InkArgKind::AdditionalContracts,
            // `#[ink(anonymous)]`
            "anonymous" => InkArgKind::Anonymous,
            // `#[ink(backend)]`
            "backend" => InkArgKind::Backend,
            // `#[ink(constructor)]`
            "constructor" => InkArgKind::Constructor,
            // `#[ink(Decode)]`
            "Decode" => InkArgKind::Decode,
            // `#[ink(default)]`
            "default" => InkArgKind::Default,
            // `#[ink(derive)]`
            "derive" => InkArgKind::Derive,
            // `#[ink(Encode)]`
            "Encode" => InkArgKind::Encode,
            // `#[ink(env)]`
            "env" => InkArgKind::Env,
            // `#[ink(environment)]`
            "environment" => InkArgKind::Environment,
            // `#[ink(event)]`
            "event" => InkArgKind::Event,
            // `#[ink(extension)]`
            "extension" => InkArgKind::Extension,
            // `#[ink(function)]`
            "function" => InkArgKind::Function,
            // `#[ink(full)]`
            "full" => InkArgKind::Full,
            // `#[ink(handle_status)]`
            "handle_status" => InkArgKind::HandleStatus,
            // `#[ink(impl)]`
            "impl" => InkArgKind::Impl,
            // `#[ink(keep_attr)]`
            "keep_attr" => InkArgKind::KeepAttr,
            // `#[ink(message)]`
            "message" => InkArgKind::Message,
            // `#[ink(namespace)]`
            "namespace" => InkArgKind::Namespace,
            // `#[ink(node_url)]`
            "node_url" => InkArgKind::NodeUrl,
            // `#[ink(payable)]`
            "payable" => InkArgKind::Payable,
            // `#[ink(runtime)]`
            "runtime" => InkArgKind::Runtime,
            // `#[ink(runtime_only)]`
            "runtime_only" => InkArgKind::RuntimeOnly,
            // `#[ink(selector)]`
            "selector" => InkArgKind::Selector,
            // `#[ink(signature_topic)]`
            "signature_topic" => InkArgKind::SignatureTopic,
            // `#[ink(storage)]`
            "storage" => InkArgKind::Storage,
            // `#[ink(topic)]`
            "topic" => InkArgKind::Topic,
            // `#[ink(TypeInfo)]`
            "TypeInfo" => InkArgKind::TypeInfo,
            // unknown ink! attribute argument.
            _ => InkArgKind::Unknown,
        }
    }
}

impl fmt::Display for InkArgKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // `#[ink(additional_contracts)]`
                InkArgKind::AdditionalContracts => "additional_contracts",
                // `#[ink(anonymous)]`
                InkArgKind::Anonymous => "anonymous",
                // `#[ink(backend)]`
                InkArgKind::Backend => "backend",
                // `#[ink(constructor)]`
                InkArgKind::Constructor => "constructor",
                // `#[ink(Decode)]`
                InkArgKind::Decode => "Decode",
                // `#[ink(default)]`
                InkArgKind::Default => "default",
                // `#[ink(derive)]`
                InkArgKind::Derive => "derive",
                // `#[ink(Encode)]`
                InkArgKind::Encode => "Encode",
                // `#[ink(env)]`
                InkArgKind::Env => "env",
                // `#[ink(environment)]`
                InkArgKind::Environment => "environment",
                // `#[ink(event)]`
                InkArgKind::Event => "event",
                // `#[ink(extension)]`
                InkArgKind::Extension => "extension",
                // `#[ink(function)]`
                InkArgKind::Function => "function",
                // `#[ink(full)]`
                InkArgKind::Full => "full",
                // `#[ink(handle_status)]`
                InkArgKind::HandleStatus => "handle_status",
                // `#[ink(impl)]`
                InkArgKind::Impl => "impl",
                // `#[ink(keep_attr)]`
                InkArgKind::KeepAttr => "keep_attr",
                // `#[ink(message)]`
                InkArgKind::Message => "message",
                // `#[ink(namespace)]`
                InkArgKind::Namespace => "namespace",
                // `#[ink(node_url)]`
                InkArgKind::NodeUrl => "node_url",
                // `#[ink(payable)]`
                InkArgKind::Payable => "payable",
                // `#[ink(runtime)]`
                InkArgKind::Runtime => "runtime",
                // `#[ink(runtime_only)]`
                InkArgKind::RuntimeOnly => "runtime_only",
                // `#[ink(selector)]`
                InkArgKind::Selector => "selector",
                // `#[ink(signature_topic)]`
                InkArgKind::SignatureTopic => "signature_topic",
                // `#[ink(storage)]`
                InkArgKind::Storage => "storage",
                // `#[ink(topic)]`
                InkArgKind::Topic => "topic",
                // `#[ink(TypeInfo)]`
                InkArgKind::TypeInfo => "TypeInfo",
                // unknown ink! attribute argument.
                InkArgKind::Unknown => "unknown",
            }
        )
    }
}

/// Assigns a sort ascending rank (i.e 0 is highest rank) to ink! attribute argument kinds
/// so that we choose the best `InkArgKind` for ink! attributes regardless of their actual ordering in source code.
///
/// (e.g the kind for `#[ink(selector=1, payable, message)]` should still be `InkArgKind::Message`).
fn ink_arg_kind_sort_order(arg_kind: InkArgKind) -> u8 {
    match arg_kind {
        // Entity-type arguments get highest priority.
        // (i.e. `storage`, `event`, `impl`, `constructor`, `message`, `extension` e.t.c).
        InkArgKind::Constructor
        | InkArgKind::Event
        | InkArgKind::Extension
        | InkArgKind::Function
        | InkArgKind::Impl
        | InkArgKind::Message
        | InkArgKind::Storage
        | InkArgKind::Topic => 0,
        // Complimentary arguments (i.e. everything else excluding "unknown") get the next priority level.
        // This includes complimentary/optional arguments for
        // entity-level arguments (e.g. `anonymous`, `payable`, `selector` e.t.c),
        // macro-level arguments (e.g `env`, `keep_attr`, `derive` e.t.c) and ambiguous arguments (e.g `namespace`).
        // This group is explicitly enumerated to force explicit decisions about
        // the priority level of new `InkArgKind` additions.
        InkArgKind::AdditionalContracts
        | InkArgKind::Anonymous
        | InkArgKind::Backend
        | InkArgKind::Decode
        | InkArgKind::Default
        | InkArgKind::Derive
        | InkArgKind::Encode
        | InkArgKind::Env
        | InkArgKind::Environment
        | InkArgKind::Full
        | InkArgKind::HandleStatus
        | InkArgKind::KeepAttr
        | InkArgKind::Namespace
        | InkArgKind::NodeUrl
        | InkArgKind::Payable
        | InkArgKind::Runtime
        | InkArgKind::RuntimeOnly
        | InkArgKind::Selector
        | InkArgKind::SignatureTopic
        | InkArgKind::TypeInfo => 1,
        // "Unknown" gets a special priority level.
        InkArgKind::Unknown => 10,
    }
}

impl InkArgKind {
    /// Returns true if the ink! argument kind is an "entity type"
    /// (i.e. `storage`, `event`, `impl`, `constructor`, `message`, `extension` e.t.c).
    pub fn is_entity_type(&self) -> bool {
        ink_arg_kind_sort_order(*self) == 0
    }

    /// Returns true if the ink! argument kind is "complementary".
    ///
    /// This includes optional arguments that complement entity-level arguments
    /// (e.g. `anonymous`, `payable`, `selector` e.t.c),
    /// macro-level arguments (e.g `env`, `keep_attr`, `derive` e.t.c) and
    /// ambiguous arguments (e.g `namespace`).
    pub fn is_complementary(&self) -> bool {
        ink_arg_kind_sort_order(*self) == 1
    }

    /// Returns extra details/docs about the ink! attribute argument kind.
    pub fn detail(&self) -> &str {
        match self {
            InkArgKind::AdditionalContracts => "Tells the ink! e2e test runner which additional contracts to build before executing the test.",
            InkArgKind::Anonymous => "Tells the ink! codegen to treat the ink! event as anonymous which omits the event signature as topic upon emitting.",
            InkArgKind::Backend => "Tells the ink! e2e test runner which type of architecture to use to execute the test.",
            InkArgKind::Constructor => "Flags a function for the ink! storage `struct` as a constructor making it available to the API for instantiating the contract.",
            InkArgKind::Decode => "Derives an implementation of the `ink::scale::Decode` trait.",
            InkArgKind::Default => "Tells UI to treat the ink! message or ink! constructor as the default choice in selection widgets (e.g dropdowns).",
            InkArgKind::Derive => "A configuration parameter used to enable/disable auto deriving of all required storage traits.",
            InkArgKind::Encode => "Derives an implementation of the `ink::scale::Encode` trait.",
            InkArgKind::Env => "Tells the ink! code generator which environment to use for the ink! smart contract.",
            InkArgKind::Environment => "Tells the ink! code generator which environment to use for the ink! smart contract.",
            InkArgKind::Event => "Defines an ink! event.",
            // TODO: Add ink! v5 description for extension.
            InkArgKind::Extension | InkArgKind::Function => "Determines the unique function ID of the chain extension function.",
            InkArgKind::Full => "Tells the ink! e2e test runner to use the standard approach of running dedicated single-node blockchain in a background process to execute the test.",
            InkArgKind::HandleStatus => "Assumes that the returned status code of the chain extension function always indicates success and therefore always loads and decodes the output buffer of the call.",
            InkArgKind::Impl => "Tells the ink! codegen that some implementation block shall be granted access to ink! internals even without it containing any ink! messages or ink! constructors.",
            InkArgKind::KeepAttr => "Tells the ink! code generator which attributes should be passed to call builders.",
            InkArgKind::Message => "Flags a method for the ink! storage `struct` as a message making it available to the API for calling the contract.",
            InkArgKind::Namespace => "Changes the resulting selectors of all the ink! messages and ink! constructors within the trait implementation.",
            InkArgKind::NodeUrl => "Tells the ink! e2e test runner which node url to connect to before executing the test",
            InkArgKind::Payable => "Allows receiving value as part of the call of the ink! message.",
            InkArgKind::Runtime => "Tells the ink! e2e test runner which runtime emulator to use when executing the test.",
            InkArgKind::RuntimeOnly => "Tells the ink! e2e test runner to use the lightweight approach of skipping the node layer by running a runtime emulator within `TestExternalities` (using drink! library) in the same process as the test.",
            InkArgKind::Selector => "The `u32` variant specifies a concrete dispatch selector for the flagged entity, \
            which allows a contract author to precisely control the selectors of their APIs making it possible to rename their API without breakage.\n\n\
            While the `_` variant specifies a fallback message that is invoked if no other ink! message matches a selector.",
            InkArgKind::SignatureTopic => "Specifies custom signature topic of the event that allows to use manually specified shared event definition.",
            InkArgKind::Storage => "Defines the ink! storage `struct`.",
            InkArgKind::Topic => "Tells the ink! codegen to provide a topic hash for the given field.",
            InkArgKind::TypeInfo => "Derives an implementation of the `ink::scale_info::TypeInfo` trait.",
            InkArgKind::Unknown => "",
        }
    }
}

impl Ord for InkArgKind {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(
            &ink_arg_kind_sort_order(*self),
            &ink_arg_kind_sort_order(*other),
        )
    }
}

impl PartialOrd for InkArgKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// The ink! attribute argument value kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InkArgValueKind {
    None,
    U16,
    U32,
    U32OrWildcard,
    U32OrWildcardOrAt,
    String(InkArgValueStringKind),
    Bool,
    Path(InkArgValuePathKind),
    Arg(InkArgKind, InkArgKind),
}

/// The ink! attribute argument value string kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum InkArgValueStringKind {
    CommaList,
    Default,
    Hex,
    Identifier,
    SpaceList,
    Url,
}

/// The ink! attribute argument value path kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum InkArgValuePathKind {
    Default,
    Environment,
    Runtime,
}

/// Converts an ink! attribute argument kind to an ink! attribute argument value kind.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L879-L1023>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/utils.rs#L92-L107>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L49-L85>.
impl From<InkArgKind> for InkArgValueKind {
    fn from(arg_kind: InkArgKind) -> Self {
        match arg_kind {
            InkArgKind::AdditionalContracts => {
                InkArgValueKind::String(InkArgValueStringKind::SpaceList)
            }
            InkArgKind::Backend => InkArgValueKind::Arg(InkArgKind::Full, InkArgKind::RuntimeOnly),
            InkArgKind::Env | InkArgKind::Environment => {
                InkArgValueKind::Path(InkArgValuePathKind::Environment)
            }
            // TODO: Set to `InkArgValueKind::U16` for ink! v5.
            InkArgKind::Extension => InkArgValueKind::U32,
            InkArgKind::Function => InkArgValueKind::U16,
            InkArgKind::HandleStatus | InkArgKind::Derive => InkArgValueKind::Bool,
            InkArgKind::KeepAttr => InkArgValueKind::String(InkArgValueStringKind::CommaList),
            InkArgKind::Namespace => InkArgValueKind::String(InkArgValueStringKind::Identifier),
            InkArgKind::NodeUrl => InkArgValueKind::String(InkArgValueStringKind::Url),
            InkArgKind::Runtime => InkArgValueKind::Path(InkArgValuePathKind::Runtime),
            InkArgKind::RuntimeOnly => {
                InkArgValueKind::Arg(InkArgKind::Default, InkArgKind::Runtime)
            }
            // TODO: Set to `InkArgValueKind::U32OrWildcardOrAt` for ink! v5.
            InkArgKind::Selector => InkArgValueKind::U32OrWildcard,
            InkArgKind::SignatureTopic => InkArgValueKind::String(InkArgValueStringKind::Hex),
            _ => InkArgValueKind::None,
        }
    }
}

impl fmt::Display for InkArgValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InkArgValueKind::None | InkArgValueKind::Arg(_, _) => "",
                InkArgValueKind::U16 => "u16",
                InkArgValueKind::U32 => "u32",
                InkArgValueKind::U32OrWildcard => "u32 | _",
                InkArgValueKind::U32OrWildcardOrAt => "u32 | _ | @",
                InkArgValueKind::String(_) => "&str",
                InkArgValueKind::Bool => "bool",
                InkArgValueKind::Path(path_kind) => match path_kind {
                    InkArgValuePathKind::Environment => "impl Environment",
                    InkArgValuePathKind::Runtime => "impl drink::SandboxConfig",
                    _ => "Path",
                },
            }
        )
    }
}

impl InkArgValueKind {
    /// Returns extra details/docs about the ink! attribute argument value kind.
    ///
    /// (e.g details about further validation is applied for the value kind).
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L879-L1023>.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/utils.rs#L92-L107>.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L49-L85>.
    pub fn detail(&self) -> &str {
        match self {
            InkArgValueKind::Path(InkArgValuePathKind::Environment) => {
                "A `ink::env::Environment` implementation."
            }
            InkArgValueKind::Path(InkArgValuePathKind::Runtime) => {
                "A `drink::SandboxConfig` implementation."
            }
            InkArgValueKind::String(InkArgValueStringKind::CommaList) => "A comma separated list.",
            InkArgValueKind::String(InkArgValueStringKind::Hex) => "A 32 byte hex string.",
            InkArgValueKind::String(InkArgValueStringKind::Identifier) => {
                "A valid Rust identifier."
            }
            InkArgValueKind::String(InkArgValueStringKind::SpaceList) => "A space separated list.",
            InkArgValueKind::String(InkArgValueStringKind::Url) => "A URL.",
            InkArgValueKind::Arg(kind_1, kind_2) => match (kind_1, kind_2) {
                (InkArgKind::Full, InkArgKind::RuntimeOnly) => "full | runtime_only",
                (InkArgKind::Default, InkArgKind::Runtime) => {
                    "default | runtime = impl drink::SandboxConfig"
                }
                _ => "",
            },
            _ => "",
        }
    }
}
