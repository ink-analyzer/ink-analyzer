//! ink! attribute argument IR.

use std::cmp::Ordering;
use std::fmt;

use ra_ap_syntax::{ast, AstToken, TextRange};

use super::meta::{MetaName, MetaNameValue, MetaOption, MetaValue};
use crate::Version;

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

    /// Returns parent attribute (if any).
    pub fn parent_attr(&self) -> Option<ast::Attr> {
        self.meta.parent_attr()
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
    /// `#[ink(abi)]`
    Abi,
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
    /// `#[ink(handle_status)]`
    HandleStatus,
    /// `#[ink(impl)]`
    Impl,
    /// `#[ink(keep_attr)]`
    KeepAttr,
    /// `#[ink(message)]`
    Message,
    /// `#[ink(name)]`
    Name,
    /// `#[ink(namespace)]`
    Namespace,
    /// `#[ink(node)]`
    Node,
    /// `#[ink(payable)]`
    Payable,
    /// `#[ink(runtime_only)]`
    RuntimeOnly,
    /// `#[ink(sandbox)]`
    Sandbox,
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
    /// `#[ink(url)]`
    Url,
    /// Unknown ink! attribute argument.
    Unknown,
}

impl From<&str> for InkArgKind {
    /// Converts a string slice representing a meta item name into an ink! attribute argument kind.
    fn from(arg_name: &str) -> Self {
        match arg_name {
            // `#[ink(abi)]`
            "abi" => InkArgKind::Abi,
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
            // `#[ink(handle_status)]`
            "handle_status" => InkArgKind::HandleStatus,
            // `#[ink(impl)]`
            "impl" => InkArgKind::Impl,
            // `#[ink(keep_attr)]`
            "keep_attr" => InkArgKind::KeepAttr,
            // `#[ink(message)]`
            "message" => InkArgKind::Message,
            // `#[ink(name)]`
            "name" => InkArgKind::Name,
            // `#[ink(namespace)]`
            "namespace" => InkArgKind::Namespace,
            // `#[ink(node)]`
            "node" => InkArgKind::Node,
            // `#[ink(payable)]`
            "payable" => InkArgKind::Payable,
            // `#[ink(runtime_only)]`
            "runtime_only" => InkArgKind::RuntimeOnly,
            // `#[ink(sandbox)]`
            "sandbox" => InkArgKind::Sandbox,
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
            // `#[ink(url)]`
            "url" => InkArgKind::Url,
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
                // `#[ink(abi)]`
                InkArgKind::Abi => "abi",
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
                // `#[ink(handle_status)]`
                InkArgKind::HandleStatus => "handle_status",
                // `#[ink(impl)]`
                InkArgKind::Impl => "impl",
                // `#[ink(keep_attr)]`
                InkArgKind::KeepAttr => "keep_attr",
                // `#[ink(message)]`
                InkArgKind::Message => "message",
                // `#[ink(name)]`
                InkArgKind::Name => "name",
                // `#[ink(namespace)]`
                InkArgKind::Namespace => "namespace",
                // `#[ink(node)]`
                InkArgKind::Node => "node",
                // `#[ink(payable)]`
                InkArgKind::Payable => "payable",
                // `#[ink(runtime_only)]`
                InkArgKind::RuntimeOnly => "runtime_only",
                // `#[ink(sandbox)]`
                InkArgKind::Sandbox => "sandbox",
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
                // `#[ink(url)]`
                InkArgKind::Url => "url",
                // unknown ink! attribute argument.
                InkArgKind::Unknown => "unknown",
            }
        )
    }
}

/// Assigns a sort ascending rank (i.e 0 is highest rank) to ink! attribute argument kinds
/// so that we choose the best `InkArgKind` for ink! attributes regardless of their actual ordering in source code.
///
/// (e.g. the kind for `#[ink(selector=1, payable, message)]` should still be `InkArgKind::Message`).
fn ink_arg_kind_sort_order(arg_kind: InkArgKind) -> u8 {
    match arg_kind {
        // Entity-type arguments get the highest priority.
        // (i.e. `storage`, `event`, `impl`, `constructor`, `message`, `extension` e.t.c).
        InkArgKind::Constructor
        | InkArgKind::Event
        | InkArgKind::Extension // Only in v4, should be 1 in v5
        | InkArgKind::Function // Only exists in v5
        | InkArgKind::Impl
        | InkArgKind::Message
        | InkArgKind::Storage
        | InkArgKind::Topic => 0,
        // Complimentary arguments (i.e. everything else excluding "unknown") get the next priority level.
        // This includes complimentary/optional arguments for
        // entity-level arguments (e.g. `anonymous`, `payable`, `selector` e.t.c),
        // macro-level arguments (e.g. `env`, `keep_attr`, `derive` e.t.c) and ambiguous arguments (e.g `namespace`).
        // This group is explicitly enumerated to force explicit decisions about
        // the priority level of new `InkArgKind` additions.
        InkArgKind::Abi
        | InkArgKind::AdditionalContracts
        | InkArgKind::Anonymous
        | InkArgKind::Backend
        | InkArgKind::Decode
        | InkArgKind::Default
        | InkArgKind::Derive
        | InkArgKind::Encode
        | InkArgKind::Env
        | InkArgKind::Environment
        | InkArgKind::HandleStatus
        | InkArgKind::KeepAttr
        | InkArgKind::Name
        | InkArgKind::Namespace
        | InkArgKind::Node
        | InkArgKind::Payable
        | InkArgKind::RuntimeOnly
        | InkArgKind::Sandbox
        | InkArgKind::Selector
        | InkArgKind::SignatureTopic
        | InkArgKind::TypeInfo
        | InkArgKind::Url => 1,
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
    pub fn detail(&self, version: Version) -> &str {
        const CHAIN_EXT_DEPRECATION_NOTICE: &str = "ink! chain extensions are deprecated. See https://github.com/use-ink/ink/pull/2621 for details.";
        match self {
            InkArgKind::Abi if version.is_gte_v6() => "Specifies the ABI (Application Binary Interface) of the target.",
            InkArgKind::AdditionalContracts if version.is_legacy() => "Tells the ink! e2e test runner which additional contracts to build before executing the test.",
            InkArgKind::AdditionalContracts => "ink! attribute argument `additional_contracts` is deprecated. See https://github.com/paritytech/ink/pull/2098 for details.",
            InkArgKind::Anonymous => "Tells the ink! codegen to treat the ink! event as anonymous which omits the event signature as topic upon emitting.",
            InkArgKind::Backend if version.is_gte_v5() => "Tells the ink! e2e test runner which type of architecture to use to execute the test.",
            InkArgKind::Constructor => "Flags a function for the ink! storage `struct` as a constructor making it available to the API for instantiating the contract.",
            InkArgKind::Decode if version.is_gte_v5() => "Derives an implementation of the `ink::scale::Decode` trait.",
            InkArgKind::Default => "Tells UI to treat the ink! message or ink! constructor as the default choice in selection widgets (e.g dropdowns).",
            InkArgKind::Derive => "A configuration parameter used to enable/disable auto deriving of all required storage traits.",
            InkArgKind::Encode if version.is_gte_v5() => "Derives an implementation of the `ink::scale::Encode` trait.",
            InkArgKind::Env => "Tells the ink! code generator which environment to use for the ink! smart contract.",
            InkArgKind::Environment => "Tells the ink! e2e test runner which environment to use to execute the test.",
            InkArgKind::Event => "Defines an ink! event.",
            InkArgKind::Extension if version.is_legacy() => "Determines the unique function ID of the chain extension function.",
            InkArgKind::Extension if version.is_v5() => "Determines the unique ID of the chain extension.",
            InkArgKind::Extension => CHAIN_EXT_DEPRECATION_NOTICE,
            InkArgKind::Function if version.is_v5() => "Determines the unique function ID of the chain extension function.",
            InkArgKind::Function if version.is_gte_v6() => CHAIN_EXT_DEPRECATION_NOTICE,
            InkArgKind::HandleStatus if version.is_lte_v5() => "Assumes that the returned status code of the chain extension function always indicates success and therefore always loads and decodes the output buffer of the call.",
            InkArgKind::HandleStatus => CHAIN_EXT_DEPRECATION_NOTICE,
            InkArgKind::Impl => "Tells the ink! codegen that some implementation block shall be granted access to ink! internals even without it containing any ink! messages or ink! constructors.",
            InkArgKind::KeepAttr => "Tells the ink! code generator which attributes should be passed to call builders.",
            InkArgKind::Message => "Flags a method for the ink! storage `struct` as a message making it available to the API for calling the contract.",
            InkArgKind::Name if version.is_gte_v6() => "Specifies a name/identifier override that is used in place of the item's name/identifier for:\n\
             - Selector computation for ink! messages and ink! constructors\
             - Signature topic computation for ink! events\
             - Contract metadata generation for the ink! messages, ink! constructors or ink! events",
            InkArgKind::Namespace => "Changes the resulting selectors of all the ink! messages and ink! constructors within the trait implementation.",
            InkArgKind::Node if version.is_gte_v5() => "Tells the ink! e2e test runner to use the standard approach of running dedicated single-node blockchain in a background process to execute the test.",
            InkArgKind::Payable => "Allows receiving value as part of the call of the ink! message.",
            InkArgKind::RuntimeOnly if version.is_v5_0() => "Tells the ink! e2e test runner to use the lightweight approach of skipping the node layer by running a runtime emulator within `TestExternalities` (using drink! library) in the same process as the test.",
            InkArgKind::RuntimeOnly if version.is_gte_v5_1() => "Tells the ink! e2e test runner to use the lightweight approach of skipping the node layer by running a runtime emulator within `TestExternalities` in the same process as the test.",
            InkArgKind::Sandbox if version.is_gte_v5() => "Tells the ink! e2e test runner which runtime emulator to use when executing the test.",
            InkArgKind::Selector => "The `u32` variant specifies a concrete dispatch selector for the flagged entity, \
            which allows a contract author to precisely control the selectors of their APIs making it possible to rename their API without breakage.\n\n\
            While the `_` variant specifies a fallback message that is invoked if no other ink! message matches a selector.",
            InkArgKind::SignatureTopic if version.is_gte_v5() => "Specifies custom signature topic of the event that allows to use manually specified shared event definition.",
            InkArgKind::Storage => "Defines the ink! storage `struct`.",
            InkArgKind::Topic => "Tells the ink! codegen to provide a topic hash for the given field.",
            InkArgKind::TypeInfo if version.is_gte_v5() => "Derives an implementation of the `ink::scale_info::TypeInfo` trait.",
            InkArgKind::Url => "Tells the ink! e2e test runner which node url to connect to before executing the test.",
            _ => "",
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
    // See <https://github.com/paritytech/ink/pull/1708>
    U32OrWildcardOrComplement,
    String(InkArgValueStringKind),
    Bool,
    Path(InkArgValuePathKind),
    // The `bool` represent whether the nested arg is required (true) or optional (false).
    Arg(InkArgKind, bool),
    Choice(InkArgKind, InkArgKind, bool),
}

/// The ink! attribute argument value string kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum InkArgValueStringKind {
    Abi,
    CommaList,
    Default,
    Hex,
    /// A valid Rust identifier
    Identifier,
    /// An "identifier-like" string i.e. must accept both valid Rust and Solidity identifiers.
    IdentifierLike,
    SpaceList,
    Url,
}

/// The ink! attribute argument value path kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum InkArgValuePathKind {
    Default,
    Environment,
    Sandbox,
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
            InkArgKind::Abi => InkArgValueKind::String(InkArgValueStringKind::Abi),
            InkArgKind::AdditionalContracts => {
                InkArgValueKind::String(InkArgValueStringKind::SpaceList)
            }
            InkArgKind::Backend => {
                InkArgValueKind::Choice(InkArgKind::Node, InkArgKind::RuntimeOnly, true)
            }
            InkArgKind::Env | InkArgKind::Environment => {
                InkArgValueKind::Path(InkArgValuePathKind::Environment)
            }
            // TODO: Set to `InkArgValueKind::U16` for ink! v5.
            InkArgKind::Extension => InkArgValueKind::U32,
            InkArgKind::Function => InkArgValueKind::U16,
            InkArgKind::HandleStatus | InkArgKind::Derive => InkArgValueKind::Bool,
            InkArgKind::KeepAttr => InkArgValueKind::String(InkArgValueStringKind::CommaList),
            InkArgKind::Name => InkArgValueKind::String(InkArgValueStringKind::IdentifierLike),
            InkArgKind::Namespace => InkArgValueKind::String(InkArgValueStringKind::Identifier),
            InkArgKind::Node => InkArgValueKind::Arg(InkArgKind::Url, false),
            InkArgKind::RuntimeOnly => InkArgValueKind::Arg(InkArgKind::Sandbox, false),
            InkArgKind::Sandbox => InkArgValueKind::Path(InkArgValuePathKind::Sandbox),
            // TODO: Set to `InkArgValueKind::U32OrWildcardOrComplement` for ink! v5.
            InkArgKind::Selector => InkArgValueKind::U32OrWildcard,
            InkArgKind::SignatureTopic => InkArgValueKind::String(InkArgValueStringKind::Hex),
            InkArgKind::Url => InkArgValueKind::String(InkArgValueStringKind::Url),
            _ => InkArgValueKind::None,
        }
    }
}

impl InkArgValueKind {
    // TODO: This should be default, behaviour, and we should instead have a `from_legacy` method for versions <= 4.x
    pub fn from_v5(arg_kind: InkArgKind, is_constructor: Option<bool>) -> Self {
        match arg_kind {
            InkArgKind::Extension => InkArgValueKind::U16,
            // Constructor selectors, don't accept the wildcard complement/`@` symbol (unlike message selectors).
            InkArgKind::Selector if is_constructor == Some(true) => InkArgValueKind::U32OrWildcard,
            InkArgKind::Selector => InkArgValueKind::U32OrWildcardOrComplement,
            _ => InkArgValueKind::from(arg_kind),
        }
    }
}

impl fmt::Display for InkArgValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InkArgValueKind::U16 => "u16".to_owned(),
                InkArgValueKind::U32 => "u32".to_owned(),
                InkArgValueKind::U32OrWildcard => "u32 | _".to_owned(),
                InkArgValueKind::U32OrWildcardOrComplement => "u32 | _ | @".to_owned(),
                InkArgValueKind::String(_) => "&str".to_owned(),
                InkArgValueKind::Bool => "bool".to_owned(),
                InkArgValueKind::Path(path_kind) => match path_kind {
                    InkArgValuePathKind::Environment => "impl Environment".to_owned(),
                    // TODO: Update to ink_sandbox::Sandbox for >= 5.1.x
                    InkArgValuePathKind::Sandbox => "impl drink::Sandbox".to_owned(),
                    _ => "Path".to_owned(),
                },
                InkArgValueKind::Arg(kind, required) if *required => kind.to_string(),
                InkArgValueKind::Choice(kind_1, kind_2, required) if *required =>
                    format!("{kind_1} | {kind_2}"),
                _ => "".to_owned(),
            }
        )
    }
}

impl InkArgValueKind {
    /// Returns extra details/docs about the ink! attribute argument value kind.
    ///
    /// (e.g. details about further validation is applied for the value kind).
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/attrs.rs#L879-L1023>.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/config.rs#L39-L70>.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/utils.rs#L92-L107>.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/config.rs#L49-L85>.
    pub fn detail(&self) -> String {
        // TODO: Make version aware
        match self {
            InkArgValueKind::Path(InkArgValuePathKind::Environment) => {
                "A `ink::env::Environment` implementation.".to_owned()
            }
            InkArgValueKind::Path(InkArgValuePathKind::Sandbox) => {
                "A `drink::Sandbox` implementation.".to_owned()
            }
            InkArgValueKind::String(InkArgValueStringKind::CommaList) => {
                "A comma separated list.".to_owned()
            }
            InkArgValueKind::String(InkArgValueStringKind::Hex) => {
                "A 32 byte hex string.".to_owned()
            }
            InkArgValueKind::String(InkArgValueStringKind::Abi) => {
                r#"The ABI (Application Binary Interface), either "ink" or "sol"."#.to_owned()
            }
            InkArgValueKind::String(InkArgValueStringKind::Identifier) => {
                "A valid Rust identifier.".to_owned()
            }
            InkArgValueKind::String(InkArgValueStringKind::IdentifierLike) => {
                "An \"identifier-like\" string i.e. \
                must begin with an alphabetic character, underscore or dollar sign, \
                and only contain alphanumeric characters, underscores and dollar signs."
                    .to_owned()
            }
            InkArgValueKind::String(InkArgValueStringKind::SpaceList) => {
                "A space separated list.".to_owned()
            }
            InkArgValueKind::String(InkArgValueStringKind::Url) => "A URL.".to_owned(),
            InkArgValueKind::Arg(kind, required) if *required => kind.to_string(),
            InkArgValueKind::Choice(kind_1, kind_2, required) if *required => {
                format!("{kind_1} | {kind_2}")
            }
            _ => "".to_owned(),
        }
    }
}
