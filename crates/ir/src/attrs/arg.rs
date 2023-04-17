//! ink! attribute argument IR.

use super::meta::{MetaNameValue, MetaOption};
use ra_ap_syntax::AstToken;

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
            kind: if let MetaOption::Ok(name) = &meta.name {
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
}

/// An ink! attribute argument kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InkArgKind {
    /// `#[ink(anonymous)]`
    Anonymous,
    /// `#[ink(constructor)]`
    Constructor,
    /// `#[ink(default)]`
    Default,
    /// `#[ink(env)]`
    Env,
    /// `#[ink(event)]`
    Event,
    /// `#[ink(extension)]`
    Extension,
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
    /// `#[ink(payable)]`
    Payable,
    /// `#[ink(selector)]`
    Selector,
    /// `#[ink(storage)]`
    Storage,
    /// `#[ink(topic)]`
    Topic,
    /// Fallback for unknown ink! attribute argument.
    Unknown,
}

impl From<&str> for InkArgKind {
    /// Convert a string slice representing a meta item name into an ink! attribute argument kind.
    fn from(arg_name: &str) -> Self {
        match arg_name {
            // `#[ink(anonymous)]`
            "anonymous" => InkArgKind::Anonymous,
            // `#[ink(constructor)]`
            "constructor" => InkArgKind::Constructor,
            // `#[ink(default)]`
            "default" => InkArgKind::Default,
            // `#[ink(env)]`
            "env" => InkArgKind::Env,
            // `#[ink(event)]`
            "event" => InkArgKind::Event,
            // `#[ink(extension)]`
            "extension" => InkArgKind::Extension,
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
            // `#[ink(payable)]`
            "payable" => InkArgKind::Payable,
            // `#[ink(selector)]`
            "selector" => InkArgKind::Selector,
            // `#[ink(storage)]`
            "storage" => InkArgKind::Storage,
            // `#[ink(topic)]`
            "topic" => InkArgKind::Topic,
            // unknown ink! attribute argument.
            _ => InkArgKind::Unknown,
        }
    }
}
