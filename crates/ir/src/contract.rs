//! ink! contract IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::traits::{FromInkAttribute, FromSyntax};
use crate::tree::utils;
use crate::{
    Constructor, Event, InkArg, InkArgKind, InkAttrData, InkAttribute, InkImpl, InkTest, Message,
    Storage,
};

/// An ink! contract.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Contract {
    /// ink! attribute IR data.
    #[macro_kind(Contract)]
    ink_attr: InkAttrData<ast::Module>,
    /// ink! storage definition.
    #[arg_kind(Storage)]
    storage: Option<Storage>,
    /// ink! events.
    #[arg_kind(Event)]
    events: Vec<Event>,
    /// ink! impl items.
    #[arg_kind(Impl)]
    impls: Vec<InkImpl>,
    /// ink! constructors.
    #[arg_kind(Constructor)]
    constructors: Vec<Constructor>,
    /// ink! messages.
    #[arg_kind(Message)]
    messages: Vec<Message>,
    /// ink! tests.
    #[macro_kind(Test)]
    tests: Vec<InkTest>,
}

impl Contract {
    /// Returns the `mod` item (if any) for the ink! contract.
    pub fn module(&self) -> Option<&ast::Module> {
        self.ink_attr.parent_ast()
    }

    /// Returns the ink! env argument (if any) for the ink! contract.
    pub fn env_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Env)
    }

    /// Returns the ink! keep_attr argument (if any) for the ink! contract.
    pub fn keep_attr_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::KeepAttr)
    }

    /// Returns the ink! storage definition for the ink! contract.
    pub fn storage(&self) -> Option<&Storage> {
        self.storage.as_ref()
    }

    /// Returns the ink! events for the ink! contract.
    pub fn events(&self) -> &[Event] {
        &self.events
    }

    /// Returns the ink! impl blocks for the ink! contract.
    pub fn impls(&self) -> &[InkImpl] {
        &self.impls
    }

    /// Returns the ink! constructors for the ink! contract.
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    /// Returns the ink! messages for the ink! contract.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }

    /// Returns the ink! tests for the ink! contract.
    pub fn tests(&self) -> &[InkTest] {
        &self.tests
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let ink_attr = parse_first_ink_attribute(quote_as_str! {
            #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
            mod MyContract {
                #[ink(storage)]
                pub struct MyContract {}

                #[ink(event)]
                pub struct MyEvent {
                }

                #[ink(event, anonymous)]
                pub struct MyEvent2 {
                }

                impl MyContract {
                    #[ink(constructor, payable, default, selector=_)]
                    pub fn my_constructor() -> Self {}

                    #[ink(message, payable, default, selector=_)]
                    pub fn my_message(&self) {}
                }

                impl MyTrait for MyContract {
                    #[ink(constructor, payable, default, selector=1)]
                    fn my_constructor() -> Self {}

                    #[ink(message, payable, default, selector=1)]
                    fn my_message(&self) {}
                }

                impl ::my_full::long_path::MyTrait for MyContract {
                    #[ink(constructor, payable, default, selector=0x2)]
                    fn my_constructor() -> Self {}

                    #[ink(message, payable, default, selector=0x2)]
                    fn my_message(&self) {}
                }

                impl relative_path::MyTrait for MyContract {
                    #[ink(constructor)]
                    fn my_constructor() -> Self {}

                    #[ink(message)]
                    fn my_message(&self) {}
                }

                #[ink(namespace="my_namespace")]
                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {}

                    #[ink(message)]
                    pub fn my_message(&self) {}
                }

                #[ink(impl)]
                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {}

                    #[ink(message)]
                    pub fn my_message(&self) {}
                }

                #[ink(impl, namespace="my_namespace")]
                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {}

                    #[ink(message)]
                    pub fn my_message(&self) {}
                }

                #[ink(impl)]
                impl MyContract {
                }

                #[cfg(test)]
                mod tests {
                    #[ink::test]
                    fn it_works {
                    }

                    #[ink::test]
                    fn it_works2 {
                    }
                }
            }
        });

        let contract = Contract::cast(ink_attr).unwrap();

        // `env` argument exists.
        assert!(contract.env_arg().is_some());

        // `keep_attr` argument exists.
        assert!(contract.keep_attr_arg().is_some());

        // storage definition exists.
        assert!(contract.storage().is_some());

        // 2 events.
        assert_eq!(contract.events().len(), 2);

        // 8 impls.
        assert_eq!(contract.impls().len(), 8);

        // 7 constructors.
        assert_eq!(contract.constructors().len(), 7);

        // 7 messages.
        assert_eq!(contract.messages().len(), 7);

        // 2 tests.
        assert_eq!(contract.tests().len(), 2);

        // `mod` item exists.
        assert!(contract.module().is_some());
    }
}
