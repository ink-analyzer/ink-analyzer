//! ink! contract IR.

use ra_ap_syntax::ast;

use crate::{Constructor, Event, InkE2ETest, InkImpl, InkTest, Message, Storage};

/// An ink! contract.
#[ink_analyzer_macro::entity(macro_kind = Contract)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Contract {
    // ASTNode type.
    ast: ast::Module,
    // ink! storage.
    storage: Option<Storage>,
    // ink! events.
    events: Vec<Event>,
    // ink! impl items.
    #[initializer(call = crate::tree::utils::ink_impl_closest_descendants)]
    impls: Vec<InkImpl>,
    // ink! constructors.
    #[initializer(call = crate::tree::utils::ink_callable_closest_descendants)]
    constructors: Vec<Constructor>,
    // ink! messages.
    #[initializer(call = crate::tree::utils::ink_callable_closest_descendants)]
    messages: Vec<Message>,
    // ink! tests.
    tests: Vec<InkTest>,
    // ink! e2e tests.
    e2e_tests: Vec<InkE2ETest>,
}

impl_has_ink_environment!(Contract, Env);

impl Contract {
    impl_pub_ast_type_getter!(module, Module);

    impl_pub_ink_arg_getter!(keep_attr_arg, KeepAttr, keep_attr);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{HasInkEnvironment, InkEntity};
    use ra_ap_syntax::AstNode;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node: ast::Module = parse_first_ast_node_of_type(quote_as_str! {
            #[derive(Clone)]
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

            #[ink::contract(env=crate::MyEnvironment, keep_attr="foo,bar")]
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

        let contract = Contract::cast(node.syntax().clone()).unwrap();

        // `env` argument exists.
        assert!(contract.environment_arg().is_some());

        // `environment` ADT is returned.
        assert!(contract.environment().is_some());

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
