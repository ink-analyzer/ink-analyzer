//! ink! contract ref IR.

use ra_ap_syntax::ast;

use crate::Message;

/// An ink! contract ref.
#[ink_analyzer_macro::entity(macro_kind = ContractRef)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractRef {
    // ASTNode type.
    ast: ast::Trait,
    // ink! messages.
    messages: Vec<Message>,
}

impl_ast_type_trait!(ContractRef, IsInkTrait);

impl_has_ink_environment!(ContractRef, Env);

impl ContractRef {
    /// Returns the specified ABI (if any).
    pub fn abi(&self) -> Option<String> {
        self.abi_arg()?.value()?.as_string()
    }

    impl_pub_ink_arg_getter!(abi_arg, Abi, abi);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::{HasInkEnvironment, InkEntity, IsInkTrait};
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node = parse_first_syntax_node(quote_as_str! {
            #[ink::contract_ref(abi="sol", env=crate::MyEnvironment)]
            pub trait MyTrait {
                #[ink(message)]
                fn my_message(&self);

                #[ink(message)]
                fn my_message_mut(&mut self);
            }

            #[derive(Clone)]
            pub struct MyEnvironment;

            impl ink::env::Environment for MyEnvironment {
                const NATIVE_TO_ETH_RATIO: u32 = 100_000_000;

                type AccountId = [u8; 16];
                type Balance = u128;
                type Hash = [u8; 32];
                type Timestamp = u64;
                type BlockNumber = u32;
                type EventRecord = ();
            }
        });

        let contract_ref = ContractRef::cast(node).unwrap();

        // `abi` argument exists.
        assert!(contract_ref.abi_arg().is_some());

        // `abi` argument value.
        assert_eq!(contract_ref.abi().as_deref(), Some("sol"));

        // `env` argument exists.
        assert!(contract_ref.env_arg().is_some());

        // `environment` ADT is returned.
        assert!(contract_ref.environment().is_some());

        // 2 messages.
        assert_eq!(contract_ref.messages().len(), 2);

        // `trait` item exists.
        assert!(contract_ref.trait_item().is_some());
    }
}
