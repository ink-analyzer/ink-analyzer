//! Test fixtures for ink! analyzer.

use crate::{
    TestCase, TestCaseModification, TestCaseParams, TestCaseResults, TestGroup,
    TestParamsOffsetOnly, TestParamsRangeOnly, TestResultAction, TestResultSignatureHelp,
    TestResultSignatureParam, TestResultTextOffsetRange, TestResultTextRange,
};

/// Describes a collection of diagnostics tests to run against optionally modified
/// ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn diagnostics_fixtures() -> Vec<TestGroup> {
    let erc20 = [
        TestCase {
            // Makes no modifications to the source code.
            modifications: None,
            // No parameters.
            params: None,
            // Expects no diagnostic errors/warnings.
            results: TestCaseResults::Diagnostic {
                n: 0,
                quickfixes: vec![],
            },
        },
        TestCase {
            // Removes `#[ink::contract]` from the source code.
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: "",
            }]),
            // No parameters.
            params: None,
            // Expects 10 diagnostic errors/warnings (i.e. 1 storage, 2 events,
            // 1 constructor and 6 messages without a contract parent).
            results: TestCaseResults::Diagnostic {
                n: 10,
                quickfixes: vec![
                    // storage actions.
                    (
                        vec![
                            // Remove attribute text edits.
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-#[ink(storage)]"),
                                    end_pat: Some("<-#[derive(Default)]"),
                                }],
                            },
                            // Remove item text edits.
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// A simple ERC-20 contract."),
                                    end_pat: Some("<-/// Event emitted when"),
                                }],
                            },
                        ],
                        // Cursor offset that can trigger the quickfixes.
                        Some("<-#[ink(storage)]"),
                    ),
                    // event actions.
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-#[ink(event)]"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Event emitted when a token"),
                                    end_pat: Some("<-/// Event emitted when an approval"),
                                }],
                            },
                        ],
                        Some("<-#[ink(event)]"),
                    ),
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(
                                        "<-#[ink(event)]\
                                                \n    pub struct Approval",
                                    ),
                                    end_pat: Some("<-pub struct Approval"),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Event emitted when an approval"),
                                    end_pat: Some("<-/// The ERC-20 error types."),
                                }],
                            },
                        ],
                        Some(
                            "<-#[ink(event)]\
                                    \n    pub struct Approval",
                        ),
                    ),
                    // constructor actions.
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-#[ink(constructor)]"),
                                    end_pat: Some("<-pub fn new(total_supply: Balance)"),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Creates a new ERC-20 contract"),
                                    end_pat: Some("<-/// Returns the total token supply."),
                                }],
                            },
                        ],
                        Some("<-#[ink(constructor)]"),
                    ),
                    // message actions.
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-#[ink(message)]"),
                                    end_pat: Some("<-pub fn total_supply(&self)"),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Returns the total token supply."),
                                    end_pat: Some("<-/// Returns the account balance"),
                                }],
                            },
                        ],
                        Some("<-#[ink(message)]"),
                    ),
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(
                                        "<-#[ink(message)]\
                                                \n        pub fn balance_of(",
                                    ),
                                    end_pat: Some("<-pub fn balance_of("),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Returns the account balance"),
                                    end_pat: Some(
                                        "self.balance_of_impl(&owner)\
                                                \n        }\n\n        ",
                                    ),
                                }],
                            },
                        ],
                        Some(
                            "<-#[ink(message)]\
                                    \n        pub fn balance_of(",
                        ),
                    ),
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(
                                        "<-#[ink(message)]\
                                                \n        pub fn allowance(",
                                    ),
                                    end_pat: Some("<-pub fn allowance("),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Returns the amount which `spender`"),
                                    end_pat: Some("<-/// Returns the amount which `spender`->"),
                                }],
                            },
                        ],
                        Some(
                            "<-#[ink(message)]\
                                    \n        pub fn allowance(",
                        ),
                    ),
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(
                                        "<-#[ink(message)]\
                                                \n        pub fn transfer(",
                                    ),
                                    end_pat: Some("<-pub fn transfer("),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Transfers `value` amount of tokens"),
                                    end_pat: Some("<-/// Allows `spender` to withdraw from"),
                                }],
                            },
                        ],
                        Some(
                            "<-#[ink(message)]\
                                    \n        pub fn transfer(",
                        ),
                    ),
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(
                                        "<-#[ink(message)]\
                                                \n        pub fn approve(",
                                    ),
                                    end_pat: Some("<-pub fn approve("),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Allows `spender` to withdraw"),
                                    end_pat: Some("<-/// Transfers `value` tokens"),
                                }],
                            },
                        ],
                        Some(
                            "<-#[ink(message)]\
                                    \n        pub fn approve(",
                        ),
                    ),
                    (
                        vec![
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some(
                                        "<-#[ink(message)]\
                                                \n        pub fn transfer_from(",
                                    ),
                                    end_pat: Some("<-pub fn transfer_from("),
                                }],
                            },
                            TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Transfers `value` tokens"),
                                    end_pat: Some("<-/// Transfers `value` amount->"),
                                }],
                            },
                        ],
                        Some(
                            "<-#[ink(message)]\
                                    \n        pub fn transfer_from(",
                        ),
                    ),
                ],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(storage)]"),
                end_pat: Some("#[ink(storage)]"),
                replacement: "",
            }]),
            params: None,
            // missing storage.
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "#[ink(storage)]",
                            start_pat: Some("use ink::storage::Mapping;"),
                            end_pat: Some("use ink::storage::Mapping;"),
                        }],
                    }],
                    Some("<-mod erc20 {"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(constructor)]"),
                end_pat: Some("#[ink(constructor)]"),
                replacement: "",
            }]),
            params: None,
            // no constructor(s).
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "#[ink(constructor)]",
                            start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                            end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                        }],
                    }],
                    Some("<-mod erc20 {"),
                )],
            },
        },
        TestCase {
            modifications: Some(
                (1..=6)
                    .map(|_| TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "",
                    })
                    .collect(),
            ),
            params: None,
            // no message(s).
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "#[ink(message)]",
                            start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                            end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                        }],
                    }],
                    Some("<-mod erc20 {"),
                )],
            },
        },
    ];

    let trait_erc20 = [
        TestCase {
            modifications: None,
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 0,
                quickfixes: vec![],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
                replacement: "",
            }]),
            params: None,
            // 6 messages without a trait definition nor impl parent.
            results: TestCaseResults::Diagnostic {
                n: 6,
                quickfixes: vec![
                    (
                        vec![TestResultAction {
                            label: "Move",
                            edits: vec![
                                TestResultTextRange {
                                    text: "fn total_supply(&self) -> Balance;",
                                    start_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                    end_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Returns the total token supply."),
                                    end_pat: Some("<-/// Returns the account balance"),
                                },
                            ],
                        }],
                        Some("<-/// Returns the total token supply."),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Move",
                            edits: vec![
                                TestResultTextRange {
                                    text: "fn balance_of(&self, owner: AccountId) -> Balance;",
                                    start_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                    end_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Returns the account balance"),
                                    end_pat: Some("<-/// Returns the amount which `spender`"),
                                },
                            ],
                        }],
                        Some("<-/// Returns the account balance"),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Move",
                            edits: vec![
                                TestResultTextRange {
                                    text: "fn allowance(",
                                    start_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                    end_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Returns the amount which `spender`"),
                                    end_pat: Some("<-/// Transfers `value` amount of tokens"),
                                },
                            ],
                        }],
                        Some("<-/// Returns the amount which `spender`"),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Move",
                            edits: vec![
                                TestResultTextRange {
                                    text: "fn transfer(",
                                    start_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                    end_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Transfers `value` amount of tokens"),
                                    end_pat: Some("<-/// Allows `spender` to withdraw"),
                                },
                            ],
                        }],
                        Some("<-/// Transfers `value` amount of tokens"),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Move",
                            edits: vec![
                                TestResultTextRange {
                                    text: "fn approve(",
                                    start_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                    end_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Allows `spender` to withdraw"),
                                    end_pat: Some("<-/// Transfers `value` tokens"),
                                },
                            ],
                        }],
                        Some("<-/// Allows `spender` to withdraw"),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Move",
                            edits: vec![
                                TestResultTextRange {
                                    text: "fn transfer_from(",
                                    start_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                    end_pat: Some(
                                        "allowances: Default::default(),\
                                                \n            }\n        }",
                                    ),
                                },
                                TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-/// Transfers `value` tokens"),
                                    end_pat: Some("\n        ) -> Result<()>;"),
                                },
                            ],
                        }],
                        Some("<-/// Transfers `value` tokens"),
                    ),
                ],
            },
        },
        TestCase {
            modifications: Some(
                (1..=6)
                    .map(|_| TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "",
                    })
                    .collect(),
            ),
            params: None,
            // 1 trait level "missing message(s)", 6 method level "not a message" errors.
            results: TestCaseResults::Diagnostic {
                n: 7,
                quickfixes: vec![
                    // Add ink! message attribute to existing methods.
                    (
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("<-fn total_supply("),
                                end_pat: Some("<-fn total_supply("),
                            }],
                        }],
                        Some("<-fn total_supply("),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("<-fn balance_of("),
                                end_pat: Some("<-fn balance_of("),
                            }],
                        }],
                        Some("<-fn balance_of("),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("<-fn allowance("),
                                end_pat: Some("<-fn allowance("),
                            }],
                        }],
                        Some("<-fn allowance("),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("<-fn transfer("),
                                end_pat: Some("<-fn transfer("),
                            }],
                        }],
                        Some("<-fn transfer("),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("<-fn approve("),
                                end_pat: Some("<-fn approve("),
                            }],
                        }],
                        Some("<-fn approve("),
                    ),
                    (
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("<-fn transfer_from("),
                                end_pat: Some("<-fn transfer_from("),
                            }],
                        }],
                        Some("<-fn transfer_from("),
                    ),
                    // Add a new method.
                    (
                        vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(message)]",
                                start_pat: Some("\n        ) -> Result<()>;"),
                                end_pat: Some("\n        ) -> Result<()>;"),
                            }],
                        }],
                        Some("<-pub trait BaseErc20 {"),
                    ),
                ],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-/// Returns the total token supply.->"),
                end_pat: Some("self.total_supply\n        }"),
                replacement: "",
            }]),
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "fn total_supply(&self) -> Balance {",
                            start_pat: Some("<-\n    }\n\n    #[ink(impl)]"),
                            end_pat: Some("<-\n    }\n\n    #[ink(impl)]"),
                        }],
                    }],
                    Some("<-impl BaseErc20 for Erc20 {"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-/// Returns the total token supply."),
                end_pat: Some("fn total_supply(&self) -> Balance;"),
                replacement: "",
            }]),
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-/// Returns the total token supply.->"),
                            end_pat: Some(
                                "self.total_supply\
                                        \n        }\n\n        ",
                            ),
                        }],
                    }],
                    Some("<-/// Returns the total token supply.->"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-fn total_supply(&self)->"),
                end_pat: Some("fn total_supply(&self)->"),
                replacement: "fn total_supply(&mut self)",
            }]),
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Change",
                        edits: vec![TestResultTextRange {
                            text: "(&self)",
                            start_pat: Some("<-(&mut self)"),
                            end_pat: Some("(&mut self)"),
                        }],
                    }],
                    Some("fn total_supply(&mut self"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-fn total_supply(&self) -> Balance {"),
                end_pat: Some("fn total_supply(&self) -> Balance {"),
                replacement: "fn total_supply(&self) -> Result<Balance> {",
            }]),
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Change",
                        edits: vec![TestResultTextRange {
                            text: "-> Balance",
                            start_pat: Some("<--> Result<Balance>"),
                            end_pat: Some("-> Result<Balance>"),
                        }],
                    }],
                    Some("-> Result<Balance>"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some(
                    "/// Returns the total token supply.\
                            \n        ->",
                ),
                end_pat: Some(
                    "/// Returns the total token supply.\
                            \n        #[ink(message)]->",
                ),
                replacement: "#[ink(message, payable)]",
            }]),
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Remove",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-, payable"),
                            end_pat: Some(", payable"),
                        }],
                    }],
                    Some(", payable"),
                )],
            },
        },
    ];

    let psp22_extension = [
        TestCase {
            modifications: None,
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 0,
                quickfixes: vec![],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-type ErrorCode = Psp22Error;"),
                end_pat: Some("type ErrorCode = Psp22Error;"),
                replacement: "",
            }]),
            params: None,
            // missing `ErrorCode` type.
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "type ErrorCode = ();",
                            start_pat: Some("pub trait Psp22Extension {"),
                            end_pat: Some("pub trait Psp22Extension {"),
                        }],
                    }],
                    Some("<-pub trait Psp22Extension {"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract(env = crate::CustomEnvironment)]"),
                end_pat: Some("#[ink::contract(env = crate::CustomEnvironment)]"),
                replacement: "#[ink::contract(env = self::CustomEnvironment)]",
            }]),
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Replace",
                        edits: vec![TestResultTextRange {
                            text: "env = crate::CustomEnvironment",
                            start_pat: Some("<-env = self::CustomEnvironment"),
                            end_pat: Some("env = self::CustomEnvironment"),
                        }],
                    }],
                    Some("<-self::CustomEnvironment"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-impl Environment for CustomEnvironment {"),
                end_pat: Some("type ChainExtension = crate::Psp22Extension;\n}"),
                replacement: "",
            }]),
            params: None,
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "impl ink::env::Environment for CustomEnvironment {",
                            start_pat: Some("pub enum CustomEnvironment {}"),
                            end_pat: Some("pub enum CustomEnvironment {}"),
                        }],
                    }],
                    Some("<-pub enum CustomEnvironment {}"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-type ErrorCode = Psp22Error;"),
                end_pat: Some("type ErrorCode = Psp22Error;"),
                replacement: "type ErrorCode = ();",
            }]),
            params: None,
            // `ErrorCode` type `()` doesn't implement
            // `ink::env::chain_extension::FromStatusCode`.
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Replace",
                        edits: vec![TestResultTextRange {
                            text: "crate::Psp22Error",
                            start_pat: Some("type ErrorCode = "),
                            end_pat: Some("type ErrorCode = ()"),
                        }],
                    }],
                    Some("type ErrorCode = ()"),
                )],
            },
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("fn token_name(asset_id: u32) -> "),
                end_pat: Some("fn token_name(asset_id: u32) -> Result<Vec<u8>>"),
                replacement: "core::result::Result<Vec<u8>, Self::ErrorCode>",
            }]),
            params: None,
            // return type uses `Self::ErrorCode`.
            results: TestCaseResults::Diagnostic {
                n: 1,
                quickfixes: vec![(
                    vec![TestResultAction {
                        label: "Replace",
                        edits: vec![TestResultTextRange {
                            text: "crate::Psp22Error",
                            start_pat: Some("core::result::Result<Vec<u8>, "),
                            end_pat: Some("core::result::Result<Vec<u8>, Self::ErrorCode"),
                        }],
                    }],
                    Some("core::result::Result<Vec<u8>, Self::ErrorCode"),
                )],
            },
        },
    ];

    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract
            // in `test-fixtures/v5` directory.
            source: "v5/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: erc20.to_vec(),
        },
        TestGroup {
            source: "v4/erc20",
            test_cases: erc20.to_vec(),
        },
        // Events 2.0.
        TestGroup {
            source: "v5/events",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic {
                        n: 0,
                        quickfixes: vec![],
                    },
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::event(anonymous)]"),
                        end_pat: Some("#[ink::event(anonymous)]"),
                        replacement: "#[ink::event(\
                        anonymous, \
                        signature_topic=\
                        \"1111111111111111111111111111111111111111111111111111111111111111\"\
                        )]",
                    }]),
                    params: None,
                    results: TestCaseResults::Diagnostic {
                        n: 1,
                        quickfixes: vec![(
                            vec![TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("#[ink::event(anonymous"),
                                    end_pat: Some("<-)]\npub struct AnonymousEvent {"),
                                }],
                            }],
                            Some("<-signature_topic"),
                        )],
                    },
                },
            ],
        },
        TestGroup {
            source: "v5/events/event-def/src",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v5/events/event-def2/src",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v5/events/event-def-unused/src",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        // Wildcard and wildcard complement selectors.
        TestGroup {
            source: "v5/wildcard-selector",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic {
                        n: 0,
                        quickfixes: vec![],
                    },
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-, selector = _"),
                        end_pat: Some(", selector = _"),
                        replacement: "",
                    }]),
                    params: None,
                    results: TestCaseResults::Diagnostic {
                        n: 1,
                        quickfixes: vec![(
                            vec![TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-, selector = @"),
                                    end_pat: Some("selector = @"),
                                }],
                            }],
                            Some("<-selector = @"),
                        )],
                    },
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-/// Wildcard selector"),
                        end_pat: Some("<-/// Wildcard complement"),
                        replacement: "",
                    }]),
                    params: None,
                    results: TestCaseResults::Diagnostic {
                        n: 1,
                        quickfixes: vec![(
                            vec![TestResultAction {
                                label: "Remove",
                                edits: vec![TestResultTextRange {
                                    text: "",
                                    start_pat: Some("<-, selector = @"),
                                    end_pat: Some("selector = @"),
                                }],
                            }],
                            Some("<-selector = @"),
                        )],
                    },
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-, selector = @"),
                        end_pat: Some("selector = @"),
                        replacement: "",
                    }]),
                    params: None,
                    results: TestCaseResults::Diagnostic {
                        n: 1,
                        quickfixes: vec![(
                            vec![TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "selector = @",
                                    start_pat: Some("#[ink(message->"),
                                    end_pat: Some("#[ink(message->"),
                                }],
                            }],
                            Some("<-pub fn wildcard_complement("),
                        )],
                    },
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-/// Wildcard complement"),
                        end_pat: Some("_message);\n        }"),
                        replacement: "",
                    }]),
                    params: None,
                    results: TestCaseResults::Diagnostic {
                        n: 1,
                        quickfixes: vec![(
                            vec![TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "selector = @",
                                    start_pat: Some("_message\n            );\n        }"),
                                    end_pat: Some("_message\n            );\n        }"),
                                }],
                            }],
                            Some("<-pub mod wildcard_selector {"),
                        )],
                    },
                },
            ],
        },
        // e2e runtime-only backend.
        TestGroup {
            source: "v5/e2e-runtime-only-backend",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        // workspace contracts.
        TestGroup {
            source: "v5/workspace-contracts/flipper",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v4/workspace-contracts/flipper",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        // Trait definitions.
        TestGroup {
            source: "v5/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        TestGroup {
            source: "v4/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        // Chain extensions.
        TestGroup {
            source: "v5/psp22-extension",
            test_cases: psp22_extension
                .clone()
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink::chain_extension(extension = 13)]"),
                            end_pat: Some("#[ink::chain_extension(extension = 13)]"),
                            replacement: "#[ink::chain_extension]",
                        }]),
                        params: None,
                        // Missing `extension` argument.
                        results: TestCaseResults::Diagnostic {
                            n: 1,
                            quickfixes: vec![(
                                vec![TestResultAction {
                                    label: "Add",
                                    edits: vec![TestResultTextRange {
                                        text: "extension = 1",
                                        start_pat: Some("#[ink::chain_extension"),
                                        end_pat: Some("#[ink::chain_extension"),
                                    }],
                                }],
                                Some("<-#[ink::chain_extension]"),
                            )],
                        },
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(function = 0x3d26)]"),
                            end_pat: Some("#[ink(function = 0x3d26)]"),
                            replacement: "#[ink(extension = 0x3d26)]",
                        }]),
                        params: None,
                        // Replace `extension` for associated functions argument with `function`.
                        results: TestCaseResults::Diagnostic {
                            n: 1,
                            quickfixes: vec![(
                                vec![TestResultAction {
                                    label: "Replace",
                                    edits: vec![TestResultTextRange {
                                        text: "function",
                                        start_pat: Some("<-extension = 0x3d26"),
                                        end_pat: Some("<- = 0x3d26"),
                                    }],
                                }],
                                Some("<-extension = 0x3d26"),
                            )],
                        },
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink::chain_extension(extension = 13)]"),
                            end_pat: Some("#[ink::chain_extension(extension = 13)]"),
                            replacement: "",
                        }]),
                        params: None,
                        // 11 extensions without a chain extension parent.
                        results: TestCaseResults::Diagnostic {
                            n: 11,
                            quickfixes: vec![
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x3d26)]"),
                                                end_pat: Some("<-fn token_name(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x3d26)]"),
                                                end_pat: Some("<-#[ink(function = 0x3420)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x3d26)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x3420)]"),
                                                end_pat: Some("<-fn token_symbol(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x3420)]"),
                                                end_pat: Some("<-#[ink(function = 0x7271)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x3420)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x7271)]"),
                                                end_pat: Some("<-fn token_decimals(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x7271)]"),
                                                end_pat: Some("<-// PSP22 interface queries"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x7271)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x162d)]"),
                                                end_pat: Some("<-fn total_supply(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x162d)]"),
                                                end_pat: Some("<-#[ink(function = 0x6568)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x162d)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x6568)]"),
                                                end_pat: Some("<-fn balance_of("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x6568)]"),
                                                end_pat: Some("<-#[ink(function = 0x4d47)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x6568)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x4d47)]"),
                                                end_pat: Some("<-fn allowance("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x4d47)]"),
                                                end_pat: Some("<-// PSP22 transfer"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x4d47)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0xdb20)]"),
                                                end_pat: Some("<-fn transfer("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 transfer"),
                                                end_pat: Some("<-// PSP22 transfer_from"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0xdb20)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x54b3)]"),
                                                end_pat: Some("<-fn transfer_from("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 transfer_from"),
                                                end_pat: Some("<-// PSP22 approve"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x54b3)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0xb20f)]"),
                                                end_pat: Some("<-fn approve("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 approve"),
                                                end_pat: Some("<-// PSP22 increase_allowance"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0xb20f)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0x96d6)]"),
                                                end_pat: Some("<-fn increase_allowance("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 increase_allowance"),
                                                end_pat: Some("<-// PSP22 decrease_allowance"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0x96d6)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(function = 0xfecb)]"),
                                                end_pat: Some("<-fn decrease_allowance("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 decrease_allowance"),
                                                end_pat: Some(") -> Result<()>;->"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(function = 0xfecb)]"),
                                ),
                            ],
                        },
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink::scale_derive(Encode, Decode, TypeInfo)]"),
                            end_pat: Some("#[ink::scale_derive(Encode, Decode, TypeInfo)]"),
                            replacement: "",
                        }]),
                        params: None,
                        // Missing `scale::Encode`, `scale::Decode` and `scale_info::TypeInfo`
                        // implementations for `ErrorCode` type.
                        results: TestCaseResults::Diagnostic {
                            n: 1,
                            quickfixes: vec![(
                                vec![TestResultAction {
                                    label: "Derive",
                                    edits: vec![TestResultTextRange {
                                        text: "#[ink::scale_derive(Encode, Decode, TypeInfo)]",
                                        start_pat: Some("<-pub enum Psp22Error {"),
                                        end_pat: Some("<-pub enum Psp22Error {"),
                                    }],
                                }],
                                Some("<-pub enum Psp22Error {"),
                            )],
                        },
                    },
                ])
                .collect(),
        },
        TestGroup {
            source: "v4/psp22-extension",
            test_cases: psp22_extension
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink::chain_extension]"),
                            end_pat: Some("#[ink::chain_extension]"),
                            replacement: "",
                        }]),
                        params: None,
                        // 11 extensions without a chain extension parent.
                        results: TestCaseResults::Diagnostic {
                            n: 11,
                            quickfixes: vec![
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                                                end_pat: Some("<-fn token_name(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                                                end_pat: Some("<-#[ink(extension = 0x3420)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x3d26)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x3420)]"),
                                                end_pat: Some("<-fn token_symbol(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x3420)]"),
                                                end_pat: Some("<-#[ink(extension = 0x7271)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x3420)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x7271)]"),
                                                end_pat: Some("<-fn token_decimals(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x7271)]"),
                                                end_pat: Some("<-// PSP22 interface queries"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x7271)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x162d)]"),
                                                end_pat: Some("<-fn total_supply(asset_id: u32)"),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x162d)]"),
                                                end_pat: Some("<-#[ink(extension = 0x6568)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x162d)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x6568)]"),
                                                end_pat: Some("<-fn balance_of("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x6568)]"),
                                                end_pat: Some("<-#[ink(extension = 0x4d47)]"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x6568)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x4d47)]"),
                                                end_pat: Some("<-fn allowance("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x4d47)]"),
                                                end_pat: Some("<-// PSP22 transfer"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x4d47)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0xdb20)]"),
                                                end_pat: Some("<-fn transfer("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 transfer"),
                                                end_pat: Some("<-// PSP22 transfer_from"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0xdb20)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x54b3)]"),
                                                end_pat: Some("<-fn transfer_from("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 transfer_from"),
                                                end_pat: Some("<-// PSP22 approve"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x54b3)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0xb20f)]"),
                                                end_pat: Some("<-fn approve("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 approve"),
                                                end_pat: Some("<-// PSP22 increase_allowance"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0xb20f)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0x96d6)]"),
                                                end_pat: Some("<-fn increase_allowance("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 increase_allowance"),
                                                end_pat: Some("<-// PSP22 decrease_allowance"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0x96d6)]"),
                                ),
                                (
                                    vec![
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-#[ink(extension = 0xfecb)]"),
                                                end_pat: Some("<-fn decrease_allowance("),
                                            }],
                                        },
                                        TestResultAction {
                                            label: "Remove",
                                            edits: vec![TestResultTextRange {
                                                text: "",
                                                start_pat: Some("<-// PSP22 decrease_allowance"),
                                                end_pat: Some(") -> Result<()>;->"),
                                            }],
                                        },
                                    ],
                                    Some("<-#[ink(extension = 0xfecb)]"),
                                ),
                            ],
                        },
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some(
                                "<-#[derive(scale::Encode, scale::Decode)]\
                            \n#[cfg_attr(feature = \"std\", derive(scale_info::TypeInfo))]",
                            ),
                            end_pat: Some(
                                "#[derive(scale::Encode, scale::Decode)]\
                            \n#[cfg_attr(feature = \"std\", derive(scale_info::TypeInfo))]",
                            ),
                            replacement: "",
                        }]),
                        params: None,
                        // Missing `scale::Encode`, `scale::Decode` and `scale_info::TypeInfo`
                        // implementations for `ErrorCode` type.
                        results: TestCaseResults::Diagnostic {
                            n: 1,
                            quickfixes: vec![(
                                vec![TestResultAction {
                                    label: "Derive",
                                    edits: vec![TestResultTextRange {
                                        text: "#[derive(\
                                    scale::Encode, scale::Decode, scale_info::TypeInfo\
                                    )]",
                                        start_pat: Some("<-pub enum Psp22Error {"),
                                        end_pat: Some("<-pub enum Psp22Error {"),
                                    }],
                                }],
                                Some("<-pub enum Psp22Error {"),
                            )],
                        },
                    },
                ])
                .collect(),
        },
        TestGroup {
            source: "v5/combined-extension",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v5/rand-extension",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        // Storage items.
        TestGroup {
            source: "v5/storage_items/non_packed_tuple_struct",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v4/storage_items/non_packed_tuple_struct",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v5/storage_items/complex_non_packed_struct",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v4/storage_items/complex_non_packed_struct",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v4/storage_items/default_storage_key_1",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
        TestGroup {
            source: "v4/storage_items/complex_non_packed_enum",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic {
                    n: 0,
                    quickfixes: vec![],
                },
            }],
        },
    ]
}

/// Describes a collection of completions tests to run against optionally modified
/// ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn completions_fixtures() -> Vec<TestGroup> {
    let erc20 = [
        TestCase {
            // Replaces `#[ink::contract]` with `#[ink]` in the source code.
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: "#[ink]",
            }]),
            // Set the offset position at the end of the `#[ink` substring.
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink"),
            })),
            // Describes the expected completions.
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                // Declares expected completion text as `ink::contract`, applied to
                // the text range whose starting offset is the position at the beginning
                // of the `ink]` substring and end offset is the position at the end of
                // the `ink]` substring.
                text: "ink::contract",
                start_pat: Some("<-ink]"),
                end_pat: Some("#[ink"),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: "#[ink::contract()]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink::contract("),
            })),
            results: TestCaseResults::Completion(vec![
                TestResultTextRange {
                    text: "env=ink::env::DefaultEnvironment",
                    start_pat: Some("#[ink::contract("),
                    end_pat: Some("#[ink::contract("),
                },
                TestResultTextRange {
                    text: r#"keep_attr="""#,
                    start_pat: Some("#[ink::contract("),
                    end_pat: Some("#[ink::contract("),
                },
            ]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(storage)]"),
                end_pat: Some("#[ink(storage)]"),
                replacement: "#[ink(st)]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink(st"),
            })),
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                text: "storage",
                start_pat: Some("<-st)]"),
                end_pat: Some("#[ink(st"),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(event)]"),
                end_pat: Some("#[ink(event)]"),
                replacement: "#[ink(ev)]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink(ev"),
            })),
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                text: "event",
                start_pat: Some("<-ev)]"),
                end_pat: Some("#[ink(ev"),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(constructor)]"),
                end_pat: Some("#[ink(constructor)]"),
                replacement: "#[ink(con)]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink(con"),
            })),
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                text: "constructor",
                start_pat: Some("<-con)]"),
                end_pat: Some("#[ink(con"),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(constructor)]"),
                end_pat: Some("#[ink(constructor)]"),
                replacement: "#[ink(constructor,)]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink(constructor,"),
            })),
            results: TestCaseResults::Completion(vec![
                TestResultTextRange {
                    text: "default",
                    start_pat: Some("#[ink(constructor,"),
                    end_pat: Some("#[ink(constructor,"),
                },
                TestResultTextRange {
                    text: "payable",
                    start_pat: Some("#[ink(constructor,"),
                    end_pat: Some("#[ink(constructor,"),
                },
                TestResultTextRange {
                    text: "selector=1",
                    start_pat: Some("#[ink(constructor,"),
                    end_pat: Some("#[ink(constructor,"),
                },
            ]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(message)]"),
                end_pat: Some("#[ink(message)]"),
                replacement: "#[ink(me)]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink(me"),
            })),
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                text: "message",
                start_pat: Some("<-me)]"),
                end_pat: Some("#[ink(me"),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(message)]"),
                end_pat: Some("#[ink(message)]"),
                replacement: "#[ink(message,)]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink(message,"),
            })),
            results: TestCaseResults::Completion(vec![
                TestResultTextRange {
                    text: "default",
                    start_pat: Some("#[ink(message,"),
                    end_pat: Some("#[ink(message,"),
                },
                TestResultTextRange {
                    text: "payable",
                    start_pat: Some("#[ink(message,"),
                    end_pat: Some("#[ink(message,"),
                },
                TestResultTextRange {
                    text: "selector=1",
                    start_pat: Some("#[ink(message,"),
                    end_pat: Some("#[ink(message,"),
                },
            ]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::test]"),
                end_pat: Some("#[ink::test]"),
                replacement: "#[ink::te]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink::te"),
            })),
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                text: "test",
                start_pat: Some("<-te]"),
                end_pat: Some("#[ink::te"),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink_e2e::test]"),
                end_pat: Some("#[ink_e2e::test]"),
                replacement: "#[ink_e2e::te]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink_e2e::te"),
            })),
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                text: "test",
                start_pat: Some("<-te]"),
                end_pat: Some("#[ink_e2e::te"),
            }]),
        },
    ];

    let trait_erc20 = [
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
                replacement: "#[ink::tr]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink::tr"),
            })),
            results: TestCaseResults::Completion(vec![TestResultTextRange {
                text: "trait_definition",
                start_pat: Some("<-tr]"),
                end_pat: Some("#[ink::tr"),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
                replacement: "#[ink::trait_definition()]",
            }]),
            params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                pat: Some("#[ink::trait_definition("),
            })),
            results: TestCaseResults::Completion(vec![
                TestResultTextRange {
                    text: r#"keep_attr="""#,
                    start_pat: Some("#[ink::trait_definition("),
                    end_pat: Some("#[ink::trait_definition("),
                },
                TestResultTextRange {
                    text: r#"namespace="my_namespace""#,
                    start_pat: Some("#[ink::trait_definition("),
                    end_pat: Some("#[ink::trait_definition("),
                },
            ]),
        },
    ];

    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract
            // in `test-fixtures/v5` directory.
            source: "v5/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: erc20
                .clone()
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                            replacement: "#[ink(event,)]",
                        }]),
                        params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                            pat: Some("#[ink(event,"),
                        })),
                        results: TestCaseResults::Completion(vec![
                            TestResultTextRange {
                                text: "anonymous",
                                start_pat: Some("#[ink(event,"),
                                end_pat: Some("#[ink(event,"),
                            },
                            TestResultTextRange {
                                text: r#"signature_topic="""#,
                                start_pat: Some("#[ink(event,"),
                                end_pat: Some("#[ink(event,"),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test()]",
                        }]),
                        params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                            pat: Some("#[ink_e2e::test("),
                        })),
                        results: TestCaseResults::Completion(vec![
                            TestResultTextRange {
                                text: "backend(node)",
                                start_pat: Some("#[ink_e2e::test("),
                                end_pat: Some("#[ink_e2e::test("),
                            },
                            TestResultTextRange {
                                text: "environment=ink::env::DefaultEnvironment",
                                start_pat: Some("#[ink_e2e::test("),
                                end_pat: Some("#[ink_e2e::test("),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(backend)]",
                        }]),
                        params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                            pat: Some("#[ink_e2e::test(backend"),
                        })),
                        results: TestCaseResults::Completion(vec![
                            TestResultTextRange {
                                text: "(node)",
                                start_pat: Some("#[ink_e2e::test(backend"),
                                end_pat: Some("#[ink_e2e::test(backend"),
                            },
                            TestResultTextRange {
                                text: "(runtime_only)",
                                start_pat: Some("#[ink_e2e::test(backend"),
                                end_pat: Some("#[ink_e2e::test(backend"),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(backend(node))]",
                        }]),
                        params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                            pat: Some("#[ink_e2e::test(backend(node"),
                        })),
                        results: TestCaseResults::Completion(vec![TestResultTextRange {
                            text: r#"(url = "ws://127.0.0.1:9000")"#,
                            start_pat: Some("#[ink_e2e::test(backend(node"),
                            end_pat: Some("#[ink_e2e::test(backend(node"),
                        }]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(backend(runtime_only))]",
                        }]),
                        params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                            pat: Some("#[ink_e2e::test(backend(runtime_only"),
                        })),
                        results: TestCaseResults::Completion(vec![TestResultTextRange {
                            text: "(sandbox = ink_e2e::MinimalSandbox)",
                            start_pat: Some("#[ink_e2e::test(backend(runtime_only"),
                            end_pat: Some("#[ink_e2e::test(backend(runtime_only"),
                        }]),
                    },
                ])
                .collect(),
        },
        TestGroup {
            source: "v4/erc20",
            test_cases: erc20
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                            replacement: "#[ink(event,)]",
                        }]),
                        params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                            pat: Some("#[ink(event,"),
                        })),
                        results: TestCaseResults::Completion(vec![TestResultTextRange {
                            text: "anonymous",
                            start_pat: Some("#[ink(event,"),
                            end_pat: Some("#[ink(event,"),
                        }]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test()]",
                        }]),
                        params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                            pat: Some("#[ink_e2e::test("),
                        })),
                        results: TestCaseResults::Completion(vec![
                            TestResultTextRange {
                                text: r#"additional_contracts="""#,
                                start_pat: Some("#[ink_e2e::test("),
                                end_pat: Some("#[ink_e2e::test("),
                            },
                            TestResultTextRange {
                                text: "environment=ink::env::DefaultEnvironment",
                                start_pat: Some("#[ink_e2e::test("),
                                end_pat: Some("#[ink_e2e::test("),
                            },
                            TestResultTextRange {
                                text: r#"keep_attr="""#,
                                start_pat: Some("#[ink_e2e::test("),
                                end_pat: Some("#[ink_e2e::test("),
                            },
                        ]),
                    },
                ])
                .collect(),
        },
        // Events 2.0.
        TestGroup {
            source: "v5/events/event-def/src",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::event]"),
                        end_pat: Some("#[ink::event]"),
                        replacement: "#[ink::ev]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::ev"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "event",
                        start_pat: Some("#[ink::"),
                        end_pat: Some("#[ink::ev"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::event]"),
                        end_pat: Some("#[ink::event]"),
                        replacement: "#[ink::event()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::event("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "anonymous",
                            start_pat: Some("#[ink::event("),
                            end_pat: Some("#[ink::event("),
                        },
                        TestResultTextRange {
                            text: r#"signature_topic="""#,
                            start_pat: Some("#[ink::event("),
                            end_pat: Some("#[ink::event("),
                        },
                    ]),
                },
            ],
        },
        // Trait definitions.
        TestGroup {
            source: "v5/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        TestGroup {
            source: "v4/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        // Chain extensions.
        TestGroup {
            source: "v5/psp22-extension",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension(extension = 13)]"),
                        end_pat: Some("#[ink::chain_extension(extension = 13)]"),
                        replacement: "#[ink]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "ink::chain_extension",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                        TestResultTextRange {
                            text: "ink::trait_definition",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension(extension = 13)]"),
                        end_pat: Some("#[ink::chain_extension(extension = 13)]"),
                        replacement: "#[ink::chain_extension()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::chain_extension("),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "extension = 1",
                        start_pat: Some("#[ink::chain_extension("),
                        end_pat: Some("#[ink::chain_extension("),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(function = 0x3d26)]"),
                        end_pat: Some("#[ink(function = 0x3d26)]"),
                        replacement: "#[ink()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "function=1",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                        TestResultTextRange {
                            text: "handle_status=true",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(function = 0x3d26)]"),
                        end_pat: Some("#[ink(function = 0x3d26)]"),
                        replacement: "#[ink(function = 0x3d26,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(function = 0x3d26,"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "handle_status=true",
                        start_pat: Some("#[ink(function = 0x3d26,"),
                        end_pat: Some("#[ink(function = 0x3d26,"),
                    }]),
                },
            ],
        },
        TestGroup {
            source: "v4/psp22-extension",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "#[ink]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "ink::chain_extension",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                        TestResultTextRange {
                            text: "ink::trait_definition",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "#[ink::chain_extension()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::chain_extension("),
                    })),
                    results: TestCaseResults::Completion(vec![]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "#[ink()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "extension=1",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                        TestResultTextRange {
                            text: "handle_status=true",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "#[ink(extension = 0x3d26,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(extension = 0x3d26,"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "handle_status=true",
                        start_pat: Some("#[ink(extension = 0x3d26,"),
                        end_pat: Some("#[ink(extension = 0x3d26,"),
                    }]),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "v5/storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "#[ink]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "ink::event",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                        TestResultTextRange {
                            text: "ink::scale_derive",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                        TestResultTextRange {
                            text: "ink::storage_item",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "#[ink::storage_item()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::storage_item("),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "derive=true",
                        start_pat: Some("#[ink::storage_item("),
                        end_pat: Some("#[ink::storage_item("),
                    }]),
                },
            ],
        },
        TestGroup {
            source: "v4/storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "#[ink]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "ink::storage_item",
                        start_pat: Some("<-ink]"),
                        end_pat: Some("#[ink"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "#[ink::storage_item()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::storage_item("),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "derive=true",
                        start_pat: Some("#[ink::storage_item("),
                        end_pat: Some("#[ink::storage_item("),
                    }]),
                },
            ],
        },
    ]
}

/// Describes a collection of actions tests to run against optionally modified
/// ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn actions_fixtures() -> Vec<TestGroup> {
    let erc20 = [
        TestCase {
            // Removes `#[ink::contract]` from the source code.
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: "",
            }]),
            // Sets the offset position at the beginning of the `mod erc20` substring.
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-mod erc20"),
            })),
            // Describes the expected code/intent actions.
            results: TestCaseResults::Action(vec![
                // Declares expected code/intent action as one text edit, with text as
                // `#[ink::contract]`, applied to the text range whose starting offset
                // is the position at the beginning of the `mod erc20` substring and
                // end offset is also the position at the beginning of the `mod erc20`
                // substring.
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink::contract]",
                        start_pat: Some("<-mod erc20"),
                        end_pat: Some("<-mod erc20"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-#[ink::contract]"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "(env = ink::env::DefaultEnvironment)",
                        start_pat: Some("#[ink::contract"),
                        end_pat: Some("#[ink::contract"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: r#"(keep_attr = "")"#,
                        start_pat: Some("#[ink::contract"),
                        end_pat: Some("#[ink::contract"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-#[ink(storage)]"),
            })),
            results: TestCaseResults::Action(vec![]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(event)]"),
                end_pat: Some("#[ink(event)]"),
                replacement: "#[ink(event)]\n    #[ink(anonymous)]",
            }]),
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-pub struct Transfer"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Flatten",
                    edits: vec![
                        TestResultTextRange {
                            text: "#[ink(event, anonymous)]",
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(anonymous)]"),
                            end_pat: Some("<-pub struct Transfer"),
                        },
                    ],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(topic)]",
                        start_pat: Some("value: Balance,"),
                        end_pat: Some("value: Balance,"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-impl Erc20 {"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(impl)]",
                        start_pat: Some("<-impl Erc20 {"),
                        end_pat: Some("<-impl Erc20 {"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: r#"(namespace = "my_namespace")"#,
                        start_pat: Some("<-impl Erc20 {"),
                        end_pat: Some("<-impl Erc20 {"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(constructor)]",
                        start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                        end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(message)]",
                        start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                        end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(constructor)]"),
                end_pat: Some("#[ink(constructor)]"),
                replacement: "",
            }]),
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-pub fn new(total_supply: Balance)"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(constructor)]",
                        start_pat: Some("<-pub fn new(total_supply: Balance)"),
                        end_pat: Some("<-pub fn new(total_supply: Balance)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(default)]",
                        start_pat: Some("<-pub fn new(total_supply: Balance)"),
                        end_pat: Some("<-pub fn new(total_supply: Balance)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(message)]",
                        start_pat: Some("<-pub fn new(total_supply: Balance)"),
                        end_pat: Some("<-pub fn new(total_supply: Balance)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(payable)]",
                        start_pat: Some("<-pub fn new(total_supply: Balance)"),
                        end_pat: Some("<-pub fn new(total_supply: Balance)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(selector = 1)]",
                        start_pat: Some("<-pub fn new(total_supply: Balance)"),
                        end_pat: Some("<-pub fn new(total_supply: Balance)"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-#[ink(constructor)]"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: ", default",
                        start_pat: Some("#[ink(constructor"),
                        end_pat: Some("#[ink(constructor"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: ", payable",
                        start_pat: Some("#[ink(constructor"),
                        end_pat: Some("#[ink(constructor"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: ", selector = 1",
                        start_pat: Some("#[ink(constructor"),
                        end_pat: Some("#[ink(constructor"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(message)]"),
                end_pat: Some("#[ink(message)]"),
                replacement: "",
            }]),
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-pub fn total_supply(&self)"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(constructor)]",
                        start_pat: Some("<-pub fn total_supply(&self)"),
                        end_pat: Some("<-pub fn total_supply(&self)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(default)]",
                        start_pat: Some("<-pub fn total_supply(&self)"),
                        end_pat: Some("<-pub fn total_supply(&self)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(message)]",
                        start_pat: Some("<-pub fn total_supply(&self)"),
                        end_pat: Some("<-pub fn total_supply(&self)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(payable)]",
                        start_pat: Some("<-pub fn total_supply(&self)"),
                        end_pat: Some("<-pub fn total_supply(&self)"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(selector = 1)]",
                        start_pat: Some("<-pub fn total_supply(&self)"),
                        end_pat: Some("<-pub fn total_supply(&self)"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-#[ink(message)]"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: ", default",
                        start_pat: Some("#[ink(message"),
                        end_pat: Some("#[ink(message"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: ", payable",
                        start_pat: Some("#[ink(message"),
                        end_pat: Some("#[ink(message"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: ", selector = 1",
                        start_pat: Some("#[ink(message"),
                        end_pat: Some("#[ink(message"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-mod tests {"),
            })),
            results: TestCaseResults::Action(vec![TestResultAction {
                label: "Add",
                edits: vec![TestResultTextRange {
                    text: "#[ink::test]",
                    start_pat: Some(
                        "<-\n    }\
                                \n\n    #[cfg(all(test, feature = \"e2e-tests\"))]",
                    ),
                    end_pat: Some(
                        "<-\n    }\
                                \n\n    #[cfg(all(test, feature = \"e2e-tests\"))]",
                    ),
                }],
            }]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-#[ink::test]"),
            })),
            results: TestCaseResults::Action(vec![]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-mod e2e_tests {"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink::test]",
                        start_pat: Some("<-\n    }\n}->"),
                        end_pat: Some("<-\n    }\n}->"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink_e2e::test]",
                        start_pat: Some("<-\n    }\n}->"),
                        end_pat: Some("<-\n    }\n}->"),
                    }],
                },
            ]),
        },
    ];

    let trait_erc20 = [
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
                replacement: "",
            }]),
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-pub trait BaseErc20"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink::chain_extension]",
                        start_pat: Some("<-pub trait BaseErc20"),
                        end_pat: Some("<-pub trait BaseErc20"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink::trait_definition]",
                        start_pat: Some("<-pub trait BaseErc20"),
                        end_pat: Some("<-pub trait BaseErc20"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-#[ink::trait_definition]"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: r#"(keep_attr = "")"#,
                        start_pat: Some("#[ink::trait_definition"),
                        end_pat: Some("#[ink::trait_definition"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: r#"(namespace = "my_namespace")"#,
                        start_pat: Some("#[ink::trait_definition"),
                        end_pat: Some("#[ink::trait_definition"),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-pub trait BaseErc20 {"),
            })),
            results: TestCaseResults::Action(vec![
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: r#"(keep_attr = "")"#,
                        start_pat: Some("#[ink::trait_definition"),
                        end_pat: Some("#[ink::trait_definition"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: r#"(namespace = "my_namespace")"#,
                        start_pat: Some("#[ink::trait_definition"),
                        end_pat: Some("#[ink::trait_definition"),
                    }],
                },
                TestResultAction {
                    label: "Add",
                    edits: vec![TestResultTextRange {
                        text: "#[ink(message)]",
                        start_pat: Some(
                            "<-\n    }\
                                \n\n    /// A simple ERC-20 contract.",
                        ),
                        end_pat: Some(
                            "<-\n    }\
                                \n\n    /// A simple ERC-20 contract.",
                        ),
                    }],
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                pat: Some("<-impl BaseErc20 for Erc20 {"),
            })),
            results: TestCaseResults::Action(vec![TestResultAction {
                label: "Add",
                edits: vec![TestResultTextRange {
                    text: "#[ink(impl)]",
                    start_pat: Some("<-impl BaseErc20 for Erc20 {"),
                    end_pat: Some("<-impl BaseErc20 for Erc20 {"),
                }],
            }]),
        },
    ];

    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract
            // in `test-fixtures/v5` directory.
            source: "v5/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: erc20
                .clone()
                .into_iter()
                .chain([
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-mod erc20"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(env = ink::env::DefaultEnvironment)",
                                    start_pat: Some("#[ink::contract"),
                                    end_pat: Some("#[ink::contract"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("#[ink::contract"),
                                    end_pat: Some("#[ink::contract"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::event]",
                                    start_pat: Some("<-#[ink::contract]"),
                                    end_pat: Some("<-#[ink::contract]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                    end_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(constructor)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(message)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink::contract]"),
                            end_pat: Some("#[ink::contract]"),
                            replacement:
                                "#[ink::contract]\n#[ink(env=ink::env::DefaultEnvironment)]",
                        }]),
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-mod erc20"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("#[ink::contract"),
                                    end_pat: Some("#[ink::contract"),
                                }],
                            },
                            TestResultAction {
                                label: "Flatten",
                                edits: vec![
                                    TestResultTextRange {
                                        text:
                                            "#[ink::contract(env = ink::env::DefaultEnvironment)]",
                                        start_pat: Some("<-#[ink::contract]"),
                                        end_pat: Some("#[ink::contract]"),
                                    },
                                    TestResultTextRange {
                                        text: "",
                                        start_pat: Some(
                                            "<-#[ink(env=ink::env::DefaultEnvironment)]",
                                        ),
                                        end_pat: Some("#[ink(env=ink::env::DefaultEnvironment)]\n"),
                                    },
                                ],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::event]",
                                    start_pat: Some("<-#[ink::contract]"),
                                    end_pat: Some("<-#[ink::contract]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                    end_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(constructor)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(message)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("#[ink(storage)]"),
                            replacement: "",
                        }]),
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("pub struct Erc20"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::event]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::scale_derive]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::storage_item]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(anonymous)]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"#[ink(signature_topic = "")]"#,
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(storage)]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                            replacement: "",
                        }]),
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-pub struct Transfer"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::event]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::scale_derive]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::storage_item]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(anonymous)]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"#[ink(signature_topic = "")]"#,
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(storage)]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-#[ink(event)]"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: ", anonymous",
                                    start_pat: Some("#[ink(event"),
                                    end_pat: Some("#[ink(event"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#", signature_topic = """#,
                                    start_pat: Some("#[ink(event"),
                                    end_pat: Some("#[ink(event"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-#[ink_e2e::test]"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(backend(node))",
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(environment = ink::env::DefaultEnvironment)",
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-async fn e2e_transfer"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(backend(node))",
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(environment = ink::env::DefaultEnvironment)",
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                        ]),
                    },
                ])
                .collect(),
        },
        TestGroup {
            source: "v4/erc20",
            test_cases: erc20
                .into_iter()
                .chain([
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-mod erc20"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(env = ink::env::DefaultEnvironment)",
                                    start_pat: Some("#[ink::contract"),
                                    end_pat: Some("#[ink::contract"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("#[ink::contract"),
                                    end_pat: Some("#[ink::contract"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                    end_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(constructor)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(message)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink::contract]"),
                            end_pat: Some("#[ink::contract]"),
                            replacement:
                                "#[ink::contract]\n#[ink(env=ink::env::DefaultEnvironment)]",
                        }]),
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-mod erc20"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("#[ink::contract"),
                                    end_pat: Some("#[ink::contract"),
                                }],
                            },
                            TestResultAction {
                                label: "Flatten",
                                edits: vec![
                                    TestResultTextRange {
                                        text:
                                            "#[ink::contract(env = ink::env::DefaultEnvironment)]",
                                        start_pat: Some("<-#[ink::contract]"),
                                        end_pat: Some("#[ink::contract]"),
                                    },
                                    TestResultTextRange {
                                        text: "",
                                        start_pat: Some(
                                            "<-#[ink(env=ink::env::DefaultEnvironment)]",
                                        ),
                                        end_pat: Some("#[ink(env=ink::env::DefaultEnvironment)]\n"),
                                    },
                                ],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                    end_pat: Some("<-\n\n    /// The ERC-20 error types."),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(constructor)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(message)]",
                                    start_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                    end_pat: Some("<-\n    }\n\n    #[cfg(test)]"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("#[ink(storage)]"),
                            replacement: "",
                        }]),
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("pub struct Erc20"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::storage_item]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(anonymous)]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(storage)]",
                                    start_pat: Some("<-pub struct Erc20"),
                                    end_pat: Some("<-pub struct Erc20"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                            replacement: "",
                        }]),
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-pub struct Transfer"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink::storage_item]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(anonymous)]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(event)]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "#[ink(storage)]",
                                    start_pat: Some("<-pub struct Transfer"),
                                    end_pat: Some("<-pub struct Transfer"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-#[ink(event)]"),
                        })),
                        results: TestCaseResults::Action(vec![TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: ", anonymous",
                                start_pat: Some("#[ink(event"),
                                end_pat: Some("#[ink(event"),
                            }],
                        }]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-#[ink_e2e::test]"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(additional_contracts = "")"#,
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(environment = ink::env::DefaultEnvironment)",
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                        ]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                            pat: Some("<-async fn e2e_transfer"),
                        })),
                        results: TestCaseResults::Action(vec![
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(additional_contracts = "")"#,
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: "(environment = ink::env::DefaultEnvironment)",
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                            TestResultAction {
                                label: "Add",
                                edits: vec![TestResultTextRange {
                                    text: r#"(keep_attr = "")"#,
                                    start_pat: Some("#[ink_e2e::test"),
                                    end_pat: Some("#[ink_e2e::test"),
                                }],
                            },
                        ]),
                    },
                ])
                .collect(),
        },
        // Events 2.0.
        TestGroup {
            source: "v5/events/event-def/src",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::event]"),
                        end_pat: Some("#[ink::event]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub struct ForeignFlipped {"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::event]",
                                start_pat: Some("<-pub struct ForeignFlipped {"),
                                end_pat: Some("<-pub struct ForeignFlipped {"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::scale_derive]",
                                start_pat: Some("<-pub struct ForeignFlipped {"),
                                end_pat: Some("<-pub struct ForeignFlipped {"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::storage_item]",
                                start_pat: Some("<-pub struct ForeignFlipped {"),
                                end_pat: Some("<-pub struct ForeignFlipped {"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(anonymous)]",
                                start_pat: Some("<-pub struct ForeignFlipped {"),
                                end_pat: Some("<-pub struct ForeignFlipped {"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(event)]",
                                start_pat: Some("<-pub struct ForeignFlipped {"),
                                end_pat: Some("<-pub struct ForeignFlipped {"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#"#[ink(signature_topic = "")]"#,
                                start_pat: Some("<-pub struct ForeignFlipped {"),
                                end_pat: Some("<-pub struct ForeignFlipped {"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(storage)]",
                                start_pat: Some("<-pub struct ForeignFlipped {"),
                                end_pat: Some("<-pub struct ForeignFlipped {"),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::event]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "(anonymous)",
                                start_pat: Some("#[ink::event"),
                                end_pat: Some("#[ink::event"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#"(signature_topic = "")"#,
                                start_pat: Some("#[ink::event"),
                                end_pat: Some("#[ink::event"),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub struct ForeignFlipped {"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "(anonymous)",
                                start_pat: Some("#[ink::event"),
                                end_pat: Some("#[ink::event"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#"(signature_topic = "")"#,
                                start_pat: Some("#[ink::event"),
                                end_pat: Some("#[ink::event"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(topic)]",
                                start_pat: Some("pub value: bool,"),
                                end_pat: Some("pub value: bool,"),
                            }],
                        },
                    ]),
                },
            ],
        },
        // Trait definitions.
        TestGroup {
            source: "v5/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        TestGroup {
            source: "v4/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        // Chain extensions.
        TestGroup {
            source: "v5/psp22-extension",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension(extension = 13)]"),
                        end_pat: Some("#[ink::chain_extension(extension = 13)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub trait Psp22Extension {"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::chain_extension]",
                                start_pat: Some("<-pub trait Psp22Extension"),
                                end_pat: Some("<-pub trait Psp22Extension"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::trait_definition]",
                                start_pat: Some("<-pub trait Psp22Extension"),
                                end_pat: Some("<-pub trait Psp22Extension"),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::chain_extension(extension = 13)]"),
                    })),
                    results: TestCaseResults::Action(vec![]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub trait Psp22Extension {"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "#[ink(function = 1)]",
                            start_pat: Some(
                                "<-\n}\
                            \n\n#[derive(Debug, PartialEq, Eq)]",
                            ),
                            end_pat: Some(
                                "<-\n}\
                            \n\n#[derive(Debug, PartialEq, Eq)]",
                            ),
                        }],
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(function = 0x3d26)]"),
                        end_pat: Some("#[ink(function = 0x3d26)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-fn token_name(asset_id: u32)"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(function = 1)]",
                                start_pat: Some("<-fn token_name(asset_id: u32)"),
                                end_pat: Some("<-fn token_name(asset_id: u32)"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(handle_status = true)]",
                                start_pat: Some("<-fn token_name(asset_id: u32)"),
                                end_pat: Some("<-fn token_name(asset_id: u32)"),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink(function = 0x3d26)]"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: ", handle_status = true",
                            start_pat: Some("#[ink(function = 0x3d26"),
                            end_pat: Some("#[ink(function = 0x3d26"),
                        }],
                    }]),
                },
            ],
        },
        TestGroup {
            source: "v4/psp22-extension",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub trait Psp22Extension {"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::chain_extension]",
                                start_pat: Some("<-pub trait Psp22Extension"),
                                end_pat: Some("<-pub trait Psp22Extension"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::trait_definition]",
                                start_pat: Some("<-pub trait Psp22Extension"),
                                end_pat: Some("<-pub trait Psp22Extension"),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::chain_extension]"),
                    })),
                    results: TestCaseResults::Action(vec![]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub trait Psp22Extension {"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "#[ink(extension = 1)]",
                            start_pat: Some(
                                "<-\n}\
                            \n\n#[derive(scale::Encode, scale::Decode)]",
                            ),
                            end_pat: Some(
                                "<-\n}\
                            \n\n#[derive(scale::Encode, scale::Decode)]",
                            ),
                        }],
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-fn token_name(asset_id: u32)"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(extension = 1)]",
                                start_pat: Some("<-fn token_name(asset_id: u32)"),
                                end_pat: Some("<-fn token_name(asset_id: u32)"),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(handle_status = true)]",
                                start_pat: Some("<-fn token_name(asset_id: u32)"),
                                end_pat: Some("<-fn token_name(asset_id: u32)"),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink(extension = 0x3d26)]"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: ", handle_status = true",
                            start_pat: Some("#[ink(extension = 0x3d26"),
                            end_pat: Some("#[ink(extension = 0x3d26"),
                        }],
                    }]),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "v5/storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-struct Contract("),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::event]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::scale_derive]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::storage_item]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(anonymous)]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(event)]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: r#"#[ink(signature_topic = "")]"#,
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(storage)]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::storage_item]"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "(derive = true)",
                            start_pat: Some("#[ink::storage_item"),
                            end_pat: Some("#[ink::storage_item"),
                        }],
                    }]),
                },
            ],
        },
        TestGroup {
            source: "v4/storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-struct Contract("),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink::storage_item]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(anonymous)]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(event)]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                        TestResultAction {
                            label: "Add",
                            edits: vec![TestResultTextRange {
                                text: "#[ink(storage)]",
                                start_pat: Some("<-struct Contract("),
                                end_pat: Some("<-struct Contract("),
                            }],
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::storage_item]"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "(derive = true)",
                            start_pat: Some("#[ink::storage_item"),
                            end_pat: Some("#[ink::storage_item"),
                        }],
                    }]),
                },
            ],
        },
    ]
}

/// Describes a collection of hover content tests to run against optionally modified
/// ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn hover_fixtures() -> Vec<TestGroup> {
    let erc20 = [
        TestCase {
            // Makes no modifications to the source code.
            modifications: None,
            // Sets the text range for the hover to span the whole
            // `#[ink::contract]` substring.
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
            })),
            // Describes the expected hover content.
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                // Expects the hover content to contain the substring "`#[ink::contract]`"
                // and to highlight the text range whose starting offset is the position
                // at the beginning of the `contract]` substring and end offset is
                // the position at the end of the `#[ink::contract` substring.
                text: "`#[ink::contract]`",
                start_pat: Some("<-contract]"),
                end_pat: Some("#[ink::contract"),
            })),
        },
        TestCase {
            // Replaces `#[ink::contract]` with `#[ink::contract(env=MyEnvironment)]`
            // in the source code.
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: "#[ink::contract(env=MyEnvironment)]",
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink::contract("),
                end_pat: Some("#[ink::contract(env"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::contract(env = E: impl Environment)]`",
                start_pat: Some("#[ink::contract("),
                end_pat: Some("#[ink::contract(env"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: r#"#[ink::contract(keep_attr="foo,bar")]"#,
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink::contract("),
                end_pat: Some("#[ink::contract(keep_attr"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::contract(keep_attr = N: string)]`",
                start_pat: Some("#[ink::contract("),
                end_pat: Some("#[ink::contract(keep_attr"),
            })),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink(storage)]"),
                end_pat: Some("#[ink(storage)]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink(storage)]`",
                start_pat: Some("<-storage)]"),
                end_pat: Some("#[ink(storage"),
            })),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink(event)]"),
                end_pat: Some("#[ink(event)]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink(event)]`",
                start_pat: Some("<-event)]"),
                end_pat: Some("#[ink(event"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(event)]"),
                end_pat: Some("#[ink(event)]"),
                replacement: "#[ink(event, anonymous)]",
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink(event, "),
                end_pat: Some("#[ink(event, anonymous"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink(anonymous)]`",
                start_pat: Some("#[ink(event, "),
                end_pat: Some("#[ink(event, anonymous"),
            })),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink(constructor)]"),
                end_pat: Some("#[ink(constructor)]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink(constructor)]`",
                start_pat: Some("<-constructor)]"),
                end_pat: Some("#[ink(constructor"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(constructor)]"),
                end_pat: Some("#[ink(constructor)]"),
                replacement: "#[ink(constructor, default)]",
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink(constructor, "),
                end_pat: Some("#[ink(constructor, default"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink(default)]`",
                start_pat: Some("#[ink(constructor, "),
                end_pat: Some("#[ink(constructor, default"),
            })),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink(message)]"),
                end_pat: Some("#[ink(message)]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink(message)]`",
                start_pat: Some("<-message)]"),
                end_pat: Some("#[ink(message"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(message)]"),
                end_pat: Some("#[ink(message)]"),
                replacement: "#[ink(message, payable)]",
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink(message, "),
                end_pat: Some("#[ink(message, payable"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink(payable)]`",
                start_pat: Some("#[ink(message, "),
                end_pat: Some("#[ink(message, payable"),
            })),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink::test]"),
                end_pat: Some("#[ink::test]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::test]`",
                start_pat: Some("<-test]"),
                end_pat: Some("#[ink::test"),
            })),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink_e2e::test]"),
                end_pat: Some("#[ink_e2e::test]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink_e2e::test]`",
                start_pat: Some("#[ink_e2e::"),
                end_pat: Some("#[ink_e2e::test"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink_e2e::test]"),
                end_pat: Some("#[ink_e2e::test]"),
                replacement: "#[ink_e2e::test(environment=MyEnvironment)]",
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink_e2e::test("),
                end_pat: Some("#[ink_e2e::test(environment"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink_e2e::test(environment = E: impl Environment)]`",
                start_pat: Some("#[ink_e2e::test("),
                end_pat: Some("#[ink_e2e::test(environment"),
            })),
        },
    ];

    let trait_erc20 = [
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::trait_definition]`",
                start_pat: Some("<-trait_definition]"),
                end_pat: Some("#[ink::trait_definition"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
                replacement: r#"#[ink::trait_definition(keep_attr="foo,bar")]"#,
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink::trait_definition("),
                end_pat: Some("#[ink::trait_definition(keep_attr"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::trait_definition(keep_attr = N: string)]`",
                start_pat: Some("#[ink::trait_definition("),
                end_pat: Some("#[ink::trait_definition(keep_attr"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
                replacement: r#"#[ink::trait_definition(namespace="my_namespace")]"#,
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink::trait_definition("),
                end_pat: Some("#[ink::trait_definition(namespace"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::trait_definition(namespace = N: string)]`",
                start_pat: Some("#[ink::trait_definition("),
                end_pat: Some("#[ink::trait_definition(namespace"),
            })),
        },
    ];

    let storage_item = [
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("<-#[ink::storage_item]"),
                end_pat: Some("#[ink::storage_item]"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::storage_item]`",
                start_pat: Some("<-storage_item]"),
                end_pat: Some("#[ink::storage_item"),
            })),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::storage_item]"),
                end_pat: Some("#[ink::storage_item]"),
                replacement: r#"#[ink::storage_item(derive=false)]"#,
            }]),
            params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                start_pat: Some("#[ink::storage_item("),
                end_pat: Some("#[ink::storage_item(derive"),
            })),
            results: TestCaseResults::Hover(Some(TestResultTextRange {
                text: "`#[ink::storage_item(derive = flag: bool)]`",
                start_pat: Some("#[ink::storage_item("),
                end_pat: Some("#[ink::storage_item(derive"),
            })),
        },
    ];

    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract
            // in `test-fixtures/v5` directory.
            source: "v5/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: erc20
                .clone()
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=_)]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink(selector = S: u32 | _ | @)]`",
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=@)]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink(selector = S: u32 | _ | @)]`",
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=1)]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink(selector = S: u32 | _ | @)]`",
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=0xA)]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink(selector = S: u32 | _ | @)]`",
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(backend(node))]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(backend"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink_e2e::test(backend(node|runtime_only))]`",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(backend"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(backend(node))]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test(backend("),
                            end_pat: Some("#[ink_e2e::test(backend(node"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink_e2e::test(backend(node|runtime_only))]`",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(backend"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            backend(node(url = \"ws://127.0.0.1:8000\")))]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test(backend(node("),
                            end_pat: Some("#[ink_e2e::test(backend(node(url"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink_e2e::test(backend(node(url = U: string))]`",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(backend"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(backend(runtime_only))]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test(backend("),
                            end_pat: Some("#[ink_e2e::test(backend(runtime_only"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink_e2e::test(backend(node|runtime_only))]`",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(backend"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            backend(runtime_only(sandbox = ink_e2e::MinimalSandbox)))]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test(backend(runtime_only("),
                            end_pat: Some("#[ink_e2e::test(backend(runtime_only(sandbox"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink_e2e::test(\
                            backend(runtime_only(sandbox = S: impl drink::Sandbox))]`",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(backend"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\"\
                            )]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(additional_contracts"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`additional_contracts` is deprecated",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(additional_contracts"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: r#"#[ink_e2e::test(keep_attr="foo,bar")]"#,
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(keep_attr"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`keep_attr` for `ink_e2e::test` attribute macro is deprecated",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(keep_attr"),
                        })),
                    },
                ])
                .collect(),
        },
        TestGroup {
            source: "v4/erc20",
            test_cases: erc20
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=_)]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink(selector = S: u32 | _)]`",
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=1)]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink(selector = S: u32 | _)]`",
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=0xA)]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink(selector = S: u32 | _)]`",
                            start_pat: Some("#[ink(message, "),
                            end_pat: Some("#[ink(message, selector"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\"\
                            )]",
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(additional_contracts"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink_e2e::test(additional_contracts = S: string)]`",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(additional_contracts"),
                        })),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: r#"#[ink_e2e::test(keep_attr="foo,bar")]"#,
                        }]),
                        params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(keep_attr"),
                        })),
                        results: TestCaseResults::Hover(Some(TestResultTextRange {
                            text: "`#[ink_e2e::test(keep_attr = N: string)]`",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test(keep_attr"),
                        })),
                    },
                ])
                .collect(),
        },
        // Events 2.0.
        TestGroup {
            source: "v5/events/event-def/src",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::event]"),
                        end_pat: Some("#[ink::event]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::event]`",
                        start_pat: Some("<-event]"),
                        end_pat: Some("#[ink::event"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::event]"),
                        end_pat: Some("#[ink::event]"),
                        replacement: "#[ink::event(anonymous)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink::event("),
                        end_pat: Some("#[ink::event(anonymous"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(anonymous)]`",
                        start_pat: Some("#[ink::event("),
                        end_pat: Some("#[ink::event(anonymous"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::event]"),
                        end_pat: Some("#[ink::event]"),
                        replacement: "#[ink::event(signature_topic = \
                        \"1111111111111111111111111111111111111111111111111111111111111111\")]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink::event("),
                        end_pat: Some("#[ink::event(signature_topic"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::event(signature_topic = S: string)]`",
                        start_pat: Some("#[ink::event("),
                        end_pat: Some("#[ink::event(signature_topic"),
                    })),
                },
            ],
        },
        // Trait definitions.
        TestGroup {
            source: "v5/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        TestGroup {
            source: "v4/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        // Chain extensions.
        TestGroup {
            source: "v5/psp22-extension",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::chain_extension"),
                        end_pat: Some("#[ink::chain_extension"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::chain_extension]`",
                        start_pat: Some("<-chain_extension"),
                        end_pat: Some("#[ink::chain_extension"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-extension = 13"),
                        end_pat: Some("extension = 13"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::chain_extension(extension = N: u16)]`",
                        start_pat: Some("<-extension = 13"),
                        end_pat: Some("#[ink::chain_extension(extension"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink(function = 0x3d26)]"),
                        end_pat: Some("#[ink(function = 0x3d26)]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(function = N: u16)]`",
                        start_pat: Some("<-function = 0x3d26)]"),
                        end_pat: Some("#[ink(function"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(function = 0x3d26)]"),
                        end_pat: Some("#[ink(function = 0x3d26)]"),
                        replacement: "#[ink(function = 0x3d26, handle_status=true)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(function = 0x3d26, "),
                        end_pat: Some("#[ink(function = 0x3d26, handle_status"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(handle_status = flag: bool)]`",
                        start_pat: Some("#[ink(function = 0x3d26, "),
                        end_pat: Some("#[ink(function = 0x3d26, handle_status"),
                    })),
                },
            ],
        },
        TestGroup {
            source: "v4/psp22-extension",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::chain_extension]`",
                        start_pat: Some("<-chain_extension]"),
                        end_pat: Some("#[ink::chain_extension"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(extension = N: u32)]`",
                        start_pat: Some("<-extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "#[ink(extension = 0x3d26, handle_status=true)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(extension = 0x3d26, "),
                        end_pat: Some("#[ink(extension = 0x3d26, handle_status"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(handle_status = flag: bool)]`",
                        start_pat: Some("#[ink(extension = 0x3d26, "),
                        end_pat: Some("#[ink(extension = 0x3d26, handle_status"),
                    })),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "v5/storage_items/non_packed_tuple_struct",
            test_cases: storage_item.to_vec(),
        },
        TestGroup {
            source: "v4/storage_items/non_packed_tuple_struct",
            test_cases: storage_item.to_vec(),
        },
    ]
}

/// Describes a collection of inlay hints tests to run against optionally modified
/// ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn inlay_hints_fixtures() -> Vec<TestGroup> {
    let erc20 = [
        TestCase {
            // Makes no modifications to the source code.
            modifications: None,
            // Sets the selection range to span the contents of the whole file.
            // NOTE: `Some(TestCaseParams::InlayHints(None))` would also work in this case.
            params: Some(TestCaseParams::InlayHints(Some(TestParamsRangeOnly {
                start_pat: Some("<-"),
                end_pat: Some("->"),
            }))),
            // Describes the expected results.
            results: TestCaseResults::InlayHints(vec![]),
        },
        TestCase {
            // Replaces `#[ink::contract]` with
            // `#[ink::contract(env=MyEnvironment, keep_attr="foo,bar")]`
            // in the source code.
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: r#"#[ink::contract(env=MyEnvironment, keep_attr="foo,bar")]"#,
            }]),
            params: Some(TestCaseParams::InlayHints(None)),
            // Expects the inlay hints for the `env` and `keep_attr`
            // ink! attribute arguments positioned at the end of each of argument's name,
            // with the argument's name being the text range the inlay hint applies to.
            results: TestCaseResults::InlayHints(vec![
                // env.
                TestResultTextOffsetRange {
                    text: "impl Environment",
                    pos_pat: Some("#[ink::contract(env"),
                    range_start_pat: Some("#[ink::contract("),
                    range_end_pat: Some("#[ink::contract(env"),
                },
                // keep_attr.
                TestResultTextOffsetRange {
                    text: "&str",
                    pos_pat: Some("#[ink::contract(env=MyEnvironment, keep_attr"),
                    range_start_pat: Some("#[ink::contract(env=MyEnvironment, "),
                    range_end_pat: Some("#[ink::contract(env=MyEnvironment, keep_attr"),
                },
            ]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink(constructor)]"),
                end_pat: Some("#[ink(constructor)]"),
                replacement: "#[ink(constructor, selector=_)]",
            }]),
            params: Some(TestCaseParams::InlayHints(None)),
            results: TestCaseResults::InlayHints(vec![
                // selector.
                TestResultTextOffsetRange {
                    text: "u32 | _",
                    pos_pat: Some("<-=_"),
                    range_start_pat: Some("<-selector=_"),
                    range_end_pat: Some("<-=_"),
                },
            ]),
        },
    ];

    let trait_erc20 = [
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::InlayHints(None)),
            results: TestCaseResults::InlayHints(vec![]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::trait_definition]"),
                end_pat: Some("#[ink::trait_definition]"),
                replacement: "#[ink::trait_definition(\
                        namespace=\"my_namespace\", \
                        keep_attr=\"foo,bar\"\
                        )]",
            }]),
            params: Some(TestCaseParams::InlayHints(None)),
            results: TestCaseResults::InlayHints(vec![
                // namespace.
                TestResultTextOffsetRange {
                    text: "&str",
                    pos_pat: Some("#[ink::trait_definition(namespace"),
                    range_start_pat: Some("#[ink::trait_definition("),
                    range_end_pat: Some("#[ink::trait_definition(namespace"),
                },
                // keep_attr.
                TestResultTextOffsetRange {
                    text: "&str",
                    pos_pat: Some(
                        "#[ink::trait_definition(\
                                namespace=\"my_namespace\", \
                                keep_attr",
                    ),
                    range_start_pat: Some(
                        "#[ink::trait_definition(\
                                namespace=\"my_namespace\", ",
                    ),
                    range_end_pat: Some(
                        "#[ink::trait_definition(\
                                namespace=\"my_namespace\", \
                                keep_attr",
                    ),
                },
            ]),
        },
    ];

    let storage_item = [
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::InlayHints(None)),
            results: TestCaseResults::InlayHints(vec![]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::storage_item]"),
                end_pat: Some("#[ink::storage_item]"),
                replacement: r#"#[ink::storage_item(derive=false)]"#,
            }]),
            params: Some(TestCaseParams::InlayHints(None)),
            results: TestCaseResults::InlayHints(vec![
                // derive.
                TestResultTextOffsetRange {
                    text: "bool",
                    pos_pat: Some("#[ink::storage_item(derive"),
                    range_start_pat: Some("#[ink::storage_item("),
                    range_end_pat: Some("#[ink::storage_item(derive"),
                },
            ]),
        },
    ];

    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract
            // in `test-fixtures/v5` directory.
            source: "v5/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: erc20
                .clone()
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=_)]",
                        }]),
                        params: Some(TestCaseParams::InlayHints(None)),
                        results: TestCaseResults::InlayHints(vec![
                            // selector.
                            TestResultTextOffsetRange {
                                text: "u32 | _ | @",
                                pos_pat: Some("<-=_"),
                                range_start_pat: Some("<-selector=_"),
                                range_end_pat: Some("<-=_"),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            backend(node), \
                            environment=MyEnvironment\
                            )]",
                        }]),
                        params: Some(TestCaseParams::InlayHints(None)),
                        results: TestCaseResults::InlayHints(vec![
                            // backend.
                            TestResultTextOffsetRange {
                                text: "node | runtime_only",
                                pos_pat: Some("#[ink_e2e::test(backend"),
                                range_start_pat: Some("#[ink_e2e::test("),
                                range_end_pat: Some("#[ink_e2e::test(backend"),
                            },
                            // environment.
                            TestResultTextOffsetRange {
                                text: "impl Environment",
                                pos_pat: Some(
                                    "#[ink_e2e::test(\
                                    backend(node), \
                                    environment",
                                ),
                                range_start_pat: Some(
                                    "#[ink_e2e::test(\
                                    backend(node), ",
                                ),
                                range_end_pat: Some(
                                    "#[ink_e2e::test(\
                                    backend(node), \
                                    environment",
                                ),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            backend(node(url = \"ws://127.0.0.1:8000\")))]",
                        }]),
                        params: Some(TestCaseParams::InlayHints(None)),
                        results: TestCaseResults::InlayHints(vec![
                            // backend.
                            TestResultTextOffsetRange {
                                text: "node | runtime_only",
                                pos_pat: Some("#[ink_e2e::test(backend"),
                                range_start_pat: Some("#[ink_e2e::test("),
                                range_end_pat: Some("#[ink_e2e::test(backend"),
                            },
                            // url.
                            TestResultTextOffsetRange {
                                text: "&str",
                                pos_pat: Some("#[ink_e2e::test(backend(node(url"),
                                range_start_pat: Some("#[ink_e2e::test(backend(node("),
                                range_end_pat: Some("#[ink_e2e::test(backend(node(url"),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            backend(node(url = \"ws://127.0.0.1:8000\")))]",
                        }]),
                        params: Some(TestCaseParams::InlayHints(None)),
                        results: TestCaseResults::InlayHints(vec![
                            // backend.
                            TestResultTextOffsetRange {
                                text: "node | runtime_only",
                                pos_pat: Some("#[ink_e2e::test(backend"),
                                range_start_pat: Some("#[ink_e2e::test("),
                                range_end_pat: Some("#[ink_e2e::test(backend"),
                            },
                            // url.
                            TestResultTextOffsetRange {
                                text: "&str",
                                pos_pat: Some("#[ink_e2e::test(backend(node(url"),
                                range_start_pat: Some("#[ink_e2e::test(backend(node("),
                                range_end_pat: Some("#[ink_e2e::test(backend(node(url"),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            backend(runtime_only(sandbox = ink_e2e::MinimalSandbox)))]",
                        }]),
                        params: Some(TestCaseParams::InlayHints(None)),
                        results: TestCaseResults::InlayHints(vec![
                            // backend.
                            TestResultTextOffsetRange {
                                text: "node | runtime_only",
                                pos_pat: Some("#[ink_e2e::test(backend"),
                                range_start_pat: Some("#[ink_e2e::test("),
                                range_end_pat: Some("#[ink_e2e::test(backend"),
                            },
                            // sandbox.
                            TestResultTextOffsetRange {
                                text: "impl drink::Sandbox",
                                pos_pat: Some("#[ink_e2e::test(backend(runtime_only(sandbox"),
                                range_start_pat: Some("#[ink_e2e::test(backend(runtime_only("),
                                range_end_pat: Some("#[ink_e2e::test(backend(runtime_only(sandbox"),
                            },
                        ]),
                    },
                ])
                .collect(),
        },
        TestGroup {
            source: "v4/erc20",
            test_cases: erc20
                .into_iter()
                .chain([
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "#[ink(message, selector=_)]",
                        }]),
                        params: Some(TestCaseParams::InlayHints(None)),
                        results: TestCaseResults::InlayHints(vec![
                            // selector.
                            TestResultTextOffsetRange {
                                text: "u32 | _",
                                pos_pat: Some("<-=_"),
                                range_start_pat: Some("<-selector=_"),
                                range_end_pat: Some("<-=_"),
                            },
                        ]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test(\
                            additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\", \
                            environment=MyEnvironment, \
                            keep_attr=\"foo,bar\"\
                            )]",
                        }]),
                        params: Some(TestCaseParams::InlayHints(None)),
                        results: TestCaseResults::InlayHints(vec![
                            // additional_contracts.
                            TestResultTextOffsetRange {
                                text: "&str",
                                pos_pat: Some("#[ink_e2e::test(additional_contracts"),
                                range_start_pat: Some("#[ink_e2e::test("),
                                range_end_pat: Some("#[ink_e2e::test(additional_contracts"),
                            },
                            // environment.
                            TestResultTextOffsetRange {
                                text: "impl Environment",
                                pos_pat: Some(
                                    "#[ink_e2e::test(\
                                    additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\", \
                                    environment",
                                ),
                                range_start_pat: Some(
                                    "#[ink_e2e::test(\
                                    additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\", ",
                                ),
                                range_end_pat: Some(
                                    "#[ink_e2e::test(\
                                    additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\", \
                                    environment",
                                ),
                            },
                            // keep_attr.
                            TestResultTextOffsetRange {
                                text: "&str",
                                pos_pat: Some(
                                    "#[ink_e2e::test(\
                                    additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\", \
                                    environment=MyEnvironment, \
                                    keep_attr",
                                ),
                                range_start_pat: Some(
                                    "#[ink_e2e::test(\
                                    additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\", \
                                    environment=MyEnvironment, ",
                                ),
                                range_end_pat: Some(
                                    "#[ink_e2e::test(\
                                    additional_contracts=\"adder/Cargo.toml flipper/Cargo.toml\", \
                                    environment=MyEnvironment, \
                                    keep_attr",
                                ),
                            },
                        ]),
                    },
                ])
                .collect(),
        },
        // Events 2.0.
        TestGroup {
            source: "v5/events",
            test_cases: vec![TestCase {
                modifications: None,
                params: Some(TestCaseParams::InlayHints(None)),
                results: TestCaseResults::InlayHints(vec![TestResultTextOffsetRange {
                    text: "&str",
                    pos_pat: Some("signature_topic"),
                    range_start_pat: Some("<-signature_topic"),
                    range_end_pat: Some("signature_topic"),
                }]),
            }],
        },
        // Trait definitions.
        TestGroup {
            source: "v5/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        TestGroup {
            source: "v4/trait-erc20",
            test_cases: trait_erc20.to_vec(),
        },
        // Chain extensions.
        TestGroup {
            source: "v5/psp22-extension",
            test_cases: vec![TestCase {
                modifications: None,
                params: Some(TestCaseParams::InlayHints(None)),
                results: TestCaseResults::InlayHints(vec![
                    // extension id.
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 13"),
                        range_start_pat: Some("<-extension = 13"),
                        range_end_pat: Some("<- = 13"),
                    },
                    // functions.
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x3d26"),
                        range_start_pat: Some("<-function = 0x3d26"),
                        range_end_pat: Some("<- = 0x3d26"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x3420"),
                        range_start_pat: Some("<-function = 0x3420"),
                        range_end_pat: Some("<- = 0x3420"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x7271"),
                        range_start_pat: Some("<-function = 0x7271"),
                        range_end_pat: Some("<- = 0x7271"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x162d"),
                        range_start_pat: Some("<-function = 0x162d"),
                        range_end_pat: Some("<- = 0x162d"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x6568"),
                        range_start_pat: Some("<-function = 0x6568"),
                        range_end_pat: Some("<- = 0x6568"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x4d47"),
                        range_start_pat: Some("<-function = 0x4d47"),
                        range_end_pat: Some("<- = 0x4d47"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0xdb20"),
                        range_start_pat: Some("<-function = 0xdb20"),
                        range_end_pat: Some("<- = 0xdb20"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x54b3"),
                        range_start_pat: Some("<-function = 0x54b3"),
                        range_end_pat: Some("<- = 0x54b3"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0xb20f"),
                        range_start_pat: Some("<-function = 0xb20f"),
                        range_end_pat: Some("<- = 0xb20f"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0x96d6"),
                        range_start_pat: Some("<-function = 0x96d6"),
                        range_end_pat: Some("<- = 0x96d6"),
                    },
                    TestResultTextOffsetRange {
                        text: "u16",
                        pos_pat: Some("<- = 0xfecb"),
                        range_start_pat: Some("<-function = 0xfecb"),
                        range_end_pat: Some("<- = 0xfecb"),
                    },
                    // contract `env`.
                    TestResultTextOffsetRange {
                        text: "impl Environment",
                        pos_pat: Some("#[ink::contract(env"),
                        range_start_pat: Some("#[ink::contract("),
                        range_end_pat: Some("#[ink::contract(env"),
                    },
                    // selectors.
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x3d261bd4"),
                        range_start_pat: Some("<-selector = 0x3d261bd4"),
                        range_end_pat: Some("<- = 0x3d261bd4"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x34205be5"),
                        range_start_pat: Some("<-selector = 0x34205be5"),
                        range_end_pat: Some("<- = 0x34205be5"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x7271b782"),
                        range_start_pat: Some("<-selector = 0x7271b782"),
                        range_end_pat: Some("<- = 0x7271b782"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x162df8c2"),
                        range_start_pat: Some("<-selector = 0x162df8c2"),
                        range_end_pat: Some("<- = 0x162df8c2"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x6568382f"),
                        range_start_pat: Some("<-selector = 0x6568382f"),
                        range_end_pat: Some("<- = 0x6568382f"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x4d47d921"),
                        range_start_pat: Some("<-selector = 0x4d47d921"),
                        range_end_pat: Some("<- = 0x4d47d921"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0xdb20f9f5"),
                        range_start_pat: Some("<-selector = 0xdb20f9f5"),
                        range_end_pat: Some("<- = 0xdb20f9f5"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x54b3c76e"),
                        range_start_pat: Some("<-selector = 0x54b3c76e"),
                        range_end_pat: Some("<- = 0x54b3c76e"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0xb20f1bbd"),
                        range_start_pat: Some("<-selector = 0xb20f1bbd"),
                        range_end_pat: Some("<- = 0xb20f1bbd"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0x96d6b57a"),
                        range_start_pat: Some("<-selector = 0x96d6b57a"),
                        range_end_pat: Some("<- = 0x96d6b57a"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _ | @",
                        pos_pat: Some("<- = 0xfecb57d5"),
                        range_start_pat: Some("<-selector = 0xfecb57d5"),
                        range_end_pat: Some("<- = 0xfecb57d5"),
                    },
                ]),
            }],
        },
        TestGroup {
            source: "v4/psp22-extension",
            test_cases: vec![TestCase {
                modifications: None,
                params: Some(TestCaseParams::InlayHints(None)),
                results: TestCaseResults::InlayHints(vec![
                    // extensions.
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x3d26"),
                        range_start_pat: Some("<-extension = 0x3d26"),
                        range_end_pat: Some("<- = 0x3d26"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x3420"),
                        range_start_pat: Some("<-extension = 0x3420"),
                        range_end_pat: Some("<- = 0x3420"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x7271"),
                        range_start_pat: Some("<-extension = 0x7271"),
                        range_end_pat: Some("<- = 0x7271"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x162d"),
                        range_start_pat: Some("<-extension = 0x162d"),
                        range_end_pat: Some("<- = 0x162d"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x6568"),
                        range_start_pat: Some("<-extension = 0x6568"),
                        range_end_pat: Some("<- = 0x6568"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x4d47"),
                        range_start_pat: Some("<-extension = 0x4d47"),
                        range_end_pat: Some("<- = 0x4d47"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0xdb20"),
                        range_start_pat: Some("<-extension = 0xdb20"),
                        range_end_pat: Some("<- = 0xdb20"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x54b3"),
                        range_start_pat: Some("<-extension = 0x54b3"),
                        range_end_pat: Some("<- = 0x54b3"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0xb20f"),
                        range_start_pat: Some("<-extension = 0xb20f"),
                        range_end_pat: Some("<- = 0xb20f"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x96d6"),
                        range_start_pat: Some("<-extension = 0x96d6"),
                        range_end_pat: Some("<- = 0x96d6"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0xfecb"),
                        range_start_pat: Some("<-extension = 0xfecb"),
                        range_end_pat: Some("<- = 0xfecb"),
                    },
                    // contract `env`.
                    TestResultTextOffsetRange {
                        text: "impl Environment",
                        pos_pat: Some("#[ink::contract(env"),
                        range_start_pat: Some("#[ink::contract("),
                        range_end_pat: Some("#[ink::contract(env"),
                    },
                    // selectors.
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x3d261bd4"),
                        range_start_pat: Some("<-selector = 0x3d261bd4"),
                        range_end_pat: Some("<- = 0x3d261bd4"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x34205be5"),
                        range_start_pat: Some("<-selector = 0x34205be5"),
                        range_end_pat: Some("<- = 0x34205be5"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x7271b782"),
                        range_start_pat: Some("<-selector = 0x7271b782"),
                        range_end_pat: Some("<- = 0x7271b782"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x162df8c2"),
                        range_start_pat: Some("<-selector = 0x162df8c2"),
                        range_end_pat: Some("<- = 0x162df8c2"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x6568382f"),
                        range_start_pat: Some("<-selector = 0x6568382f"),
                        range_end_pat: Some("<- = 0x6568382f"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x4d47d921"),
                        range_start_pat: Some("<-selector = 0x4d47d921"),
                        range_end_pat: Some("<- = 0x4d47d921"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0xdb20f9f5"),
                        range_start_pat: Some("<-selector = 0xdb20f9f5"),
                        range_end_pat: Some("<- = 0xdb20f9f5"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x54b3c76e"),
                        range_start_pat: Some("<-selector = 0x54b3c76e"),
                        range_end_pat: Some("<- = 0x54b3c76e"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0xb20f1bbd"),
                        range_start_pat: Some("<-selector = 0xb20f1bbd"),
                        range_end_pat: Some("<- = 0xb20f1bbd"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x96d6b57a"),
                        range_start_pat: Some("<-selector = 0x96d6b57a"),
                        range_end_pat: Some("<- = 0x96d6b57a"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0xfecb57d5"),
                        range_start_pat: Some("<-selector = 0xfecb57d5"),
                        range_end_pat: Some("<- = 0xfecb57d5"),
                    },
                ]),
            }],
        },
        // Storage items.
        TestGroup {
            source: "v5/storage_items/non_packed_tuple_struct",
            test_cases: storage_item.to_vec(),
        },
        TestGroup {
            source: "v4/storage_items/non_packed_tuple_struct",
            test_cases: storage_item.to_vec(),
        },
    ]
}

/// Describes a collection of signature help tests to run against optionally modified
/// ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn signature_help_fixtures() -> Vec<TestGroup> {
    let erc20 = [
        TestCase {
            // Replaces `#[ink::contract]` with `#[ink::contract()]` in the source code.
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::contract]"),
                end_pat: Some("#[ink::contract]"),
                replacement: "#[ink::contract()]",
            }]),
            // Set the offset position at the end of the `#[ink::contract(` substring.
            params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                pat: Some("#[ink::contract("),
            })),
            // Describes the expected signature help.
            results: TestCaseResults::SignatureHelp(vec![
                // Declares expected signature help text as
                // `env: impl Environment, keep_attr: &str`, applied to the text range
                // whose starting and end offset is the position at the beginning
                // of the `#[ink::contract(` substring.
                TestResultSignatureHelp {
                    label: "env: impl Environment, keep_attr: &str",
                    start_pat: Some("#[ink::contract("),
                    end_pat: Some("#[ink::contract("),
                    params: vec![
                        TestResultSignatureParam {
                            start_pat: Some("<-env"),
                            end_pat: Some("impl Environment"),
                        },
                        TestResultSignatureParam {
                            start_pat: Some("<-keep_attr"),
                            end_pat: Some("&str"),
                        },
                    ],
                    active_param: Some(0),
                },
            ]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                pat: Some("#[ink(storage"),
            })),
            results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                label: "storage",
                start_pat: Some("<-storage)]"),
                end_pat: Some("#[ink(storage"),
                params: vec![TestResultSignatureParam {
                    start_pat: Some("<-storage"),
                    end_pat: Some("storage"),
                }],
                active_param: Some(0),
            }]),
        },
        TestCase {
            modifications: None,
            params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                pat: Some("#[ink(constructor"),
            })),
            results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                label: "constructor, default, payable, selector: u32 | _",
                start_pat: Some("<-constructor)]"),
                end_pat: Some("#[ink(constructor"),
                params: vec![
                    TestResultSignatureParam {
                        start_pat: Some("<-constructor"),
                        end_pat: Some("constructor"),
                    },
                    TestResultSignatureParam {
                        start_pat: Some("<-default"),
                        end_pat: Some("default"),
                    },
                    TestResultSignatureParam {
                        start_pat: Some("<-payable"),
                        end_pat: Some("payable"),
                    },
                    TestResultSignatureParam {
                        start_pat: Some("<-selector"),
                        end_pat: Some("u32 | _"),
                    },
                ],
                active_param: Some(0),
            }]),
        },
        TestCase {
            modifications: Some(vec![TestCaseModification {
                start_pat: Some("<-#[ink::test]"),
                end_pat: Some("#[ink::test]"),
                replacement: "#[ink::test()]",
            }]),
            params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                pat: Some("#[ink::test("),
            })),
            results: TestCaseResults::SignatureHelp(vec![]),
        },
    ];

    let trait_erc20 = [TestCase {
        modifications: Some(vec![TestCaseModification {
            start_pat: Some("<-#[ink::trait_definition]"),
            end_pat: Some("#[ink::trait_definition]"),
            replacement: "#[ink::trait_definition()]",
        }]),
        params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
            pat: Some("#[ink::trait_definition("),
        })),
        results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
            label: "keep_attr: &str, namespace: &str",
            start_pat: Some("#[ink::trait_definition("),
            end_pat: Some("#[ink::trait_definition("),
            params: vec![
                TestResultSignatureParam {
                    start_pat: Some("<-keep_attr"),
                    end_pat: Some("keep_attr: &str"),
                },
                TestResultSignatureParam {
                    start_pat: Some("<-namespace"),
                    end_pat: Some("namespace: &str"),
                },
            ],
            active_param: Some(0),
        }]),
    }];

    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract
            // in `test-fixtures/v5` directory.
            source: "v5/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: erc20
                .clone()
                .into_iter()
                .chain([
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                            pat: Some("#[ink(event"),
                        })),
                        results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                            label: "event, anonymous, signature_topic: &str",
                            start_pat: Some("<-event)]"),
                            end_pat: Some("#[ink(event"),
                            params: vec![
                                TestResultSignatureParam {
                                    start_pat: Some("<-event"),
                                    end_pat: Some("event"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-anonymous"),
                                    end_pat: Some("anonymous"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-signature_topic"),
                                    end_pat: Some("signature_topic: &str"),
                                },
                            ],
                            active_param: Some(0),
                        }]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                            pat: Some("#[ink(message"),
                        })),
                        results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                            label: "message, default, payable, selector: u32 | _ | @",
                            start_pat: Some("<-message)]"),
                            end_pat: Some("#[ink(message"),
                            params: vec![
                                TestResultSignatureParam {
                                    start_pat: Some("<-message"),
                                    end_pat: Some("message"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-default"),
                                    end_pat: Some("default"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-payable"),
                                    end_pat: Some("payable"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-selector"),
                                    end_pat: Some("u32 | _ | @"),
                                },
                            ],
                            active_param: Some(0),
                        }]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test()]",
                        }]),
                        params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                            pat: Some("#[ink_e2e::test("),
                        })),
                        results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                            label: "backend: node | runtime_only, environment: impl Environment",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test("),
                            params: vec![
                                TestResultSignatureParam {
                                    start_pat: Some("<-backend"),
                                    end_pat: Some("backend: node | runtime_only"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-environment"),
                                    end_pat: Some("impl Environment"),
                                },
                            ],
                            active_param: Some(0),
                        }]),
                    },
                ])
                .collect(),
        },
        TestGroup {
            source: "v4/erc20",
            test_cases: erc20
                .into_iter()
                .chain([
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                            pat: Some("#[ink(event"),
                        })),
                        results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                            label: "event, anonymous",
                            start_pat: Some("<-event)]"),
                            end_pat: Some("#[ink(event"),
                            params: vec![
                                TestResultSignatureParam {
                                    start_pat: Some("<-event"),
                                    end_pat: Some("event"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-anonymous"),
                                    end_pat: Some("anonymous"),
                                },
                            ],
                            active_param: Some(0),
                        }]),
                    },
                    TestCase {
                        modifications: None,
                        params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                            pat: Some("#[ink(message"),
                        })),
                        results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                            label: "message, default, payable, selector: u32 | _",
                            start_pat: Some("<-message)]"),
                            end_pat: Some("#[ink(message"),
                            params: vec![
                                TestResultSignatureParam {
                                    start_pat: Some("<-message"),
                                    end_pat: Some("message"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-default"),
                                    end_pat: Some("default"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-payable"),
                                    end_pat: Some("payable"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-selector"),
                                    end_pat: Some("u32 | _"),
                                },
                            ],
                            active_param: Some(0),
                        }]),
                    },
                    TestCase {
                        modifications: Some(vec![TestCaseModification {
                            start_pat: Some("<-#[ink_e2e::test]"),
                            end_pat: Some("#[ink_e2e::test]"),
                            replacement: "#[ink_e2e::test()]",
                        }]),
                        params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                            pat: Some("#[ink_e2e::test("),
                        })),
                        results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                            label: "additional_contracts: &str, \
                        environment: impl Environment, \
                        keep_attr: &str",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test("),
                            params: vec![
                                TestResultSignatureParam {
                                    start_pat: Some("<-additional_contracts"),
                                    end_pat: Some("additional_contracts: &str"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-environment"),
                                    end_pat: Some("impl Environment"),
                                },
                                TestResultSignatureParam {
                                    start_pat: Some("<-keep_attr"),
                                    end_pat: Some("keep_attr: &str"),
                                },
                            ],
                            active_param: Some(0),
                        }]),
                    },
                ])
                .collect(),
        },
        // Trait definitions.
        TestGroup {
            source: "v5/trait-erc20",
            test_cases: trait_erc20
                .clone()
                .into_iter()
                .chain([TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                        pat: Some("#[ink(message"),
                    })),
                    results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                        label: "message, default, payable, selector: u32 | _ | @",
                        start_pat: Some("<-message)]"),
                        end_pat: Some("#[ink(message"),
                        params: vec![
                            TestResultSignatureParam {
                                start_pat: Some("<-message"),
                                end_pat: Some("message"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-default"),
                                end_pat: Some("default"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-payable"),
                                end_pat: Some("payable"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-selector"),
                                end_pat: Some("u32 | _ | @"),
                            },
                        ],
                        active_param: Some(0),
                    }]),
                }])
                .collect(),
        },
        TestGroup {
            source: "v4/trait-erc20",
            test_cases: trait_erc20
                .into_iter()
                .chain([TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                        pat: Some("#[ink(message"),
                    })),
                    results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                        label: "message, default, payable, selector: u32 | _",
                        start_pat: Some("<-message)]"),
                        end_pat: Some("#[ink(message"),
                        params: vec![
                            TestResultSignatureParam {
                                start_pat: Some("<-message"),
                                end_pat: Some("message"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-default"),
                                end_pat: Some("default"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-payable"),
                                end_pat: Some("payable"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-selector"),
                                end_pat: Some("u32 | _"),
                            },
                        ],
                        active_param: Some(0),
                    }]),
                }])
                .collect(),
        },
        // Chain extensions.
        TestGroup {
            source: "v4/psp22-extension",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "#[ink::chain_extension()]",
                    }]),
                    params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                        pat: Some("#[ink::chain_extension("),
                    })),
                    results: TestCaseResults::SignatureHelp(vec![]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                        pat: Some("<-extension = 0x3d26"),
                    })),
                    results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                        label: "extension: u32, handle_status: bool",
                        start_pat: Some("<-extension = 0x3d26"),
                        end_pat: Some("extension = 0x3d26"),
                        params: vec![
                            TestResultSignatureParam {
                                start_pat: Some("<-extension"),
                                end_pat: Some("u32"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-handle_status"),
                                end_pat: Some("bool"),
                            },
                        ],
                        active_param: Some(0),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "#[ink(extension = 0x3d26, handle_status = true)]",
                    }]),
                    params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                        pat: Some("handle_status"),
                    })),
                    results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                        label: "extension: u32, handle_status: bool",
                        start_pat: Some("<-extension = 0x3d26"),
                        end_pat: Some("handle_status = true"),
                        params: vec![
                            TestResultSignatureParam {
                                start_pat: Some("<-extension"),
                                end_pat: Some("u32"),
                            },
                            TestResultSignatureParam {
                                start_pat: Some("<-handle_status"),
                                end_pat: Some("bool"),
                            },
                        ],
                        active_param: Some(1),
                    }]),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "v4/storage_items/non_packed_tuple_struct",
            test_cases: vec![TestCase {
                modifications: Some(vec![TestCaseModification {
                    start_pat: Some("<-#[ink::storage_item]"),
                    end_pat: Some("#[ink::storage_item]"),
                    replacement: "#[ink::storage_item()]",
                }]),
                params: Some(TestCaseParams::SignatureHelp(TestParamsOffsetOnly {
                    pat: Some("#[ink::storage_item("),
                })),
                results: TestCaseResults::SignatureHelp(vec![TestResultSignatureHelp {
                    label: "derive: bool",
                    start_pat: Some("#[ink::storage_item("),
                    end_pat: Some("#[ink::storage_item("),
                    params: vec![TestResultSignatureParam {
                        start_pat: Some("<-derive"),
                        end_pat: Some("bool"),
                    }],
                    active_param: Some(0),
                }]),
            }],
        },
    ]
}
