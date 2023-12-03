# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.14] - 2023-12-03

- General robustness improvements for semantic analyzer.

## [0.2.13] - 2023-12-03

- Add diagnostics and quickfixes for chain environment arguments (i.e. argument value is a path to an item `T` and `T: impl ink::env::Environment`).
- Improvements for trait definition resolution and formatting of related quickfixes.
- General robustness improvements for semantic analyzer.

## [0.2.12] - 2023-11-02

- Signature help improvements.
- Advertise server command capabilities and account for workspace edit client capabilities.
- General robustness improvements for semantic analyzer.

## [0.2.11] - 2023-10-28

- Add create project command.
- Add diagnostics and quickfixes for ink! trait definition implementations.

## [0.2.10] - 2023-10-09

- General robustness improvements for semantic analyzer.

## [0.2.9] - 2023-09-29

- Signature help improvements.
- Improvements to code actions for trait `impl` blocks.
- General robustness improvements for semantic analyzer.

## [0.2.8] - 2023-09-25

- Add signature help for ink! attribute arguments.
- Add code/intent actions for "flattening" ink! attributes.
- General robustness improvements for semantic analyzer.

## [0.2.7] - 2023-09-19

- Improve diagnostics and quickfixes for ambiguous attribute arguments.
- Improve "primary" attribute and argument quickfixes.
- General robustness improvements for semantic analyzer.

## [0.2.6] - 2023-09-18

- More code/intent actions for adding ink! entities (i.e. code stubs/snippets for items not just attributes).
- "Unique selector" quickfix improvements.
- General robustness improvements for semantic analyzer.

## [0.2.5] - 2023-09-13

- Add code/intent actions for adding ink! entities (i.e. code stubs/snippets for items not just attributes).
- Improve formatting of text edits (i.e. code/intent actions and quickfixes) inserted after comments and rustdoc.
- General robustness improvements for semantic analyzer.

## [0.2.4] - 2023-09-10

- Improve formatting of move and delete text edits (i.e. actions and quickfixes).
- General robustness improvements for semantic analyzer.

## [0.2.3] - 2023-09-09

- Improve formatting of text edits (i.e. actions and quickfixes) based on context.
- General robustness improvements for semantic analyzer.

## [0.2.2] - 2023-09-07

- Add quickfixes for diagnostic errors and warnings.
- Add support for ink! e2e macro (i.e. diagnostics, completions, code/intent actions and hover content).
- Add inlay hints for ink! attribute arguments with values.
- Add support for multiple snippets in code actions.
- Limit diagnostic ranges to item "declarations".
- General robustness improvements for semantic analyzer.

## [0.2.1] - 2023-07-22

- First distributable release.
