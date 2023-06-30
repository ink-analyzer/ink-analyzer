//! A Language Server Protocol (LSP) implementation for the [ink!](https://use.ink/) smart contract programming language.

use clap::Parser;
use lsp_server::Connection;

/// ink! Language Server CLI.
#[derive(Debug, Parser)]
#[command(name = "ink-lsp-server")]
#[command(version, about = "Language Server Protocol (LSP) implementation for the ink! smart contract programming language.", long_about = None)]
struct Cli;

fn main() -> anyhow::Result<()> {
    // Processes CLI options (help and version) and returns if either is used.
    Cli::parse();

    // Runs the LSP server.
    run_server()
}

/// Runs the LSP server.
fn run_server() -> anyhow::Result<()> {
    // Creates the transport.
    let (connection, io_threads) = Connection::stdio();

    // Initializes the server.
    let (connection, initialize_params) = ink_lsp_server::initialize(connection)?;

    // Runs the message dispatch loop.
    ink_lsp_server::main_loop(connection, initialize_params.capabilities)?;

    // Waits for thread to finish.
    io_threads.join()?;

    Ok(())
}
