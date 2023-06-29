//! A Language Server Protocol (LSP) implementation for the [ink!](https://use.ink/) smart contract programming language.

use lsp_server::Connection;

fn main() -> anyhow::Result<()> {
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
