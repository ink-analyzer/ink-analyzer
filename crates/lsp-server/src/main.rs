//! A Language Server Protocol (LSP) implementation for the [ink!](https://use.ink/) smart contract programming language.

use lsp_server::Connection;
use lsp_types::InitializeParams;

fn main() -> anyhow::Result<()> {
    // Creates the transport.
    let (connection, io_threads) = Connection::stdio();

    // Starts initialization.
    let (initialize_id, initialize_params_json) = connection.initialize_start()?;
    let initialize_params: InitializeParams = serde_json::from_value(initialize_params_json)
        .map_err(|error| {
            anyhow::format_err!("Failed to deserialize initialize parameters: {error}")
        })?;

    // Finalize initialization.
    let initialize_result = serde_json::to_value(ink_lsp_server::initialize(&initialize_params))
        .map_err(|error| anyhow::format_err!("Failed to serialize initialize result: {error}"))?;
    connection.initialize_finish(initialize_id, initialize_result)?;

    // Runs the message dispatch loop.
    ink_lsp_server::main_loop(connection, initialize_params.capabilities)?;

    // Waits for thread to finish.
    io_threads.join()?;

    Ok(())
}
