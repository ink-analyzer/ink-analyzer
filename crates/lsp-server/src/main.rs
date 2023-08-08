//! A Language Server Protocol (LSP) implementation for the [ink!](https://use.ink/) smart contract programming language.

use clap::Parser;
use lsp_server::Connection;

/// ink! Language Server CLI.
#[derive(Debug, Parser)]
#[command(name = "ink-lsp-server")]
#[command(version, about = "Language Server Protocol (LSP) implementation for the ink! smart contract programming language.", long_about = None)]
struct Cli;

fn main() -> anyhow::Result<()> {
    // Processes CLI options, only returns an `Err` result for command and argument errors, and for the default help and version arguments.
    if let Err(error) = Cli::try_parse() {
        match error.kind() {
            // Exit and show relevant information for help and version.
            clap::error::ErrorKind::DisplayHelp | clap::error::ErrorKind::DisplayVersion => {
                error.exit();
            }
            // Ignore unknown arguments (e.g VSCode's LSP client passes an additional --stdio that can't be disabled).
            clap::error::ErrorKind::UnknownArgument => {
                // Prints the error message.
                error.print()?;
                // Prints the decision to start the server anyway.
                eprintln!("\nink! Language Server will ignore the argument and start anyway ...");
            }
            // Exit and show relevant information for all other cases.
            _ => {
                error.exit();
            }
        }
    }

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
