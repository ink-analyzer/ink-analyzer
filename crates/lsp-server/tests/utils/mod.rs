//! Utilities for ink! Language Server integration tests.

use std::thread;

/// Creates an LSP server, starts its message dispatch loop and returns a in memory connection to it to simulate a connected client.
pub fn create_initialized_lsp_server() -> lsp_server::Connection {
    // Creates pair of in-memory connections to simulate an LSP client and server.
    let (server_connection, client_connection) = lsp_server::Connection::memory();

    // Creates client capabilities.
    let client_capabilities = test_utils::simple_client_config();

    // Runs the message dispatch loop on a separate thread (because `ink_lsp_server::main_loop` function is blocking).
    thread::spawn(|| ink_lsp_server::main_loop(server_connection, client_capabilities));

    // Returns client connection.
    client_connection
}
