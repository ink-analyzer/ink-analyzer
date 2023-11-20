//! ink! analyzer procedural macro utilities.

use quote::quote;

/// Returns the normalized crate root path of the `ink_analyzer_ir` crate.
/// i.e `crate` when the calling crate is `ink_analyzer_ir` itself and `ink_analyzer_ir` otherwise.
pub fn get_normalized_ir_crate_path() -> proc_macro2::TokenStream {
    if let Ok(pkg_name) = std::env::var("CARGO_PKG_NAME") {
        if pkg_name == "ink-analyzer-ir" {
            return quote! { crate };
        }
    }
    quote! { ink_analyzer_ir }
}
