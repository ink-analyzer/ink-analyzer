use proc_macro2::TokenStream;

#[derive(Debug, Clone)]
pub enum Error {
    Syn(syn::Error),
    Darling(darling::Error),
}

impl Error {
    pub fn into_compile_error(self) -> TokenStream {
        match self {
            Error::Syn(err) => err.to_compile_error(),
            Error::Darling(err) => err.write_errors(),
        }
    }
}

macro_rules! impl_from_error {
    ($variant: ident, $ty: ty) => {
        impl From<$ty> for Error {
            fn from(value: $ty) -> Self {
                Error::$variant(value)
            }
        }
    };
}

impl_from_error!(Syn, syn::Error);
impl_from_error!(Darling, darling::Error);
