mod ga;

use quote::quote;
use syn::{parse::Parse, parse_macro_input};

struct GaInput {}

// mod kw {
//     use syn::custom_keyword;

//     custom_keyword!(dimension);
//     custom_keyword!(multivector);
//     custom_keyword!(n_vectors);
// }

impl Parse for GaInput {
    fn parse(_input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(GaInput {})
    }
}

#[proc_macro]
pub fn ga(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let GaInput {} = parse_macro_input!(tokens as GaInput);
    quote! {}.into()
}
