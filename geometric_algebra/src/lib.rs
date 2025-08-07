mod ga;

use quote::quote;
use syn::{Ident, Token, Type, parse::Parse, parse_macro_input};

use crate::ga::{Basis, SquaresTo};

const GRADE_NAMES: &[&str] = &[
    "Scalar",
    "Vector",
    "Bivector",
    "Trivector",
    "Quadvector",
    "Pentavector",
    "Hexavector",
    "Heptavector",
    "Octovector",
];

mod kw {
    use syn::custom_keyword;

    custom_keyword!(element_type);
    custom_keyword!(scalar_name);
    custom_keyword!(negative_one);
    custom_keyword!(zero);
    custom_keyword!(positive_one);
    custom_keyword!(elements);
}

struct Element {
    ident: String,
    squares_to: SquaresTo,
}

impl Parse for Element {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?.to_string();
        input.parse::<Token![=]>()?;
        let squares_to = {
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::negative_one) {
                input.parse::<kw::negative_one>()?;
                SquaresTo::NegativeOne
            } else if lookahead.peek(kw::zero) {
                input.parse::<kw::zero>()?;
                SquaresTo::Zero
            } else if lookahead.peek(kw::positive_one) {
                input.parse::<kw::positive_one>()?;
                SquaresTo::PositiveOne
            } else {
                return Err(lookahead.error());
            }
        };
        Ok(Element { ident, squares_to })
    }
}

struct GaInput {
    element_type: Type,
    scalar_name: String,
    elements: Vec<Element>,
}

impl Parse for GaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::element_type>()?;
        input.parse::<Token![=]>()?;
        let element_type = input.parse::<Type>()?;
        input.parse::<Token![;]>()?;

        input.parse::<kw::scalar_name>()?;
        input.parse::<Token![=]>()?;
        let scalar_name = input.parse::<Ident>()?.to_string();
        input.parse::<Token![;]>()?;

        input.parse::<kw::elements>()?;
        input.parse::<Token![=]>()?;
        let elements_tokens;
        syn::bracketed!(elements_tokens in input);
        let elements = elements_tokens
            .parse_terminated(Element::parse, Token![,])?
            .into_iter()
            .collect::<Vec<_>>();
        input.parse::<Token![;]>()?;

        Ok(GaInput {
            element_type,
            scalar_name,
            elements,
        })
    }
}

#[proc_macro]
pub fn ga(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let GaInput {
        element_type: _,
        scalar_name: _,
        elements,
    } = parse_macro_input!(tokens as GaInput);

    let _basis = Basis {
        bases: elements.iter().map(|element| element.squares_to).collect(),
    };

    quote! {}.into()
}
