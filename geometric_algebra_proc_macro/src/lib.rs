use geometric_algebra::{Basis, Expression, SquaresTo};
use quote::{format_ident, quote};
use syn::{LitInt, Token, Type, parse::Parse, parse_macro_input};

struct PgaInput {
    dimension: isize,
    type_: Type,
}

impl Parse for PgaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let dimension = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Token![,]>()?;
        let type_ = input.parse()?;
        Ok(PgaInput { dimension, type_ })
    }
}

#[proc_macro]
pub fn pga(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let PgaInput { dimension, type_ } = parse_macro_input!(tokens as PgaInput);

    if dimension < 0 {
        return quote! {
            compile_error!("Dimension number cannot be less than 0");
        }
        .into();
    }
    let dimension = dimension as usize;
    if dimension > 9 {
        return quote! {
            compile_error!("PGA dimensions greater than 9 are currently not supported");
        }
        .into();
    }

    let basis = Basis {
        bases: std::iter::once(SquaresTo::Zero)
            .chain(std::iter::repeat_n(SquaresTo::PositiveOne, dimension))
            .collect(),
    };

    let multivector = {
        let mut multivector_members = vec![];
        for i in 0..=basis.bases.len() {
            let terms = Expression::generate_grade(&basis, i, &mut 0).split_into_ga_terms();
            for term in terms {
                let name = match term.bases.len() {
                    0 => format_ident!("s"),
                    _ => {
                        let mut s = String::with_capacity(term.bases.len());
                        for base in term.bases {
                            s.push(char::from_digit(base.0 as _, 10).unwrap());
                        }
                        format_ident!("e{}", s)
                    }
                };
                multivector_members.push(quote! { #name });
            }
        }

        quote! {
            pub struct Multivector {
                #(#multivector_members: #type_,)*
            }

            impl Multivector {
                pub fn zero() -> Self {
                    Self {
                        #(#multivector_members: 0i8.into(),)*
                    }
                }

                pub fn one() -> Self {
                    Self {
                        #(#multivector_members: 1i8.into(),)*
                    }
                }
            }
        }
    };

    quote! {
        #multivector
    }
    .into()
}
