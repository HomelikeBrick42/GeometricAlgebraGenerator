use geometric_algebra::{Basis, Expression, SquaresTo, Term, Value};
use proc_macro2::{Span, TokenStream};
use quote::{TokenStreamExt, format_ident, quote};
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
        let mut multivector_terms = Expression { terms: vec![] };

        let mut multivector_members = vec![];
        for i in 0..=basis.bases.len() {
            let terms = Expression::generate_grade(&basis, i, &mut 0).split_into_ga_terms();
            for term in terms {
                multivector_terms.terms.push(Term {
                    values: term.bases.iter().copied().map(Value::Basis).collect(),
                });
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

        let mul_impl = {
            let mut self_fields = vec![];
            let mut other_fields = vec![];

            let mut index = 0usize;
            let mut self_expr = multivector_terms.simplify(&basis);
            for term in &mut self_expr.terms {
                let field_name = match term.values.len() {
                    0 => format_ident!("s"),
                    _ => {
                        let mut s = String::with_capacity(term.values.len());
                        for base in &term.values {
                            let Value::Basis(base) = base else {
                                unreachable!()
                            };
                            s.push(char::from_digit(base.0 as _, 10).unwrap());
                        }
                        format_ident!("e{}", s)
                    }
                };

                let variable_name = format_ident!("_{index}");
                self_fields.push(quote! { #field_name: #variable_name });

                let variable_name = format!("_{index}");
                term.values.push(Value::Variable(variable_name));
                index += 1;
            }

            let mut other_expr = multivector_terms.simplify(&basis);
            for term in &mut other_expr.terms {
                let field_name = match term.values.len() {
                    0 => format_ident!("s"),
                    _ => {
                        let mut s = String::with_capacity(term.values.len());
                        for base in &term.values {
                            let Value::Basis(base) = base else {
                                unreachable!()
                            };
                            s.push(char::from_digit(base.0 as _, 10).unwrap());
                        }
                        format_ident!("e{}", s)
                    }
                };

                let variable_name = format_ident!("_{index}");
                other_fields.push(quote! { #field_name: #variable_name });

                let variable_name = format!("_{index}");
                term.values.push(Value::Variable(variable_name));
                index += 1;
            }

            let result = self_expr
                .multiply(&other_expr)
                .simplify(&basis)
                .split_into_ga_terms();

            let mut result_terms = vec![];
            for term in result {
                let field_name = match term.bases.len() {
                    0 => format_ident!("s"),
                    _ => {
                        let mut s = String::with_capacity(term.bases.len());
                        for base in term.bases {
                            s.push(char::from_digit(base.0 as _, 10).unwrap());
                        }
                        format_ident!("e{}", s)
                    }
                };

                let expression = expression_to_tokens(&term.expression, &type_);

                result_terms.push(quote! { #field_name: #expression });
            }

            quote! {
                impl ::core::ops::Mul<Self> for Multivector {
                    type Output = Self;

                    fn mul(self, other: Self) -> Self::Output {
                        let Self {
                            #(#self_fields,)*
                        } = self;
                        let Self {
                            #(#other_fields,)*
                        } = other;
                        Self {
                            #(#result_terms,)*
                        }
                    }
                }
            }
        };

        quote! {
            pub struct Multivector {
                #(#multivector_members: #type_,)*
            }

            impl Multivector {
                pub fn zero() -> Self {
                    Self {
                        #(#multivector_members: <#type_ as ::core::convert::From<i8>>::from(0),)*
                    }
                }

                pub fn one() -> Self {
                    Self {
                        s: <#type_ as ::core::convert::From<i8>>::from(1),
                        ..Self::zero()
                    }
                }
            }

            #mul_impl
        }
    };

    quote! {
        #multivector
    }
    .into()
}

fn expression_to_tokens(expression: &Expression, type_: &Type) -> TokenStream {
    let mut terms = quote! {};
    terms.append_separated(
        expression
            .terms
            .iter()
            .map(|term| term_to_tokens(term, type_)),
        Token![+](Span::call_site()),
    );
    quote! { (#terms) }
}

fn term_to_tokens(term: &Term, type_: &Type) -> TokenStream {
    let mut values = quote! {};
    values.append_separated(
        term.values
            .iter()
            .map(|value| value_to_tokens(value, type_)),
        Token![*](Span::call_site()),
    );
    quote! { (#values) }
}

fn value_to_tokens(value: &Value, type_: &Type) -> TokenStream {
    match value {
        &Value::Constant(value) => {
            let value = i8::try_from(value).unwrap();
            quote! { <#type_ as ::core::convert::From<i8>>::from(#value) }
        }
        Value::Variable(name) => {
            let name = format_ident!("{name}");
            quote! { #name }
        }
        Value::Basis(_) => unreachable!(),
        Value::Expression(expression) => expression_to_tokens(expression, type_),
    }
}
