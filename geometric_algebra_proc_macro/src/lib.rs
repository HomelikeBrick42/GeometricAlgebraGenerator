use geometric_algebra::{Basis, BasisIndex, Expression, SquaresTo, Term, Value};
use proc_macro2::{Span, TokenStream};
use quote::{TokenStreamExt, format_ident, quote};
use syn::{Ident, LitBool, LitInt, Token, Type, parse::Parse, parse_macro_input};

struct PgaInput {
    dimension: isize,
    type_: Type,
    multivector: bool,
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(dimension);
    custom_keyword!(multivector);
}

impl Parse for PgaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![type]>()?;
        input.parse::<Token![=]>()?;
        let type_ = input.parse()?;
        input.parse::<Token![;]>()?;

        input.parse::<kw::dimension>()?;
        input.parse::<Token![=]>()?;
        let dimension = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Token![;]>()?;

        input.parse::<kw::multivector>()?;
        input.parse::<Token![=]>()?;
        let multivector = input.parse::<LitBool>()?.value;
        input.parse::<Token![;]>()?;

        Ok(PgaInput {
            dimension,
            type_,
            multivector,
        })
    }
}

#[proc_macro]
pub fn pga(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let PgaInput {
        dimension,
        type_,
        multivector,
    } = parse_macro_input!(tokens as PgaInput);

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

    let multivector = if multivector {
        let mut multivector_terms = Expression { terms: vec![] };

        let mut multivector_members = vec![];
        for i in 0..=basis.bases.len() {
            let terms = Expression::generate_grade(&basis, i, &mut 0).split_into_ga_terms();
            multivector_members.extend(field_names(
                terms.iter().map(|term| term.bases.iter().cloned()),
            ));
            for term in terms {
                multivector_terms.terms.push(Term {
                    values: term.bases.iter().copied().map(Value::Basis).collect(),
                });
            }
        }

        let add_impl = binary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            &multivector_terms,
            format_ident!("other"),
            |a, b, _| a.add(b),
            &basis,
            &type_,
        );
        let sub_impl = binary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            &multivector_terms,
            format_ident!("other"),
            |a, b, _| {
                a.add(&b.multiply(&Expression {
                    terms: vec![Term {
                        values: vec![Value::Constant(-1)],
                    }],
                }))
            },
            &basis,
            &type_,
        );
        let mul_impl = binary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            &multivector_terms,
            format_ident!("other"),
            |a, b, _| a.multiply(b),
            &basis,
            &type_,
        );

        let negation_impl = unary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            |a, _| {
                a.multiply(&Expression {
                    terms: vec![Term {
                        values: vec![Value::Constant(-1)],
                    }],
                })
            },
            &basis,
            &type_,
        );

        let inner_impl = binary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            &multivector_terms,
            format_ident!("other"),
            |a, b, basis| a.inner(b, basis),
            &basis,
            &type_,
        );
        let wedge_impl = binary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            &multivector_terms,
            format_ident!("other"),
            |a, b, basis| a.wedge(b, basis),
            &basis,
            &type_,
        );
        let regressive_impl = binary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            &multivector_terms,
            format_ident!("other"),
            |a, b, basis| a.regressive(b, basis),
            &basis,
            &type_,
        );

        let reverse_impl = unary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            |a, _| a.reverse(),
            &basis,
            &type_,
        );
        let dual_impl = unary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            |a, basis| a.dual(basis),
            &basis,
            &type_,
        );
        let dual_inverse_impl = unary_operation_body(
            &multivector_terms,
            format_ident!("self"),
            |a, basis| a.dual_inverse(basis),
            &basis,
            &type_,
        );

        let mut grade_parts = vec![];
        let mut grade_part_branches = vec![];
        for i in 0..=basis.bases.len() {
            let name = format_ident!("grade{i}");

            let imp = unary_operation_body(
                &multivector_terms,
                format_ident!("self"),
                |a, _| a.grade_part(i),
                &basis,
                &type_,
            );

            grade_parts.push(quote! {
                #[inline]
                #[must_use]
                pub fn #name(self) -> Self {
                    #imp
                }
            });
            grade_part_branches.push(quote! {
                #i => self.#name()
            });
        }

        quote! {
            pub struct Multivector {
                #(#multivector_members: #type_,)*
            }

            impl Multivector {
                #[inline]
                #[must_use]
                pub fn zero() -> Self {
                    Self {
                        #(#multivector_members: <#type_ as ::core::convert::From<i8>>::from(0),)*
                    }
                }

                #[inline]
                #[must_use]
                pub fn one() -> Self {
                    Self {
                        s: <#type_ as ::core::convert::From<i8>>::from(1),
                        ..Self::zero()
                    }
                }

                #[inline]
                #[must_use]
                pub fn inner(self, other: Self) -> Self {
                    #inner_impl
                }

                #[inline]
                #[must_use]
                pub fn wedge(self, other: Self) -> Self {
                    #wedge_impl
                }

                #[inline]
                #[must_use]
                pub fn regressive(self, other: Self) -> Self {
                    #regressive_impl
                }

                #[inline]
                #[must_use]
                pub fn reverse(self) -> Self {
                    #reverse_impl
                }

                #[inline]
                #[must_use]
                pub fn dual(self) -> Self {
                    #dual_impl
                }

                #[inline]
                #[must_use]
                pub fn dual_inverse(self) -> Self {
                    #dual_inverse_impl
                }

                #[inline]
                #[must_use]
                pub fn grade_part(self, grade: usize) -> Self {
                    match grade {
                        #(#grade_part_branches,)*
                        _ => Self::zero(),
                    }
                }

                #(#grade_parts)*
            }

            impl ::core::ops::Add<Self> for Multivector {
                type Output = Self;

                #[inline]
                #[must_use]
                fn add(self, other: Self) -> Self::Output {
                    #add_impl
                }
            }

            impl ::core::ops::Sub<Self> for Multivector {
                type Output = Self;

                #[inline]
                #[must_use]
                fn sub(self, other: Self) -> Self::Output {
                    #sub_impl
                }
            }

            impl ::core::ops::Mul<Self> for Multivector {
                type Output = Self;

                #[inline]
                #[must_use]
                fn mul(self, other: Self) -> Self::Output {
                    #mul_impl
                }
            }

            impl ::core::ops::Neg for Multivector {
                type Output = Self;

                #[inline]
                #[must_use]
                fn neg(self) -> Self::Output {
                    #negation_impl
                }
            }
        }
    } else {
        quote! {}
    };

    quote! {
        #multivector
    }
    .into()
}

fn unary_operation_body(
    a: &Expression,
    a_name: Ident,
    op: impl FnOnce(&Expression, &Basis) -> Expression,
    basis: &Basis,
    type_: &Type,
) -> TokenStream {
    let mut a_expr = a.simplify(basis);
    let a_field_names = field_names(a_expr.terms.iter().map(|term| {
        term.values.iter().filter_map(|value| {
            let Value::Basis(base) = *value else {
                return None;
            };
            Some(base)
        })
    }))
    .collect::<Vec<_>>();
    let mut a_fields = vec![];
    for (index, term) in a_expr.terms.iter_mut().enumerate() {
        let variable_name = format_ident!("_{index}");
        a_fields.push(quote! { #variable_name });

        let variable_name = format!("_{index}");
        term.values.push(Value::Variable(variable_name));
    }

    let result = op(&a_expr, basis).simplify(basis).split_into_ga_terms();

    let result_field_names =
        field_names(result.iter().map(|term| term.bases.iter().copied())).collect::<Vec<_>>();
    let result_expressions = result
        .iter()
        .map(|term| expression_to_tokens(&term.expression, type_))
        .collect::<Vec<_>>();

    quote! {
        let Self {
            #(#a_field_names: #a_fields,)*
        } = #a_name;
        #[allow(clippy::needless_update)]
        let result = Self {
            #(#result_field_names: #result_expressions,)*
            ..Self::zero()
        };
        result
    }
}

fn binary_operation_body(
    a: &Expression,
    a_name: Ident,
    b: &Expression,
    b_name: Ident,
    op: impl FnOnce(&Expression, &Expression, &Basis) -> Expression,
    basis: &Basis,
    type_: &Type,
) -> TokenStream {
    let mut index = 0usize;

    let mut a_expr = a.simplify(basis);
    let a_field_names = field_names(a_expr.terms.iter().map(|term| {
        term.values.iter().filter_map(|value| {
            let Value::Basis(base) = *value else {
                return None;
            };
            Some(base)
        })
    }))
    .collect::<Vec<_>>();
    let mut a_fields = vec![];
    for term in &mut a_expr.terms {
        let variable_name = format_ident!("_{index}");
        a_fields.push(quote! { #variable_name });

        let variable_name = format!("_{index}");
        term.values.push(Value::Variable(variable_name));
        index += 1;
    }

    let mut b_expr = b.simplify(basis);
    let b_field_names = field_names(b_expr.terms.iter().map(|term| {
        term.values.iter().filter_map(|value| {
            let Value::Basis(base) = *value else {
                return None;
            };
            Some(base)
        })
    }))
    .collect::<Vec<_>>();
    let mut b_fields = vec![];
    for term in &mut b_expr.terms {
        let variable_name = format_ident!("_{index}");
        b_fields.push(quote! { #variable_name });

        let variable_name = format!("_{index}");
        term.values.push(Value::Variable(variable_name));
        index += 1;
    }

    let result = op(&a_expr, &b_expr, basis)
        .simplify(basis)
        .split_into_ga_terms();

    let result_field_names =
        field_names(result.iter().map(|term| term.bases.iter().copied())).collect::<Vec<_>>();
    let result_expressions = result
        .iter()
        .map(|term| expression_to_tokens(&term.expression, type_))
        .collect::<Vec<_>>();

    quote! {
        let Self {
            #(#a_field_names: #a_fields,)*
        } = #a_name;
        let Self {
            #(#b_field_names: #b_fields,)*
        } = #b_name;
        #[allow(clippy::needless_update)]
        let result = Self {
            #(#result_field_names: #result_expressions,)*
            ..Self::zero()
        };
        result
    }
}

fn field_names(
    terms: impl Iterator<Item: Iterator<Item = BasisIndex>>,
) -> impl Iterator<Item = Ident> {
    terms.map(|bases| {
        let name = bases
            .map(|base| char::from_digit(base.0 as _, 10).unwrap())
            .collect::<String>();
        if name.is_empty() {
            format_ident!("s")
        } else {
            format_ident!("e{name}")
        }
    })
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
