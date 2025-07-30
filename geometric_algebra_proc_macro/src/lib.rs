use geometric_algebra::{Basis, BasisIndex, Expression, SquaresTo, Term, Value};
use proc_macro2::{Span, TokenStream};
use quote::{TokenStreamExt, format_ident, quote};
use syn::{Ident, LitBool, LitInt, Token, Type, parse::Parse, parse_macro_input};

struct PgaInput {
    dimension: isize,
    type_: Type,
    multivector: bool,
    n_vectors: bool,
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(dimension);
    custom_keyword!(multivector);
    custom_keyword!(n_vectors);
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

        input.parse::<kw::n_vectors>()?;
        input.parse::<Token![=]>()?;
        let n_vectors = input.parse::<LitBool>()?.value;
        input.parse::<Token![;]>()?;

        Ok(PgaInput {
            dimension,
            type_,
            multivector,
            n_vectors,
        })
    }
}

#[proc_macro]
pub fn pga(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let PgaInput {
        dimension,
        type_,
        multivector,
        n_vectors,
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

    let grade_types = [
        quote! { #type_ },
        {
            let ident = format_ident!("Vector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Bivector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Trivector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Quadvector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Pentavector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Hexavector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Heptavector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Octavector");
            quote! { #ident }
        },
        {
            let ident = format_ident!("Nonavector");
            quote! { #ident }
        },
    ];
    let grade_names = [
        "scalar",
        "vector",
        "bivector",
        "trivector",
        "quadvector",
        "pentavector",
        "hexavector",
        "heptavector",
        "octavector",
        "nonavector",
    ];

    let function_attributes = quote! {
        #[inline]
        #[must_use]
    };

    let basis = Basis {
        bases: std::iter::once(SquaresTo::Zero)
            .chain(std::iter::repeat_n(SquaresTo::PositiveOne, dimension))
            .collect(),
    };

    let multivector_tokens = if multivector {
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
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, _| a.add(b),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let sub_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, _| {
                a.add(&b.multiply(&Expression {
                    terms: vec![Term {
                        values: vec![Value::Constant(-1)],
                    }],
                }))
            },
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let mul_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, _| a.multiply(b),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );

        let negation_impl = unary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            |a, _| {
                a.multiply(&Expression {
                    terms: vec![Term {
                        values: vec![Value::Constant(-1)],
                    }],
                })
            },
            &quote! { Self },
            &basis,
            &type_,
            false,
        );

        let inner_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, basis| a.inner(b, basis),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let wedge_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, basis| a.wedge(b, basis),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let regressive_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, basis| a.regressive(b, basis),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );

        let reverse_impl = unary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            |a, _| a.reverse(),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let dual_impl = unary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            |a, basis| a.dual(basis),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let dual_inverse_impl = unary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            |a, basis| a.dual_inverse(basis),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );

        let pseudonorm_squared_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            |a, b, _| a.multiply(&b.reverse()),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let ideal_pseudonorm_squared_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            |a, b, basis| a.dual(basis).multiply(&b.dual(basis).reverse()),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );

        let project_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, basis| a.inner(b, basis).multiply(b),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );
        let apply_impl = binary_operation_body(
            &multivector_terms,
            &format_ident!("self"),
            &quote! { Self },
            &multivector_terms,
            &format_ident!("other"),
            &quote! { Self },
            |a, b, _| a.multiply(b).multiply(&a.reverse()),
            &quote! { Self },
            &basis,
            &type_,
            false,
        );

        let mut grade_parts = vec![];
        let mut grade_part_branches = vec![];
        for i in 0..=basis.bases.len() {
            let name = format_ident!("grade{i}");

            let imp = unary_operation_body(
                &multivector_terms,
                &format_ident!("self"),
                &quote! { Self },
                |a, _| a.grade_part(i),
                &quote! { Self },
                &basis,
                &type_,
                false,
            );

            grade_parts.push(quote! {
                #function_attributes
                pub fn #name(self) -> Self {
                    #imp
                }
            });
            grade_part_branches.push(quote! {
                #i => self.#name()
            });
        }

        let split_into_parts = if n_vectors {
            let grade_types = &grade_types[..=basis.bases.len()];

            let mut parts = vec![];
            #[allow(clippy::needless_range_loop)]
            for index in 0..grade_types.len() {
                let grade_type = &grade_types[index];

                let fields = field_names(multivector_terms.grade_part(index).terms.iter().map(
                    |term| {
                        term.values.iter().filter_map(|value| {
                            let Value::Basis(base) = *value else {
                                return None;
                            };
                            Some(base)
                        })
                    },
                ))
                .collect::<Vec<_>>();

                if index > 0 {
                    parts.push(quote! {
                        #grade_type {
                            #(#fields,)*
                        }
                    });
                } else {
                    parts.push(quote! {
                        #(#fields)*
                    });
                }
            }

            let self_field_names = field_names(multivector_terms.terms.iter().map(|term| {
                term.values.iter().filter_map(|value| {
                    let Value::Basis(base) = *value else {
                        return None;
                    };
                    Some(base)
                })
            }))
            .collect::<Vec<_>>();

            let mut from_parts = vec![];
            #[allow(clippy::needless_range_loop)]
            for index in 0..grade_types.len() {
                let grade_type = &grade_types[index];

                let from_name = format_ident!("from_{}", grade_names[index]);
                let into_name = format_ident!("{}_part", grade_names[index]);

                let fields = field_names(multivector_terms.grade_part(index).terms.iter().map(
                    |term| {
                        term.values.iter().filter_map(|value| {
                            let Value::Basis(base) = *value else {
                                return None;
                            };
                            Some(base)
                        })
                    },
                ))
                .collect::<Vec<_>>();

                let pattern = &parts[index];
                from_parts.push(quote! {
                    #function_attributes
                    pub fn #from_name(part: #grade_type) -> Self {
                        let #pattern = part;
                        Self {
                            #(#fields,)*
                            ..Self::zero()
                        }
                    }

                    #function_attributes
                    pub fn #into_name(self) -> #grade_type {
                        let Self {
                            #(#self_field_names,)*
                        } = self;
                        #pattern
                    }
                });
            }

            Some(quote! {
                #(#from_parts)*

                #function_attributes
                pub fn into_grades(self) -> (#(#grade_types,)*) {
                    let Self {
                        #(#self_field_names,)*
                    } = self;
                    (#(#parts,)*)
                }
            })
        } else {
            None
        };

        Some(quote! {
            #[derive(::core::clone::Clone, ::core::marker::Copy)]
            pub struct Multivector {
                #(pub #multivector_members: #type_,)*
            }

            impl ::core::fmt::Debug for Multivector
            where
                for<'__> #type_: ::core::fmt::Debug,
            {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(::core::stringify!(Multivector))
                        #(.field(::core::stringify!(#multivector_members), &self.#multivector_members))*
                        .finish()
                }
            }

            impl Multivector {
                #function_attributes
                pub fn zero() -> Self {
                    Self {
                        #(#multivector_members: <#type_ as ::core::convert::From<i8>>::from(0),)*
                    }
                }

                #function_attributes
                pub fn one() -> Self {
                    Self {
                        s: <#type_ as ::core::convert::From<i8>>::from(1),
                        ..Self::zero()
                    }
                }

                #split_into_parts

                #function_attributes
                pub fn inner(self, other: Self) -> Self {
                    #inner_impl
                }

                #function_attributes
                pub fn wedge(self, other: Self) -> Self {
                    #wedge_impl
                }

                #function_attributes
                pub fn regressive(self, other: Self) -> Self {
                    #regressive_impl
                }

                #function_attributes
                pub fn reverse(self) -> Self {
                    #reverse_impl
                }

                #function_attributes
                pub fn dual(self) -> Self {
                    #dual_impl
                }

                #function_attributes
                pub fn dual_inverse(self) -> Self {
                    #dual_inverse_impl
                }

                #function_attributes
                pub fn grade_part(self, grade: usize) -> Self {
                    match grade {
                        #(#grade_part_branches,)*
                        _ => Self::zero(),
                    }
                }

                #(#grade_parts)*

                #function_attributes
                pub fn pseudonorm_squared(self) -> Self {
                    #pseudonorm_squared_impl
                }

                #function_attributes
                pub fn norm_squared(self) -> f32 {
                    self.pseudonorm_squared().scalar_part()
                }

                #function_attributes
                pub fn magnitude_squared(self) -> f32 {
                    self.norm_squared()
                }

                #function_attributes
                pub fn magnitude(self) -> f32 {
                    self.magnitude_squared().sqrt()
                }

                #function_attributes
                pub fn ideal_pseudonorm_squared(self) -> Self {
                    #ideal_pseudonorm_squared_impl
                }

                #function_attributes
                pub fn ideal_norm_squared(self) -> f32 {
                    self.ideal_pseudonorm_squared().scalar_part()
                }

                #function_attributes
                pub fn ideal_magnitude_squared(self) -> f32 {
                    self.ideal_norm_squared()
                }

                #function_attributes
                pub fn ideal_magnitude(self) -> f32 {
                    self.ideal_magnitude_squared().sqrt()
                }

                #function_attributes
                pub fn project(self, other: Self) -> Self {
                    #project_impl
                }

                #function_attributes
                pub fn apply(self, other: Self) -> Self {
                    #apply_impl
                }
            }

            impl ::core::ops::Add<Self> for Multivector {
                type Output = Self;

                #function_attributes
                fn add(self, other: Self) -> Self::Output {
                    #add_impl
                }
            }

            impl ::core::ops::Sub<Self> for Multivector {
                type Output = Self;

                #function_attributes
                fn sub(self, other: Self) -> Self::Output {
                    #sub_impl
                }
            }

            impl ::core::ops::Mul<Self> for Multivector {
                type Output = Self;

                #function_attributes
                fn mul(self, other: Self) -> Self::Output {
                    #mul_impl
                }
            }

            impl ::core::ops::Neg for Multivector {
                type Output = Self;

                #function_attributes
                fn neg(self) -> Self::Output {
                    #negation_impl
                }
            }
        })
    } else {
        None
    };

    let n_vectors_tokens = if n_vectors {
        let point = if let index @ 1.. = basis.bases.len().saturating_sub(1) {
            let name = &grade_types[index];
            Some(quote! { pub type Point = #name; })
        } else {
            None
        };
        let line = if let index @ 1.. = basis.bases.len().saturating_sub(2) {
            let name = &grade_types[index];
            Some(quote! { pub type Line = #name; })
        } else {
            None
        };
        let plane = if let index @ 1.. = basis.bases.len().saturating_sub(3) {
            let name = &grade_types[index];
            Some(quote! { pub type Plane = #name; })
        } else {
            None
        };
        let hyperplane = if let index @ 1.. = basis.bases.len().saturating_sub(4) {
            let name = &grade_types[index];
            Some(quote! { pub type Hyperplane = #name; })
        } else {
            None
        };

        let mut n_vectors = Vec::with_capacity(basis.bases.len());
        #[allow(clippy::needless_range_loop)]
        for n in 1..=basis.bases.len() {
            let mut n_vector_terms = Expression { terms: vec![] };

            let mut n_vector_members = vec![];
            {
                let terms = Expression::generate_grade(&basis, n, &mut 0).split_into_ga_terms();
                n_vector_members.extend(field_names(terms.iter().map(
                    |term| -> std::iter::Cloned<std::slice::Iter<'_, BasisIndex>> {
                        term.bases.iter().cloned()
                    },
                )));
                for term in terms {
                    n_vector_terms.terms.push(Term {
                        values: term.bases.iter().copied().map(Value::Basis).collect(),
                    });
                }
            }

            let mut inners = vec![];
            let mut wedges = vec![];
            let mut regressives = vec![];
            for m in 1..=basis.bases.len() {
                let m_type = &grade_types[m];
                let name = format_ident!("{}", grade_names[m]);

                let mut m_vector_terms = Expression { terms: vec![] };
                let mut m_vector_members = vec![];
                {
                    let terms = Expression::generate_grade(&basis, m, &mut 0).split_into_ga_terms();
                    m_vector_members.extend(field_names(terms.iter().map(
                        |term| -> std::iter::Cloned<std::slice::Iter<'_, BasisIndex>> {
                            term.bases.iter().cloned()
                        },
                    )));
                    for term in terms {
                        m_vector_terms.terms.push(Term {
                            values: term.bases.iter().copied().map(Value::Basis).collect(),
                        });
                    }
                }

                {
                    let inner_name = format_ident!("inner_{}", grade_names[m]);

                    let inner_result_grade = n.abs_diff(m);
                    let inner_result_type = &grade_types[inner_result_grade];
                    let inner_impl = binary_operation_body(
                        &n_vector_terms,
                        &format_ident!("self"),
                        &quote! { Self },
                        &m_vector_terms,
                        &name,
                        m_type,
                        |a, b, basis| a.inner(b, basis),
                        inner_result_type,
                        &basis,
                        &type_,
                        inner_result_grade == 0,
                    );

                    inners.push(quote! {
                        pub fn #inner_name(self, #name: #m_type) -> #inner_result_type {
                            #inner_impl
                        }
                    });
                }

                'wedge: {
                    let wedge_name = format_ident!("wedge_{}", grade_names[m]);

                    let wedge_result_grade = n + m;
                    if wedge_result_grade > basis.bases.len() {
                        break 'wedge;
                    }
                    let wedge_result_type = &grade_types[wedge_result_grade];
                    let wedge_impl = binary_operation_body(
                        &n_vector_terms,
                        &format_ident!("self"),
                        &quote! { Self },
                        &m_vector_terms,
                        &name,
                        m_type,
                        |a, b, basis| a.wedge(b, basis),
                        wedge_result_type,
                        &basis,
                        &type_,
                        wedge_result_grade == 0,
                    );

                    wedges.push(quote! {
                        pub fn #wedge_name(self, #name: #m_type) -> #wedge_result_type {
                            #wedge_impl
                        }
                    });
                }

                'regressive: {
                    let regressive_name = format_ident!("regressive_{}", grade_names[m]);

                    let Some(regressive_result_grade) = basis
                        .bases
                        .len()
                        .checked_sub((basis.bases.len() - n) + (basis.bases.len() - m))
                    else {
                        break 'regressive;
                    };

                    let regressive_result_type = &grade_types[regressive_result_grade];
                    let regressive_impl = binary_operation_body(
                        &n_vector_terms,
                        &format_ident!("self"),
                        &quote! { Self },
                        &m_vector_terms,
                        &name,
                        m_type,
                        |a, b, basis| a.regressive(b, basis),
                        regressive_result_type,
                        &basis,
                        &type_,
                        regressive_result_grade == 0,
                    );

                    regressives.push(quote! {
                        pub fn #regressive_name(self, #name: #m_type) -> #regressive_result_type {
                            #regressive_impl
                        }
                    });
                }
            }

            let name = &grade_types[n];
            n_vectors.push(quote! {
                #[derive(::core::clone::Clone, ::core::marker::Copy)]
                pub struct #name {
                    #(pub #n_vector_members: #type_,)*
                }

                impl ::core::fmt::Debug for #name
                where
                    for<'__> #type_: ::core::fmt::Debug,
                {
                    #[inline]
                    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                        f.debug_struct(::core::stringify!(#name))
                            #(.field(::core::stringify!(#n_vector_members), &self.#n_vector_members))*
                            .finish()
                    }
                }

                impl #name {
                    #function_attributes
                    pub fn zero() -> Self {
                        Self {
                            #(#n_vector_members: <#type_ as ::core::convert::From<i8>>::from(0),)*
                        }
                    }

                    #(#inners)*
                    #(#wedges)*
                    #(#regressives)*
                }
            });
        }

        Some(quote! {
            #point
            #line
            #plane
            #hyperplane

            #(#n_vectors)*
        })
    } else {
        None
    };

    quote! {
        #multivector_tokens

        #n_vectors_tokens
    }
    .into()
}

#[allow(clippy::too_many_arguments)]
fn unary_operation_body(
    a: &Expression,
    a_name: &Ident,
    a_type: &TokenStream,
    op: impl FnOnce(&Expression, &Basis) -> Expression,
    result_type: &TokenStream,
    basis: &Basis,
    type_: &Type,
    flatten_results: bool,
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
    let mut result_expressions = result
        .iter()
        .map(|term| expression_to_tokens(&term.expression, type_))
        .collect::<Vec<_>>();

    if result_expressions.is_empty() {
        result_expressions.push(quote! { <#type_ as ::core::convert::From<i8>>::from(0) });
    }

    let result = if flatten_results {
        quote! {
            (#(#result_expressions),*)
        }
    } else {
        quote! {
            #result_type {
                #(#result_field_names: #result_expressions,)*
                ..<#result_type>::zero()
            }
        }
    };

    quote! {
        let #a_type {
            #(#a_field_names: #a_fields,)*
        } = #a_name;
        #[allow(clippy::needless_update)]
        let result = #result;
        result
    }
}

#[allow(clippy::too_many_arguments)]
fn binary_operation_body(
    a: &Expression,
    a_name: &Ident,
    a_type: &TokenStream,
    b: &Expression,
    b_name: &Ident,
    b_type: &TokenStream,
    op: impl FnOnce(&Expression, &Expression, &Basis) -> Expression,
    result_type: &TokenStream,
    basis: &Basis,
    type_: &Type,
    flatten_results: bool,
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
    let mut result_expressions = result
        .iter()
        .map(|term| expression_to_tokens(&term.expression, type_))
        .collect::<Vec<_>>();

    if result_expressions.is_empty() {
        result_expressions.push(quote! { <#type_ as ::core::convert::From<i8>>::from(0) });
    }

    let result = if flatten_results {
        quote! {
            #(#result_expressions)*
        }
    } else {
        quote! {
            #result_type {
                #(#result_field_names: #result_expressions,)*
                ..<#result_type>::zero()
            }
        }
    };

    quote! {
        let #a_type {
            #(#a_field_names: #a_fields,)*
        } = #a_name;
        let #b_type {
            #(#b_field_names: #b_fields,)*
        } = #b_name;
        #[allow(clippy::needless_update)]
        let result = #result;
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
    if expression.terms.is_empty() {
        terms.append_all(quote! { <#type_ as ::core::convert::From<i8>>::from(0) });
    }
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
    if term.values.is_empty() {
        values.append_all(quote! { <#type_ as ::core::convert::From<i8>>::from(1) });
    }
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
