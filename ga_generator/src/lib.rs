#![doc = include_str!("./../../README.md")]

mod ga;

use proc_macro2::TokenStream;
use quote::{TokenStreamExt, format_ident, quote};
use std::{cell::Cell, collections::HashMap, fmt::Write};
use syn::{
    Attribute, Ident, LitInt, Token, Type, ext::IdentExt, parenthesized, parse::Parse,
    parse_macro_input,
};

use crate::ga::{Basis, SquaresTo};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(element_type);
    custom_keyword!(scalar_name);
    custom_keyword!(negative_one);
    custom_keyword!(zero);
    custom_keyword!(positive_one);
    custom_keyword!(elements);
    custom_keyword!(group);
}

struct Element {
    name: Ident,
    squares_to: SquaresTo,
}

impl Parse for Element {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
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
        Ok(Element {
            name: ident,
            squares_to,
        })
    }
}

struct Group {
    attrs: Vec<Attribute>,
    name: Ident,
    expression: Expression,
}

impl Parse for Group {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::group>()?;

        let attrs = input.call(Attribute::parse_outer)?;

        let name = input.parse::<Ident>()?;
        input.parse::<Token![=]>()?;

        let expression = input.parse::<Expression>()?;

        input.parse::<Token![;]>()?;
        Ok(Group {
            attrs,
            name,
            expression,
        })
    }
}

struct Argument {
    name: Ident,
    group: Ident,
}

impl Parse for Argument {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse::<Ident>()?;
        input.parse::<Token![:]>()?;
        let group = input.parse::<Ident>()?;
        Ok(Argument { name, group })
    }
}

enum Expression {
    Name(Ident),
    Constant(isize),
    Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Inner {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Wedge {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Regressive {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Negate {
        operand: Box<Expression>,
    },
    Reverse {
        operand: Box<Expression>,
    },
    Dual {
        operand: Box<Expression>,
    },
}

impl Parse for Expression {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        fn primary_expression(input: syn::parse::ParseStream) -> syn::Result<Expression> {
            let lookahead = input.lookahead1();
            Ok(if lookahead.peek(Ident::peek_any) {
                Expression::Name(input.parse()?)
            } else if lookahead.peek(LitInt) {
                Expression::Constant(input.parse::<LitInt>()?.base10_parse()?)
            } else if lookahead.peek(syn::token::Paren) {
                let expression_tokens;
                parenthesized!(expression_tokens in input);
                expression_tokens.parse::<Expression>()?
            } else if lookahead.peek(Token![-]) {
                input.parse::<Token![-]>()?;
                let operand = primary_expression(input)?;
                Expression::Negate {
                    operand: Box::new(operand),
                }
            } else if lookahead.peek(Token![~]) {
                input.parse::<Token![~]>()?;
                let operand = primary_expression(input)?;
                Expression::Reverse {
                    operand: Box::new(operand),
                }
            } else if lookahead.peek(Token![!]) {
                input.parse::<Token![!]>()?;
                let operand = primary_expression(input)?;
                Expression::Dual {
                    operand: Box::new(operand),
                }
            } else {
                return Err(lookahead.error());
            })
        }

        fn binary_operator(input: syn::parse::ParseStream) -> syn::Result<Expression> {
            let left = primary_expression(input)?;
            Ok(if input.peek(Token![*]) {
                input.parse::<Token![*]>()?;
                let right = primary_expression(input)?;
                Expression::Mul {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            } else if input.peek(Token![|]) {
                input.parse::<Token![|]>()?;
                let right = primary_expression(input)?;
                Expression::Inner {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            } else if input.peek(Token![^]) {
                input.parse::<Token![^]>()?;
                let right = primary_expression(input)?;
                Expression::Wedge {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            } else if input.peek(Token![&]) {
                input.parse::<Token![&]>()?;
                let right = primary_expression(input)?;
                Expression::Regressive {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            } else {
                left
            })
        }

        let mut left = binary_operator(input)?;
        loop {
            if input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                let right = binary_operator(input)?;
                left = Self::Add {
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if input.peek(Token![-]) {
                input.parse::<Token![-]>()?;
                let right = binary_operator(input)?;
                left = Self::Sub {
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            };
        }
        Ok(left)
    }
}

struct Variable {
    name: Ident,
    value: Expression,
}

impl Parse for Variable {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![let]>()?;
        let name = input.call(Ident::parse_any)?;
        input.parse::<Token![=]>()?;
        let value = input.parse::<Expression>()?;
        input.parse::<Token![;]>()?;
        Ok(Variable { name, value })
    }
}

struct Return {
    value: Expression,
}

impl Parse for Return {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![return]>()?;
        let value = input.parse::<Expression>()?;
        input.parse::<Token![;]>()?;
        Ok(Return { value })
    }
}

struct Function {
    name: Ident,
    arguments: Vec<Argument>,
    return_group: Ident,
    variables: Vec<Variable>,
    return_expr: Return,
}

impl Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![fn]>()?;
        let name = input.parse::<Ident>()?;

        let arguments_tokens;
        syn::parenthesized!(arguments_tokens in input);
        let arguments = arguments_tokens
            .parse_terminated(Argument::parse, Token![,])?
            .into_iter()
            .collect::<Vec<_>>();

        input.parse::<Token![->]>()?;
        let return_group = input.parse::<Ident>()?;

        let mut variables = vec![];
        let mut return_expr = None;

        let body_tokens;
        syn::braced!(body_tokens in input);
        while !body_tokens.is_empty() {
            let lookahead = body_tokens.lookahead1();
            if lookahead.peek(Token![let]) {
                variables.push(body_tokens.parse::<Variable>()?);
            } else if lookahead.peek(Token![return]) {
                return_expr = Some(body_tokens.parse::<Return>()?);
                break;
            } else {
                return Err(lookahead.error());
            }
        }

        let return_expr = return_expr.ok_or_else(|| {
            syn::Error::new(
                body_tokens.span(),
                "There must be a `return` as the last statement in the body",
            )
        })?;

        Ok(Function {
            name,
            arguments,
            return_group,
            variables,
            return_expr,
        })
    }
}

struct GaInput {
    element_type: Type,
    scalar_name: Ident,
    elements: Vec<Element>,
    groups: Vec<Group>,
    functions: Vec<Function>,
}

impl Parse for GaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::element_type>()?;
        input.parse::<Token![=]>()?;
        let element_type = input.parse::<Type>()?;
        input.parse::<Token![;]>()?;

        input.parse::<kw::scalar_name>()?;
        input.parse::<Token![=]>()?;
        let scalar_name = input.parse::<Ident>()?;
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

        let mut groups = vec![];
        let mut functions = vec![];
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::group) {
                groups.push(input.parse::<Group>()?);
            } else if lookahead.peek(Token![fn]) {
                functions.push(input.parse::<Function>()?);
            } else {
                return Err(lookahead.error());
            }
        }

        Ok(GaInput {
            element_type,
            scalar_name,
            elements,
            groups,
            functions,
        })
    }
}

fn eval_expression(
    expression: &Expression,
    names: &HashMap<String, Box<dyn Fn() -> ga::Expression + '_>>,
    basis: &Basis,
) -> syn::Result<ga::Expression> {
    Ok(match expression {
        Expression::Name(ident) => names
            .get(&ident.to_string())
            .ok_or_else(|| syn::Error::new(ident.span(), "Unknown name"))?(
        ),
        Expression::Constant(value) => ga::Expression {
            terms: vec![ga::Term {
                values: vec![ga::Value::Constant(*value)],
            }],
        },
        Expression::Add { left, right } => {
            let left = eval_expression(left, names, basis)?;
            let right = eval_expression(right, names, basis)?;
            left.add(&right)
        }
        Expression::Sub { left, right } => {
            let left = eval_expression(left, names, basis)?;
            let right = eval_expression(right, names, basis)?;
            left.add(&right.multiply(&ga::Expression {
                terms: vec![ga::Term {
                    values: vec![ga::Value::Constant(-1)],
                }],
            }))
        }
        Expression::Mul { left, right } => {
            let left = eval_expression(left, names, basis)?;
            let right = eval_expression(right, names, basis)?;
            left.multiply(&right)
        }
        Expression::Inner { left, right } => {
            let left = eval_expression(left, names, basis)?.simplify(basis);
            let right = eval_expression(right, names, basis)?.simplify(basis);
            left.inner(&right, basis)
        }
        Expression::Wedge { left, right } => {
            let left = eval_expression(left, names, basis)?.simplify(basis);
            let right = eval_expression(right, names, basis)?.simplify(basis);
            left.wedge(&right, basis)
        }
        Expression::Regressive { left, right } => {
            let left = eval_expression(left, names, basis)?.simplify(basis);
            let right = eval_expression(right, names, basis)?.simplify(basis);
            left.regressive(&right, basis)
        }
        Expression::Negate { operand } => {
            let operand = eval_expression(operand, names, basis)?;
            operand.multiply(&ga::Expression {
                terms: vec![ga::Term {
                    values: vec![ga::Value::Constant(-1)],
                }],
            })
        }
        Expression::Reverse { operand } => {
            let operand = eval_expression(operand, names, basis)?.simplify(basis);
            operand.reverse()
        }
        Expression::Dual { operand } => {
            let operand = eval_expression(operand, names, basis)?.simplify(basis);
            operand.dual(basis)
        }
    })
}

/// returns false for positive members, and true for ones that need to be negated before loading/storing
fn get_group_member_bases(
    expression: &Expression,
    scalar_name: &Ident,
    elements: &[Element],
    groups: &[Group],
    basis: &Basis,
) -> syn::Result<Vec<Vec<ga::BasisIndex>>> {
    let id = &Cell::new(0usize);
    let mut names: HashMap<String, Box<dyn Fn() -> ga::Expression>> =
        HashMap::with_capacity(1 + elements.len() + groups.len());
    names.insert(
        scalar_name.to_string(),
        Box::new(|| {
            let variable = ga::Value::Variable(format!("_{}", id.get()));
            id.set(id.get() + 1);
            ga::Expression {
                terms: vec![ga::Term {
                    values: vec![variable],
                }],
            }
        }),
    );
    for (i, element) in elements.iter().enumerate() {
        names.insert(
            element.name.to_string(),
            Box::new(move || {
                let variable = ga::Value::Variable(format!("_{}", id.get()));
                id.set(id.get() + 1);
                ga::Expression {
                    terms: vec![ga::Term {
                        values: vec![ga::Value::Basis(ga::BasisIndex(i)), variable],
                    }],
                }
            }),
        );
    }
    for group in groups {
        let expression = eval_expression(&group.expression, &names, basis)?.simplify(basis);
        names.insert(
            group.name.to_string(),
            Box::new(move || ga::Expression {
                terms: expression
                    .split_into_ga_terms()
                    .into_iter()
                    .map(|term| ga::Term {
                        values: term
                            .bases
                            .into_iter()
                            .flat_map(|basis| {
                                let variable = ga::Value::Variable(format!("_{}", id.get()));
                                id.set(id.get() + 1);
                                [ga::Value::Basis(basis), variable]
                            })
                            .collect(),
                    })
                    .collect(),
            }),
        );
    }
    let bases = eval_expression(expression, &names, basis)?
        .simplify(basis)
        .split_into_ga_terms()
        .into_iter()
        .map(|term| term.bases)
        .collect::<Vec<_>>();
    Ok(bases)
}

fn generate_group(
    Group {
        attrs,
        name,
        expression: _,
    }: &Group,
    element_type: &Type,
    scalar_name: &Ident,
    elements: &[Element],
    group_member_names: &[Ident],
) -> syn::Result<TokenStream> {
    if scalar_name == name {
        return Err(syn::Error::new(
            name.span(),
            "Group name cannot be the same as scalar name",
        ));
    }

    for element in elements {
        if element.name == *name {
            return Err(syn::Error::new(
                name.span(),
                "Group name cannot be the same as an element name",
            ));
        }
    }

    let name_without_span = format_ident!("{}", name.to_string());
    Ok(quote! {
        #(#attrs)*
        pub struct #name {
            #(#group_member_names: #element_type,)*
        }

        // the name_without_span is to prevent hovering over the group name repeating the type defintion twice
        impl #name_without_span {
            pub fn zero() -> Self {
                Self {
                    #(#group_member_names: <#element_type as ::core::convert::From<i8>>::from(0),)*
                }
            }
        }
    })
}

#[allow(clippy::too_many_arguments)]
fn generate_function(
    Function {
        name,
        arguments,
        return_group,
        variables,
        return_expr,
    }: &Function,
    element_type: &Type,
    scalar_name: &Ident,
    elements: &[Element],
    groups: &[Group],
    group_member_bases: &[Vec<Vec<ga::BasisIndex>>],
    group_member_names: &[Vec<Ident>],
    basis: &Basis,
) -> syn::Result<TokenStream> {
    let argument_names_and_groups = arguments
        .iter()
        .map(|argument| {
            if argument.name == *scalar_name {
                return Err(syn::Error::new(
                    argument.name.span(),
                    "Cannot have an argument with the same name as the scalar name",
                ));
            }

            for group in groups {
                if argument.name == group.name {
                    return Err(syn::Error::new(
                        argument.name.span(),
                        "Cannot have an argument with the same name as a group name",
                    ));
                }
            }

            let group = groups
                .iter()
                .position(|group| group.name == argument.group)
                .ok_or_else(|| syn::Error::new(argument.group.span(), "Unknown group name"))?;
            Ok((&argument.name, group))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let _return_group_type = groups
        .iter()
        .position(|group| group.name == *return_group)
        .ok_or_else(|| syn::Error::new(return_group.span(), "Unknown group name"))?;

    let body = {
        let mut names: HashMap<String, Box<dyn Fn() -> ga::Expression + '_>> =
            HashMap::with_capacity(1 + elements.len() + argument_names_and_groups.len());

        names.insert(
            scalar_name.to_string(),
            Box::new(|| ga::Expression {
                terms: vec![ga::Term {
                    values: vec![ga::Value::Constant(1)],
                }],
            }),
        );
        for (i, element) in elements.iter().enumerate() {
            names.insert(
                element.name.to_string(),
                Box::new(move || ga::Expression {
                    terms: vec![ga::Term {
                        values: vec![ga::Value::Basis(ga::BasisIndex(i))],
                    }],
                }),
            );
        }

        let mut id = 0usize;
        let mut parameter_variables = vec![];
        for &(name, group_index) in &argument_names_and_groups {
            let group_member_bases = &group_member_bases[group_index];
            let group_member_names = &group_member_names[group_index];

            let mut expression = ga::Expression { terms: vec![] };

            for i in 0..group_member_names.len() {
                let group_member_base = &group_member_bases[i];

                expression.terms.push(ga::Term {
                    values: std::iter::once(ga::Value::Variable(format!("_{id}")))
                        .chain(group_member_base.iter().copied().map(ga::Value::Basis))
                        .collect(),
                });

                let group_member_name = &group_member_names[i];

                let name = format_ident!("{name}");
                let variable_name = format_ident!("_{id}");
                parameter_variables.push(quote! {
                    let #variable_name = #name.#group_member_name;
                });

                id += 1;
            }

            names.insert(name.to_string(), Box::new(move || expression.clone()));
        }

        for variable in variables {
            let expression = eval_expression(&variable.value, &names, basis)?.simplify(basis);
            names.insert(
                variable.name.to_string(),
                Box::new(move || expression.clone()),
            );
        }

        let terms = eval_expression(&return_expr.value, &names, basis)?
            .simplify(basis)
            .split_into_ga_terms()
            .into_iter()
            .map(|term| {
                let member_name = if term.bases.is_empty() {
                    format_ident!("{scalar_name}")
                } else {
                    let mut name = String::new();
                    for basis in term.bases {
                        write!(name, "{}", elements[basis.0].name).unwrap();
                    }
                    format_ident!("{name}")
                };

                fn emit_value(value: &ga::Value, element_type: &Type) -> TokenStream {
                    match value {
                        ga::Value::Constant(value) => {
                            let value = i8::try_from(*value).expect("constant should fit in an i8");
                            quote! { <#element_type as ::core::convert::From<i8>>::from(#value) }
                        }
                        ga::Value::Variable(name) => {
                            let name = format_ident!("{name}");
                            quote! { #name }
                        }
                        ga::Value::Basis(_) => unreachable!(),
                        ga::Value::Expression(_) => unreachable!(),
                    }
                }

                fn emit_term(term: &ga::Term, element_type: &Type) -> TokenStream {
                    let mut result = quote! {};
                    result.append_separated(
                        term.values
                            .iter()
                            .map(|value| emit_value(value, element_type)),
                        quote! { * },
                    );
                    quote! { (#result) }
                }

                fn emit_expression(
                    expression: &ga::Expression,
                    element_type: &Type,
                ) -> TokenStream {
                    let mut result = quote! {};
                    result.append_separated(
                        expression
                            .terms
                            .iter()
                            .map(|term| emit_term(term, element_type)),
                        quote! { + },
                    );
                    result
                }

                let expression = emit_expression(&term.expression, element_type);
                quote! { #member_name: #expression }
            })
            .collect::<Vec<_>>();

        quote! {
            #(#parameter_variables)*
            #[allow(clippy::needless_update)]
            #return_group {
                #(#terms,)*
                ..#return_group::zero()
            }
        }
    };

    let arguments = arguments
        .iter()
        .map(|Argument { name, group }| {
            quote! { #name: #group }
        })
        .collect::<Vec<_>>();
    Ok(quote! {
        pub fn #name(#(#arguments,)*) -> #return_group {
            #body
        }
    })
}

fn generate(
    GaInput {
        element_type,
        scalar_name,
        elements,
        groups,
        functions,
    }: GaInput,
) -> syn::Result<TokenStream> {
    for (i, element) in elements.iter().enumerate() {
        if element.name == scalar_name {
            return Err(syn::Error::new(
                element.name.span(),
                "Element name cannot be the same as the scalar name",
            ));
        }

        for other in &elements[..i] {
            if element.name == other.name {
                return Err(syn::Error::new(
                    element.name.span(),
                    "Cannot repeat element names",
                ));
            }
        }
    }

    let basis = Basis {
        bases: elements.iter().map(|element| element.squares_to).collect(),
    };

    let group_member_bases = groups
        .iter()
        .enumerate()
        .map(|(i, group)| {
            get_group_member_bases(
                &group.expression,
                &scalar_name,
                &elements,
                &groups[..i],
                &basis,
            )
        })
        .collect::<syn::Result<Vec<_>>>()?;
    let group_member_names = group_member_bases
        .iter()
        .map(|members| {
            members
                .iter()
                .map(|base| {
                    if base.is_empty() {
                        format_ident!("{scalar_name}")
                    } else {
                        let mut name = String::new();
                        for basis in base {
                            write!(name, "{}", elements[basis.0].name).unwrap();
                        }
                        format_ident!("{name}")
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let structs = groups
        .iter()
        .enumerate()
        .map(|(i, group)| {
            generate_group(
                group,
                &element_type,
                &scalar_name,
                &elements,
                &group_member_names[i],
            )
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let functions = functions
        .iter()
        .map(|function| {
            generate_function(
                function,
                &element_type,
                &scalar_name,
                &elements,
                &groups,
                &group_member_bases,
                &group_member_names,
                &basis,
            )
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote! {
        #(#structs)*
        #(#functions)*
    })
}

#[proc_macro]
pub fn ga(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    generate(parse_macro_input!(tokens))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
