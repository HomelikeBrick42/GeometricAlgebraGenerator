mod ga;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Attribute, Ident, LitInt, Token, Type, ext::IdentExt, parenthesized, parse::Parse,
    parse_macro_input,
};

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

struct ElementList {
    elements: Vec<Ident>,
}

impl Parse for ElementList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut elements = vec![];
        while {
            elements.push(input.parse::<Ident>()?);
            if input.peek(Token![*]) {
                input.parse::<Token![*]>()?;
                true
            } else {
                false
            }
        } {}
        Ok(ElementList { elements })
    }
}

struct Group {
    attrs: Vec<Attribute>,
    name: Ident,
    element_lists: Vec<ElementList>,
}

impl Parse for Group {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::group>()?;

        let attrs = input.call(Attribute::parse_outer)?;

        let name = input.parse::<Ident>()?;
        input.parse::<Token![=]>()?;

        let mut element_lists = vec![];
        while {
            element_lists.push(input.parse::<ElementList>()?);
            if input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                true
            } else {
                false
            }
        } {}

        input.parse::<Token![;]>()?;
        Ok(Group {
            attrs,
            name,
            element_lists,
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

enum PrimaryExpression {
    Name(Ident),
    Constant(isize),
    Expression(Box<Expression>),
}

impl Parse for PrimaryExpression {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        Ok(if lookahead.peek(Ident::peek_any) {
            Self::Name(input.parse()?)
        } else if lookahead.peek(LitInt) {
            Self::Constant(input.parse::<LitInt>()?.base10_parse()?)
        } else if lookahead.peek(syn::token::Paren) {
            let expression_tokens;
            parenthesized!(expression_tokens in input);
            let expression = expression_tokens.parse::<Expression>()?;
            Self::Expression(Box::new(expression))
        } else {
            return Err(lookahead.error());
        })
    }
}

enum Expression {
    PrimaryExpression(PrimaryExpression),
    Add {
        left: PrimaryExpression,
        right: PrimaryExpression,
    },
    Sub {
        left: PrimaryExpression,
        right: PrimaryExpression,
    },
    Mul {
        left: PrimaryExpression,
        right: PrimaryExpression,
    },
}

impl Parse for Expression {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let left = input.parse()?;
        Ok(if input.peek(Token![+]) {
            input.parse::<Token![+]>()?;
            let right = input.parse()?;
            Self::Add { left, right }
        } else if input.peek(Token![-]) {
            input.parse::<Token![-]>()?;
            let right = input.parse()?;
            Self::Sub { left, right }
        } else if input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            let right = input.parse()?;
            Self::Mul { left, right }
        } else {
            Self::PrimaryExpression(left)
        })
    }
}

struct Variable {
    name: Ident,
    value: Expression,
}

impl Parse for Variable {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![let]>()?;
        let name = input.parse::<Ident>()?;
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

fn group_element_list_name(
    element_list: &ElementList,
    scalar_name: &Ident,
    elements: &[Element],
) -> syn::Result<Ident> {
    let mut member_name = String::new();

    let mut appended_names = vec![];
    for element in &element_list.elements {
        let element_name = element.to_string();

        for appended_name in &appended_names {
            if element_name == *appended_name {
                return Err(syn::Error::new(
                    element.span(),
                    "Cannot use the same element twice in an element list",
                ));
            }
        }

        let mut found = false;

        if *scalar_name == element_name {
            if element_list.elements.len() == 1 {
                found = true;
                member_name.push_str(&element_name);
            } else {
                return Err(syn::Error::new(
                    element.span(),
                    "Using a scalar in a group multiplied with other elements is not supported",
                ));
            }
        }

        for element in elements {
            if element.name == element_name {
                found = true;
                member_name.push_str(&element_name);
                break;
            }
        }

        if !found {
            return Err(syn::Error::new(
                element.span(),
                "Unknown scalar, element, or group name",
            ));
        }

        appended_names.push(element_name);
    }

    Ok(format_ident!("{member_name}"))
}

fn generate_group(
    Group {
        attrs,
        name,
        element_lists,
    }: &Group,
    element_type: &Type,
    scalar_name: &Ident,
    elements: &[Element],
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

    let member_names = element_lists
        .iter()
        .map(|element_list| group_element_list_name(element_list, scalar_name, elements))
        .collect::<syn::Result<Vec<_>>>()?;

    let name_without_span = format_ident!("{}", name.to_string());
    Ok(quote! {
        #(#attrs)*
        pub struct #name {
            #(#member_names: #element_type,)*
        }

        // the name_without_span is to prevent hovering over the group name repeating the type defintion twice
        impl #name_without_span {
            pub fn zero() -> Self {
                Self {
                    #(#member_names: <#element_type as ::core::convert::From<i8>>::from(0),)*
                }
            }
        }
    })
}

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
                .find(|&group| group.name == argument.group)
                .ok_or_else(|| syn::Error::new(argument.group.span(), "Unknown group name"))?;
            Ok((&argument.name, group))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let return_group_type = groups
        .iter()
        .find(|&group| group.name == *return_group)
        .ok_or_else(|| syn::Error::new(return_group.span(), "Unknown group name"))?;

    let arguments = arguments
        .iter()
        .map(|Argument { name, group }| {
            quote! { #name: #group }
        })
        .collect::<Vec<_>>();
    Ok(quote! {
        pub fn #name(#(#arguments,)*) -> #return_group {
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

    let structs = groups
        .iter()
        .map(|group| generate_group(group, &element_type, &scalar_name, &elements))
        .collect::<syn::Result<Vec<_>>>()?;

    let basis = Basis {
        bases: elements.iter().map(|element| element.squares_to).collect(),
    };

    let functions = functions
        .iter()
        .map(|function| {
            generate_function(
                function,
                &element_type,
                &scalar_name,
                &elements,
                &groups,
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
