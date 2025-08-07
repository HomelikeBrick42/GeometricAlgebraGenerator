mod ga;

use quote::{format_ident, quote};
use syn::{Attribute, Error, Ident, Token, Type, parse::Parse, parse_macro_input};

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

struct Function {}

impl Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![fn]>()?;
        Ok(Function {})
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

#[proc_macro]
pub fn ga(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let GaInput {
        element_type,
        scalar_name,
        elements,
        groups,
        functions: _,
    } = parse_macro_input!(tokens as GaInput);

    for (i, element) in elements.iter().enumerate() {
        if element.name == scalar_name {
            return Error::new(
                element.name.span(),
                "Element name cannot be the same as the scalar name",
            )
            .into_compile_error()
            .into();
        }

        for other in &elements[..i] {
            if element.name == other.name {
                return Error::new(element.name.span(), "Cannot repeat element names")
                    .into_compile_error()
                    .into();
            }
        }
    }

    let mut structs = vec![];
    for Group {
        attrs,
        name,
        element_lists,
    } in &groups
    {
        if scalar_name == *name {
            return Error::new(name.span(), "Group name cannot be the same as scalar name")
                .into_compile_error()
                .into();
        }

        for element in &elements {
            if element.name == *name {
                return Error::new(
                    name.span(),
                    "Group name cannot be the same as an element name",
                )
                .into_compile_error()
                .into();
            }
        }

        let mut member_names = vec![];

        for element_list in element_lists {
            let mut member_name = String::new();

            let mut appended_names = vec![];
            for element in &element_list.elements {
                let element_name = element.to_string();

                for appended_name in &appended_names {
                    if element_name == *appended_name {
                        return Error::new(
                            element.span(),
                            "Cannot use the same element twice in an element list",
                        )
                        .into_compile_error()
                        .into();
                    }
                }

                let mut found = false;

                if scalar_name == element_name {
                    if element_list.elements.len() == 1 {
                        found = true;
                        member_name.push_str(&element_name);
                    } else {
                        return Error::new(
                            element.span(),
                            "Using a scalar in a group multiplied with other elements is not supported"
                        )
                        .into_compile_error()
                        .into();
                    }
                }

                for element in &elements {
                    if element.name == element_name {
                        found = true;
                        member_name.push_str(&element_name);
                        break;
                    }
                }

                for group in &groups {
                    if group.name == element_name {
                        todo!("using groups in defintions of other groups")
                    }
                }

                if !found {
                    return Error::new(element.span(), "Unknown scalar, element, or group name")
                        .into_compile_error()
                        .into();
                }

                appended_names.push(element_name);
            }

            member_names.push(format_ident!("{member_name}"));
        }

        structs.push(quote! {
            #(#attrs)*
            pub struct #name {
                #(#member_names: #element_type,)*
            }
        });
    }

    let _basis = Basis {
        bases: elements.iter().map(|element| element.squares_to).collect(),
    };

    quote! {
        #(#structs)*
    }
    .into()
}
