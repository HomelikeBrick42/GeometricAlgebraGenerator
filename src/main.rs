#![deny(rust_2018_idioms)]

use itertools::Itertools;
use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
    iter::{Filter, Peekable},
    str::Chars,
};

enum Result {
    Zero,
    One,
    NegativeOne,
}

struct Basis {
    name: &'static str,
    square_result: Result,
}

struct Algebra {
    bases: Vec<Basis>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Expression(Vec<Term>);

#[derive(Debug, Clone, PartialEq, Eq)]
struct Term(Vec<Atom>);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Atom {
    Scalar(isize),
    Basis(usize),
    Variable(String),
    Expression(Expression),
}

impl Algebra {
    fn pretty_print(
        &self,
        expression: &Expression,
        writer: &mut (impl Write + ?Sized),
    ) -> std::io::Result<()> {
        for (term_index, term) in expression.0.iter().enumerate() {
            if term_index > 0 {
                write!(writer, " + ")?;
            }
            for (atom_index, atom) in term.0.iter().enumerate() {
                if atom_index > 0 {
                    write!(writer, "*")?;
                }
                match *atom {
                    Atom::Scalar(value) => write!(writer, "{value}")?,
                    Atom::Basis(basis_index) => write!(writer, "{}", self.bases[basis_index].name)?,
                    Atom::Variable(ref name) => write!(writer, "{name}")?,
                    Atom::Expression(ref expression) => {
                        write!(writer, "(")?;
                        self.pretty_print(expression, writer)?;
                        write!(writer, ")")?;
                    }
                }
            }
        }
        Ok(())
    }

    fn expand_expression(expression: &Expression) -> Expression {
        let mut result = Expression(vec![]);
        for term in &expression.0 {
            Self::expand_term(term.clone(), &mut result);
        }
        for term in &result.0 {
            for atom in &term.0 {
                assert!(!matches!(atom, Atom::Expression(_)));
            }
        }
        result
    }

    fn expand_term(mut term: Term, expression: &mut Expression) {
        if let Some(index) = term
            .0
            .iter()
            .position(|atom| matches!(atom, Atom::Expression(_)))
        {
            let Atom::Expression(removed_expression) = term.0.remove(index) else {
                unreachable!()
            };
            for new_term in removed_expression.0 {
                let mut term = term.clone();
                term.0.splice(index..index, new_term.0);
                Self::expand_term(term, expression);
            }
        } else {
            expression.0.push(term);
        }
    }

    fn sort_terms(&self, expression: &mut Expression) {
        for term in &mut expression.0 {
            let mut swapped = true;
            while swapped {
                swapped = false;
                for index in 1..term.0.len() {
                    let [a, b, ..] = &mut term.0[index - 1..] else {
                        unreachable!()
                    };
                    match (a, b) {
                        (a @ Atom::Basis(_), b @ Atom::Scalar(_))
                        | (a @ Atom::Basis(_), b @ Atom::Variable(_))
                        | (a @ Atom::Variable(_), b @ Atom::Scalar(_)) => {
                            std::mem::swap(a, b);
                            swapped = true;
                        }

                        (Atom::Variable(a), Atom::Variable(b)) if *b < *a => {
                            std::mem::swap(a, b);
                            swapped = true;
                        }

                        (Atom::Basis(a), Atom::Basis(b)) if *b < *a => {
                            std::mem::swap(a, b);
                            term.0.push(Atom::Scalar(-1));
                            swapped = true;
                        }

                        (_, _) => {}
                    }
                }
            }
        }
    }

    fn compact_terms(&self, expression: &mut Expression) {
        for term in &mut expression.0 {
            let mut simplified = true;
            while simplified {
                simplified = false;
                let mut index = 1;
                while index < term.0.len() {
                    let [a, b, ..] = &mut term.0[index - 1..] else {
                        unreachable!()
                    };
                    match (a, b) {
                        (Atom::Scalar(a), Atom::Scalar(b)) => {
                            *a *= *b;
                            term.0.remove(index);
                            simplified = true;
                        }

                        (atom @ &mut Atom::Basis(a), &mut Atom::Basis(b)) if a == b => {
                            *atom = match self.bases[a].square_result {
                                Result::Zero => Atom::Scalar(0),
                                Result::One => Atom::Scalar(1),
                                Result::NegativeOne => Atom::Scalar(-1),
                            };
                            term.0.remove(index);
                            simplified = true;
                        }

                        (_, _) => {
                            index += 1;
                        }
                    }
                }
            }
        }
    }

    fn get_possible_terms(&self) -> Vec<Vec<usize>> {
        let mut terms = vec![vec![]];
        for len in 1..=self.bases.len() {
            terms.extend(
                self.bases
                    .iter()
                    .enumerate()
                    .map(|(index, _)| index)
                    .combinations(len),
            );
        }
        terms
    }

    fn collect_terms(&self, expression: &Expression) -> Vec<(Vec<usize>, Expression)> {
        let mut possible_terms = self
            .get_possible_terms()
            .into_iter()
            .map(|terms| (terms, Expression(vec![Term(vec![Atom::Scalar(0)])])))
            .collect::<Vec<_>>();

        for term in &expression.0 {
            let mut atoms = term.0.clone();
            let terms = &mut possible_terms
                .iter_mut()
                .find(|(bases, _)| {
                    itertools::equal(
                        &**bases,
                        atoms.iter().filter_map(|atom| match atom {
                            Atom::Basis(index) => Some(index),
                            _ => None,
                        }),
                    )
                })
                .unwrap()
                .1
                 .0;
            atoms.retain(|atom| !matches!(atom, Atom::Basis(_)));
            terms.push(Term(atoms));
        }

        {
            for (_, term) in &mut possible_terms {
                let mut index = 0;
                while index < term.0.len() {
                    let mut variables = term.0[index]
                        .0
                        .iter()
                        .filter(|atom| matches!(atom, Atom::Variable(_)))
                        .cloned()
                        .collect::<Vec<_>>();
                    let mut scalar = 0;
                    term.0.retain(|term| {
                        if itertools::equal(
                            &variables,
                            term.0
                                .iter()
                                .filter(|atom| matches!(atom, Atom::Variable(_))),
                        ) {
                            scalar += term
                                .0
                                .iter()
                                .filter_map(|atom| {
                                    if let &Atom::Scalar(value) = atom {
                                        Some(value)
                                    } else {
                                        None
                                    }
                                })
                                .product::<isize>();
                            false
                        } else {
                            true
                        }
                    });
                    if scalar != 1 || variables.is_empty() {
                        variables.insert(0, Atom::Scalar(scalar));
                    }
                    if scalar != 0 {
                        term.0.insert(index, Term(variables));
                        index += 1;
                    }
                }
            }
        }

        possible_terms.retain_mut(|(_, Expression(terms))| !terms.is_empty());
        possible_terms
    }

    fn parse_atom(&self, chars: &mut Peekable<FilteredChars<'_>>) -> Atom {
        match chars.peek() {
            Some('(') => {
                chars.next();
                let expression = self.parse_expression(chars);
                assert_eq!(chars.next(), Some(')'));
                Atom::Expression(expression)
            }

            Some(c) if c.is_alphabetic() => {
                let mut name = String::new();
                while let Some(c) = chars.next_if(|&c| c.is_alphanumeric()) {
                    name.push(c);
                }
                if let Some(index) = self.bases.iter().position(|basis| basis.name == name) {
                    Atom::Basis(index)
                } else {
                    Atom::Variable(name)
                }
            }

            Some(_) => {
                let mut s = String::new();
                while let Some(c) = chars.next_if(|&c| c == '-' || c.is_ascii_digit()) {
                    s.push(c);
                }
                Atom::Scalar(s.parse().unwrap())
            }

            None => panic!("unexpected EOF"),
        }
    }

    fn parse_term(&self, chars: &mut Peekable<FilteredChars<'_>>) -> Term {
        let mut atoms = vec![];
        while {
            atoms.push(self.parse_atom(chars));
            chars.next_if_eq(&'*').is_some()
        } {}
        Term(atoms)
    }

    fn parse_expression(&self, chars: &mut Peekable<FilteredChars<'_>>) -> Expression {
        let mut terms = vec![];
        while {
            terms.push(self.parse_term(chars));
            chars.next_if_eq(&'+').is_some()
        } {}
        Expression(terms)
    }
}

type FilteredChars<'a> = Filter<Chars<'a>, fn(&char) -> bool>;

fn main() {
    let algebra = Algebra {
        bases: vec![
            Basis {
                name: "e0",
                square_result: Result::Zero,
            },
            Basis {
                name: "e1",
                square_result: Result::One,
            },
            Basis {
                name: "e2",
                square_result: Result::One,
            },
            Basis {
                name: "e3",
                square_result: Result::One,
            },
            Basis {
                name: "e4",
                square_result: Result::One,
            },
            Basis {
                name: "e5",
                square_result: Result::One,
            },
            Basis {
                name: "e6",
                square_result: Result::One,
            },
            Basis {
                name: "e7",
                square_result: Result::One,
            },
        ],
    };

    // let expression = Expression(vec![
    //     Term(vec![Atom::Scalar(3)]),
    //     Term(vec![
    //         Atom::Scalar(5),
    //         Atom::Expression(Expression(vec![
    //             Term(vec![Atom::Variable("a".into())]),
    //             Term(vec![Atom::Basis(2)]),
    //             Term(vec![Atom::Scalar(6)]),
    //             Term(vec![Atom::Variable("a".into())]),
    //             Term(vec![Atom::Expression(Expression(vec![
    //                 Term(vec![Atom::Basis(1), Atom::Basis(0)]),
    //                 Term(vec![Atom::Variable("c".into())]),
    //             ]))]),
    //         ])),
    //         Atom::Basis(0),
    //     ]),
    // ]);

    let possible_terms = algebra.get_possible_terms();

    let expression;
    if false {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        let mut chars = line
            .chars()
            .filter::<fn(&_) -> _>(|&c| !c.is_whitespace())
            .peekable();
        expression = algebra.parse_expression(&mut chars);
        assert_eq!(chars.next(), None);
    } else {
        let mut term_index = 0usize;
        let mut get_terms = || {
            Expression(
                possible_terms
                    .iter()
                    .map(|bases| {
                        let mut atoms = bases.iter().cloned().map(Atom::Basis).collect::<Vec<_>>();
                        atoms.insert(0, Atom::Variable(format!("_{term_index}")));
                        term_index += 1;
                        Term(atoms)
                    })
                    .collect(),
            )
        };
        expression = Expression(vec![Term(vec![
            Atom::Expression(get_terms()),
            Atom::Expression(get_terms()),
        ])]);
    }

    // algebra
    //     .pretty_print(&expression, &mut std::io::stdout())
    //     .unwrap();
    // println!();

    let mut expanded_expression = Algebra::expand_expression(&expression);
    // algebra
    //     .pretty_print(&expanded_expression, &mut std::io::stdout())
    //     .unwrap();
    // println!();

    algebra.sort_terms(&mut expanded_expression);
    // algebra
    //     .pretty_print(&expanded_expression, &mut std::io::stdout())
    //     .unwrap();
    // println!();

    algebra.compact_terms(&mut expanded_expression);
    // algebra
    //     .pretty_print(&expanded_expression, &mut std::io::stdout())
    //     .unwrap();
    // println!();

    let collected_terms = algebra.collect_terms(&expanded_expression);

    // println!();
    // println!("Terms:");
    // for (bases, expression) in &collected_terms {
    //     if bases.is_empty() {
    //         print!("1");
    //     } else {
    //         for (i, &base) in bases.iter().enumerate() {
    //             if i > 0 {
    //                 print!("*");
    //             }
    //             print!("{}", algebra.bases[base].name);
    //         }
    //     }
    //     print!(" is ");
    //     algebra
    //         .pretty_print(expression, &mut std::io::stdout())
    //         .unwrap();
    //     println!();
    // }
    // println!();

    {
        #[derive(Debug)]
        enum Node {
            Variable(String),
            Not(Box<Node>),
            Add(Box<Node>, Box<Node>),
            Mul(Box<Node>, Box<Node>),
        }

        let nodes = collected_terms
            .iter()
            .map(|(bases, expression)| {
                fn convert_term(atoms: &[Atom]) -> Node {
                    if let [Atom::Scalar(-1), rest @ ..] = atoms {
                        return Node::Not(Box::new(convert_term(rest)));
                    }

                    fn convert_atom(atom: &Atom) -> Node {
                        match atom {
                            Atom::Scalar(_) => unreachable!(),
                            Atom::Basis(_) => unreachable!(),
                            Atom::Variable(name) => Node::Variable(name.clone()),
                            Atom::Expression(_) => unreachable!(),
                        }
                    }

                    let mut node = convert_atom(&atoms[0]);
                    for atom in &atoms[1..] {
                        node = Node::Mul(Box::new(node), Box::new(convert_atom(atom)));
                    }
                    node
                }

                let mut node = convert_term(&expression.0[0].0);
                for term in &expression.0[1..] {
                    node = Node::Add(Box::new(node), Box::new(convert_term(&term.0)));
                }
                (bases, node)
            })
            .collect::<Vec<_>>();

        print!("pub struct Multivector<");
        for i in 0..possible_terms.len() {
            if i > 0 {
                print!(", ");
            }
            print!("_{i}");
        }
        println!("> {{");
        for (i, bases) in possible_terms.iter().enumerate() {
            print!("    pub ");
            if bases.is_empty() {
                print!("s");
            } else {
                print!("e");
                for basis in bases {
                    print!("{basis}");
                }
            }
            println!(": _{i},");
        }
        println!("}}");
        println!();

        print!("impl<");
        for i in 0..possible_terms.len() * 2 {
            if i > 0 {
                print!(", ");
            }
            print!("_{i}");
        }
        print!("> Mul<Multivector<");
        for i in 0..possible_terms.len() {
            if i > 0 {
                print!(", ");
            }
            print!("_{}", possible_terms.len() + i);
        }
        print!(">> for Multivector<");
        for i in 0..possible_terms.len() {
            if i > 0 {
                print!(", ");
            }
            print!("_{i}");
        }
        println!(">");
        println!("where");
        fn print_type(node: &Node, writer: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
            match node {
                Node::Variable(name) => write!(writer, "{name}"),
                Node::Not(node) => {
                    write!(writer, "NO<")?;
                    print_type(node, writer)?;
                    write!(writer, ">")
                }
                Node::Add(left, right) => {
                    write!(writer, "AO<")?;
                    print_type(left, writer)?;
                    write!(writer, ", ")?;
                    print_type(right, writer)?;
                    write!(writer, ">")
                }
                Node::Mul(left, right) => {
                    write!(writer, "MO<")?;
                    print_type(left, writer)?;
                    write!(writer, ", ")?;
                    print_type(right, writer)?;
                    write!(writer, ">")
                }
            }
        }
        fn print_type_to_string(node: &Node) -> String {
            let mut bytes = vec![];
            print_type(node, &mut bytes).unwrap();
            String::from_utf8(bytes).unwrap()
        }
        {
            let mut bounds = BTreeSet::new();
            fn add_bounds(node: &Node, bounds: &mut BTreeSet<String>) {
                match node {
                    Node::Variable(name) => {
                        bounds.insert(format!("{name}: Copy"));
                    }
                    Node::Not(node) => {
                        add_bounds(node, bounds);
                        bounds.insert(format!("{}: Neg", print_type_to_string(node)));
                    }
                    Node::Add(left, right) => {
                        add_bounds(left, bounds);
                        add_bounds(right, bounds);
                        bounds.insert(format!(
                            "{}: Add<{}>",
                            print_type_to_string(left),
                            print_type_to_string(right)
                        ));
                    }
                    Node::Mul(left, right) => {
                        add_bounds(left, bounds);
                        add_bounds(right, bounds);
                        bounds.insert(format!(
                            "{}: Mul<{}>",
                            print_type_to_string(left),
                            print_type_to_string(right)
                        ));
                    }
                }
            }
            for (_, node) in &nodes {
                add_bounds(node, &mut bounds);
            }
            for bound in bounds.into_iter().rev() {
                println!("    {bound},");
            }
        }
        println!("{{");
        println!("    type Output = Multivector<");
        for (_, node) in &nodes {
            print!("        ");
            print_type(node, &mut std::io::stdout()).unwrap();
            println!(",");
        }
        println!("    >;");
        println!();
        print!("    fn mul(self, other: Multivector<");
        for i in 0..possible_terms.len() {
            if i > 0 {
                print!(", ");
            }
            print!("_{}", possible_terms.len() + i);
        }
        println!(">) -> Self::Output {{");
        println!("        let Multivector {{");
        for (i, bases) in possible_terms.iter().enumerate() {
            print!("            ");
            if bases.is_empty() {
                print!("s");
            } else {
                print!("e");
                for basis in bases {
                    print!("{basis}");
                }
            }
            println!(": _{i},");
        }
        println!("        }} = self;");
        println!("        let Multivector {{");
        for (i, bases) in possible_terms.iter().enumerate() {
            print!("            ");
            if bases.is_empty() {
                print!("s");
            } else {
                print!("e");
                for basis in bases {
                    print!("{basis}");
                }
            }
            println!(": _{},", possible_terms.len() + i);
        }
        println!("        }} = other;");
        println!("        Multivector {{");
        for (bases, node) in &nodes {
            print!("            ");
            if bases.is_empty() {
                print!("s");
            } else {
                print!("e");
                for basis in *bases {
                    print!("{basis}");
                }
            }
            print!(": ");
            fn print_node(node: &Node) {
                match node {
                    Node::Variable(name) => print!("{name}"),
                    Node::Not(node) => {
                        print!("-");
                        print_node(node);
                    }
                    Node::Add(left, right) => {
                        print!("(");
                        print_node(left);
                        print!(" + ");
                        print_node(right);
                        print!(")");
                    }
                    Node::Mul(left, right) => {
                        print!("(");
                        print_node(left);
                        print!(" * ");
                        print_node(right);
                        print!(")");
                    }
                }
            }
            print_node(node);
            println!(",");
        }
        println!("        }}");
        println!("    }}");
        println!("}}");

        println!();
    }
}
