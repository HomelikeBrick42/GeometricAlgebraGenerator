use itertools::Itertools;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SquaresTo {
    NegativeOne = -1,
    Zero = 0,
    PositiveOne = 1,
}

impl SquaresTo {
    pub fn multiply(self, other: Self) -> Self {
        match (self, other) {
            (SquaresTo::NegativeOne, SquaresTo::NegativeOne) => SquaresTo::PositiveOne,
            (SquaresTo::Zero, _) | (_, SquaresTo::Zero) => SquaresTo::Zero,
            (SquaresTo::PositiveOne, other) | (other, SquaresTo::PositiveOne) => other,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Basis {
    pub bases: Vec<SquaresTo>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasisIndex(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Constant(isize),
    Variable(String),
    Basis(BasisIndex),
    Expression(Expression),
}

impl Value {
    pub fn reverse(&self) -> Self {
        match *self {
            Value::Constant(value) => Value::Constant(value),
            Value::Variable(ref name) => Value::Variable(name.clone()),
            Value::Basis(basis_index) => Value::Basis(basis_index),
            Value::Expression(ref expression) => Value::Expression(expression.reverse()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Term {
    pub values: Vec<Value>,
}

impl Term {
    pub fn multiply(&self, other: &Self) -> Self {
        Self {
            values: self
                .values
                .iter()
                .chain(other.values.iter())
                .cloned()
                .collect(),
        }
    }

    pub fn simplify(&self, basis: &Basis) -> Self {
        let mut new_values = self.values.clone();

        let mut sign_change = SquaresTo::PositiveOne;
        {
            let mut were_duplicate_bases = false;

            let mut swapped = true;
            while swapped {
                swapped = false;
                for i in 1..new_values.len() {
                    let a = &new_values[i - 1];
                    let b = &new_values[i];

                    // just dont try to reorder groups, wait until they are expanded
                    if let (Value::Expression(_), _) | (_, Value::Expression(_)) = (a, b) {
                        continue;
                    }

                    match a.cmp(b) {
                        std::cmp::Ordering::Less => {}
                        std::cmp::Ordering::Equal => {
                            if let (Value::Basis(_), Value::Basis(_)) = (a, b) {
                                were_duplicate_bases = true;
                            }
                        }
                        std::cmp::Ordering::Greater => {
                            if let (Value::Basis(_), Value::Basis(_)) = (a, b) {
                                sign_change = sign_change.multiply(SquaresTo::NegativeOne);
                            }

                            new_values.swap(i - 1, i);
                            swapped = true;
                        }
                    }
                }
            }

            if were_duplicate_bases {
                let mut i = 1;
                while i < new_values.len() {
                    let a = &new_values[i - 1];
                    let b = &new_values[i];

                    if let (Value::Basis(a), Value::Basis(b)) = (a, b)
                        && a == b
                    {
                        sign_change = sign_change.multiply(basis.bases[a.0]);
                        new_values.drain(i - 1..=i);
                    } else {
                        i += 1;
                    }
                }
            }
        }

        let mut new_values = VecDeque::from(new_values);

        let mut constant = match sign_change {
            SquaresTo::NegativeOne => -1,
            SquaresTo::Zero => 0,
            SquaresTo::PositiveOne => 1,
        };

        // all numbers should be at the front of the list, so combine them
        while let Some(&Value::Constant(value)) = new_values.front() {
            new_values.pop_front();
            constant *= value;
        }

        if constant != 1 {
            new_values.push_front(Value::Constant(constant));
        }

        Self {
            values: new_values.into(),
        }
    }

    pub fn reverse(&self) -> Self {
        Self {
            values: self.values.iter().rev().map(Value::reverse).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expression {
    pub terms: Vec<Term>,
}

impl Expression {
    pub fn add(&self, other: &Self) -> Self {
        Self {
            terms: self
                .terms
                .iter()
                .chain(other.terms.iter())
                .cloned()
                .collect(),
        }
    }

    pub fn multiply(&self, other: &Self) -> Self {
        Self {
            terms: vec![Term {
                values: vec![
                    Value::Expression(self.clone()),
                    Value::Expression(other.clone()),
                ],
            }],
        }
    }

    pub fn simplify(&self, basis: &Basis) -> Self {
        let mut result = self.clone();

        while {
            let mut new_terms = result
                .terms
                .iter()
                .map(|term| term.simplify(basis))
                .collect::<Vec<_>>();

            // all constants should be at the front of the list
            new_terms.retain(|term| !matches!(term.values.first(), Some(Value::Constant(0))));

            'simplification: {
                for (term_index, term) in new_terms.iter().enumerate() {
                    for (value_index, value) in term.values.iter().enumerate() {
                        if let Value::Expression(_) = value {
                            let mut before_terms = term.values.clone();
                            let after_terms = before_terms.split_off(value_index + 1);

                            let Some(Value::Expression(expression)) = before_terms.pop() else {
                                unreachable!()
                            };

                            for term in &expression.terms {
                                new_terms.push(
                                    Term {
                                        values: before_terms.clone(),
                                    }
                                    .multiply(term)
                                    .multiply(&Term {
                                        values: after_terms.clone(),
                                    }),
                                );
                            }

                            new_terms.remove(term_index);
                            break 'simplification;
                        }
                    }
                }

                for i in 0..new_terms.len() {
                    let mut a = new_terms[i].values.as_slice();
                    let mut a_constant = 1;
                    while let Some(Value::Constant(value)) = a.first() {
                        a_constant *= value;
                        a = &a[1..];
                    }

                    for j in i + 1..new_terms.len() {
                        let mut b = new_terms[j].values.as_slice();
                        let mut b_constant = 1;
                        while let Some(Value::Constant(value)) = b.first() {
                            b_constant *= value;
                            b = &b[1..];
                        }

                        if a == b {
                            let new_term = Term {
                                values: std::iter::once(Value::Constant(a_constant + b_constant))
                                    .chain(a.iter().cloned())
                                    .collect(),
                            };
                            new_terms.remove(j);
                            new_terms.remove(i);
                            new_terms.push(new_term);
                            break 'simplification;
                        }
                    }
                }
            }

            new_terms.sort_by(|a, b| a.values.len().cmp(&b.values.len()).then(a.cmp(b)));
            let changed = result.terms != new_terms;
            result = Expression { terms: new_terms };
            changed
        } {}

        result
    }

    pub fn reverse(&self) -> Self {
        Self {
            terms: self.terms.iter().map(Term::reverse).collect(),
        }
    }

    /// must only be called once expression is simplified
    pub fn split_into_ga_terms(&self) -> Vec<GATerm> {
        let mut hashmap = HashMap::<Vec<BasisIndex>, GATerm>::new();

        for term in &self.terms {
            let mut bases = vec![];
            let mut non_bases = vec![];

            for value in &term.values {
                match *value {
                    Value::Expression(_) => panic!("there should be no nested expressions"),
                    Value::Basis(basis) => bases.push(basis),
                    ref value => non_bases.push(value.clone()),
                }
            }

            let ga_term = hashmap.entry(bases.clone()).or_insert_with(|| GATerm {
                bases,
                expression: Expression { terms: vec![] },
            });
            ga_term.expression.terms.push(Term { values: non_bases });
        }

        let mut terms = hashmap
            .into_values()
            .map(|mut term| {
                term.expression
                    .terms
                    .sort_by(|a, b| a.values.len().cmp(&b.values.len()).then(a.cmp(b)));
                term
            })
            .collect::<Vec<_>>();
        terms.sort_by(|a, b| {
            a.bases
                .len()
                .cmp(&b.bases.len())
                .then_with(|| a.bases.cmp(&b.bases))
        });
        terms
    }

    pub fn generate_grade(basis: &Basis, grade: usize, offset: &mut usize) -> Self {
        Self {
            terms: basis
                .bases
                .iter()
                .enumerate()
                .map(|(i, _)| BasisIndex(i))
                .combinations(grade)
                .map(|basis| {
                    let mut values = basis.into_iter().map(Value::Basis).collect::<Vec<_>>();
                    values.push(Value::Variable(format!("_{offset}")));
                    *offset += 1;
                    Term { values }
                })
                .collect(),
        }
    }

    pub fn grade_part(&self, grade: usize) -> Expression {
        Self {
            terms: self
                .split_into_ga_terms()
                .into_iter()
                .filter_map(|term| {
                    if term.bases.len() != grade {
                        return None;
                    }

                    Some(Term {
                        values: term
                            .bases
                            .into_iter()
                            .map(Value::Basis)
                            .chain(std::iter::once(Value::Expression(term.expression)))
                            .collect(),
                    })
                })
                .collect(),
        }
    }

    pub fn wedge(&self, other: &Self, basis: &Basis) -> Self {
        let a = self.split_into_ga_terms();
        let b = other.split_into_ga_terms();
        let mut terms = vec![];
        for a in &a {
            for b in &b {
                let a_grade = a.bases.len();
                let b_grade = b.bases.len();

                let a_values = Expression {
                    terms: vec![Term {
                        values: a
                            .bases
                            .iter()
                            .cloned()
                            .map(Value::Basis)
                            .chain(std::iter::once(Value::Expression(a.expression.clone())))
                            .collect(),
                    }],
                };
                let b_values = Expression {
                    terms: vec![Term {
                        values: b
                            .bases
                            .iter()
                            .cloned()
                            .map(Value::Basis)
                            .chain(std::iter::once(Value::Expression(b.expression.clone())))
                            .collect(),
                    }],
                };
                terms.extend(
                    a_values
                        .multiply(&b_values)
                        .simplify(basis)
                        .grade_part(a_grade + b_grade)
                        .simplify(basis)
                        .terms,
                );
            }
        }
        Self { terms }
    }

    pub fn inner(&self, other: &Self, basis: &Basis) -> Self {
        let a = self.split_into_ga_terms();
        let b = other.split_into_ga_terms();
        let mut terms = vec![];
        for a in &a {
            for b in &b {
                let a_grade = a.bases.len();
                let b_grade = b.bases.len();

                let a_values = Expression {
                    terms: vec![Term {
                        values: a
                            .bases
                            .iter()
                            .cloned()
                            .map(Value::Basis)
                            .chain(std::iter::once(Value::Expression(a.expression.clone())))
                            .collect(),
                    }],
                };
                let b_values = Expression {
                    terms: vec![Term {
                        values: b
                            .bases
                            .iter()
                            .cloned()
                            .map(Value::Basis)
                            .chain(std::iter::once(Value::Expression(b.expression.clone())))
                            .collect(),
                    }],
                };
                terms.extend(
                    a_values
                        .multiply(&b_values)
                        .simplify(basis)
                        .grade_part(a_grade.abs_diff(b_grade))
                        .simplify(basis)
                        .terms,
                );
            }
        }
        Self { terms }
    }

    pub fn dual(&self, basis: &Basis) -> Self {
        Self {
            terms: self
                .split_into_ga_terms()
                .into_iter()
                .map(|term| {
                    let mut new_basis = Term {
                        values: basis
                            .bases
                            .iter()
                            .enumerate()
                            .map(|(i, _)| BasisIndex(i))
                            .filter(|basis| {
                                term.bases
                                    .iter()
                                    .all(|other_basis| basis.0 != other_basis.0)
                            })
                            .map(Value::Basis)
                            .collect::<Vec<_>>(),
                    }
                    .simplify(basis);
                    let term_bases = Term {
                        values: term
                            .bases
                            .into_iter()
                            .map(Value::Basis)
                            .chain(new_basis.values.clone())
                            .collect(),
                    }
                    .simplify(basis);

                    if let Value::Constant(-1) = term_bases.values[0] {
                        new_basis.values.push(Value::Constant(-1));
                    }
                    new_basis.values.push(Value::Expression(term.expression));
                    new_basis
                })
                .collect(),
        }
    }

    pub fn dual_inverse(&self, basis: &Basis) -> Self {
        Self {
            terms: self
                .split_into_ga_terms()
                .into_iter()
                .map(|term| {
                    let mut new_basis = Term {
                        values: basis
                            .bases
                            .iter()
                            .enumerate()
                            .map(|(i, _)| BasisIndex(i))
                            .filter(|basis| {
                                term.bases
                                    .iter()
                                    .all(|other_basis| basis.0 != other_basis.0)
                            })
                            .map(Value::Basis)
                            .collect::<Vec<_>>(),
                    }
                    .simplify(basis);
                    let term_bases = Term {
                        values: new_basis
                            .values
                            .iter()
                            .cloned()
                            .chain(term.bases.into_iter().map(Value::Basis))
                            .collect(),
                    }
                    .simplify(basis);

                    if let Value::Constant(-1) = term_bases.values[0] {
                        new_basis.values.push(Value::Constant(-1));
                    }
                    new_basis.values.push(Value::Expression(term.expression));
                    new_basis
                })
                .collect(),
        }
    }

    pub fn regressive(&self, other: &Self, basis: &Basis) -> Self {
        self.dual(basis)
            .simplify(basis)
            .wedge(&other.dual(basis).simplify(basis), basis)
            .simplify(basis)
            .dual_inverse(basis)
    }
}

#[derive(Debug, Clone)]
pub struct GATerm {
    pub bases: Vec<BasisIndex>,
    pub expression: Expression,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Value::Variable(ref name) => write!(f, "{name}"),
            Value::Constant(value) => write!(f, "{value}"),
            Value::Expression(ref expression) => write!(f, "({expression})"),
            Value::Basis(BasisIndex(index)) => write!(f, "e{index}"),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.values.is_empty() {
            return write!(f, "1");
        }

        for (i, value) in self.values.iter().enumerate() {
            if i > 0 {
                write!(f, "*")?;
            }
            write!(f, "{value}")?;
        }
        Ok(())
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.terms.is_empty() {
            return write!(f, "0");
        }

        for (i, term) in self.terms.iter().enumerate() {
            if i > 0 {
                write!(f, " + ")?;
            }
            write!(f, "{term}")?;
        }
        Ok(())
    }
}
