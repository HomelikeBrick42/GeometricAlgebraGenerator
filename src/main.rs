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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expression {
    pub terms: Vec<Term>,
}

impl Expression {
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

fn main() {
    let basis = Basis {
        bases: vec![
            SquaresTo::Zero,
            SquaresTo::PositiveOne,
            SquaresTo::PositiveOne,
            SquaresTo::PositiveOne,
        ],
    };

    let a = Expression {
        terms: vec![
            Term {
                values: vec![Value::Variable("a1".into())],
            },
            Term {
                values: vec![
                    Value::Variable("b1".into()),
                    Value::Basis(BasisIndex(2)),
                    Value::Basis(BasisIndex(3)),
                ],
            },
            Term {
                values: vec![
                    Value::Variable("c1".into()),
                    Value::Basis(BasisIndex(1)),
                    Value::Basis(BasisIndex(3)),
                ],
            },
            Term {
                values: vec![
                    Value::Variable("d1".into()),
                    Value::Basis(BasisIndex(1)),
                    Value::Basis(BasisIndex(2)),
                ],
            },
        ],
    };
    let b = Expression {
        terms: vec![
            Term {
                values: vec![Value::Variable("a2".into())],
            },
            Term {
                values: vec![
                    Value::Variable("b2".into()),
                    Value::Basis(BasisIndex(2)),
                    Value::Basis(BasisIndex(3)),
                ],
            },
            Term {
                values: vec![
                    Value::Variable("c2".into()),
                    Value::Basis(BasisIndex(1)),
                    Value::Basis(BasisIndex(3)),
                ],
            },
            Term {
                values: vec![
                    Value::Variable("d2".into()),
                    Value::Basis(BasisIndex(1)),
                    Value::Basis(BasisIndex(2)),
                ],
            },
        ],
    };

    let expression = a.multiply(&b);
    println!("{expression}");
    let expression = expression.simplify(&basis);
    println!("{expression}");

    println!();

    let ga_terms = expression.split_into_ga_terms();
    for ga_term in &ga_terms {
        for basis in &ga_term.bases {
            print!("e{}*", basis.0);
        }
        print!("({})", ga_term.expression);
        println!();
    }
}
