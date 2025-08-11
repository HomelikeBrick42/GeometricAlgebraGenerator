# `ga_generator`

[![Latest Version](https://img.shields.io/crates/v/ga_generator.svg)](https://crates.io/crates/ga_generator)
[![Rust Documentation](https://docs.rs/ga_generator/badge.svg)](https://docs.rs/ga_generator)
![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)

A crate for generating geometric algebra equations for any algebra and dimension

```rust
ga_generator::ga! {
    element_type = f32;
    scalar_name = s;
    elements = [e0 = zero, e1 = positive_one, e2 = positive_one];

    group Scalar = s;
    group Vector = e0 + e1 + e2;
    group Bivector = Vector ^ Vector;
    group Trivector = Bivector ^ Vector;
    group Rotor = Scalar + Bivector;

    fn test(a: Vector, b: Vector) -> Rotor {
        return a * b;
    }

    fn project(a: Vector, b: Bivector) -> Vector {
        let perp = a | b;
        return perp * b;
    }
}
```

<details class="custom"><summary>The usage above expands to something like this</summary>

```rust
pub struct Scalar {
    pub s: f32,
}
impl ::core::clone::Clone for Scalar
where
    for<'__> f32: ::core::clone::Clone,
{
    fn clone(&self) -> Self {
        Self {
            s: <f32 as ::core::clone::Clone>::clone(&self.s),
        }
    }
}
impl ::core::marker::Copy for Scalar
where
    for<'__> f32: ::core::marker::Copy,
{}
impl ::core::fmt::Debug for Scalar
where
    for<'__> f32: ::core::fmt::Debug,
{
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Scalar").field("s", &self.s).finish()
    }
}
impl Scalar {
    pub fn zero() -> Self {
        Self {
            s: <f32 as ::core::convert::From<i8>>::from(0),
        }
    }
}
pub struct Vector {
    pub e0: f32,
    pub e1: f32,
    pub e2: f32,
}
impl ::core::clone::Clone for Vector
where
    for<'__> f32: ::core::clone::Clone,
{
    fn clone(&self) -> Self {
        Self {
            e0: <f32 as ::core::clone::Clone>::clone(&self.e0),
            e1: <f32 as ::core::clone::Clone>::clone(&self.e1),
            e2: <f32 as ::core::clone::Clone>::clone(&self.e2),
        }
    }
}
impl ::core::marker::Copy for Vector
where
    for<'__> f32: ::core::marker::Copy,
{}
impl ::core::fmt::Debug for Vector
where
    for<'__> f32: ::core::fmt::Debug,
{
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Vector")
            .field("e0", &self.e0)
            .field("e1", &self.e1)
            .field("e2", &self.e2)
            .finish()
    }
}
impl Vector {
    pub fn zero() -> Self {
        Self {
            e0: <f32 as ::core::convert::From<i8>>::from(0),
            e1: <f32 as ::core::convert::From<i8>>::from(0),
            e2: <f32 as ::core::convert::From<i8>>::from(0),
        }
    }
}
pub struct Bivector {
    pub e0e1: f32,
    pub e0e2: f32,
    pub e1e2: f32,
}
impl ::core::clone::Clone for Bivector
where
    for<'__> f32: ::core::clone::Clone,
{
    fn clone(&self) -> Self {
        Self {
            e0e1: <f32 as ::core::clone::Clone>::clone(&self.e0e1),
            e0e2: <f32 as ::core::clone::Clone>::clone(&self.e0e2),
            e1e2: <f32 as ::core::clone::Clone>::clone(&self.e1e2),
        }
    }
}
impl ::core::marker::Copy for Bivector
where
    for<'__> f32: ::core::marker::Copy,
{}
impl ::core::fmt::Debug for Bivector
where
    for<'__> f32: ::core::fmt::Debug,
{
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Bivector")
            .field("e0e1", &self.e0e1)
            .field("e0e2", &self.e0e2)
            .field("e1e2", &self.e1e2)
            .finish()
    }
}
impl Bivector {
    pub fn zero() -> Self {
        Self {
            e0e1: <f32 as ::core::convert::From<i8>>::from(0),
            e0e2: <f32 as ::core::convert::From<i8>>::from(0),
            e1e2: <f32 as ::core::convert::From<i8>>::from(0),
        }
    }
}
pub struct Trivector {
    pub e0e1e2: f32,
}
impl ::core::clone::Clone for Trivector
where
    for<'__> f32: ::core::clone::Clone,
{
    fn clone(&self) -> Self {
        Self {
            e0e1e2: <f32 as ::core::clone::Clone>::clone(&self.e0e1e2),
        }
    }
}
impl ::core::marker::Copy for Trivector
where
    for<'__> f32: ::core::marker::Copy,
{}
impl ::core::fmt::Debug for Trivector
where
    for<'__> f32: ::core::fmt::Debug,
{
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Trivector").field("e0e1e2", &self.e0e1e2).finish()
    }
}
impl Trivector {
    pub fn zero() -> Self {
        Self {
            e0e1e2: <f32 as ::core::convert::From<i8>>::from(0),
        }
    }
}
pub struct Rotor {
    pub s: f32,
    pub e0e1: f32,
    pub e0e2: f32,
    pub e1e2: f32,
}
impl ::core::clone::Clone for Rotor
where
    for<'__> f32: ::core::clone::Clone,
{
    fn clone(&self) -> Self {
        Self {
            s: <f32 as ::core::clone::Clone>::clone(&self.s),
            e0e1: <f32 as ::core::clone::Clone>::clone(&self.e0e1),
            e0e2: <f32 as ::core::clone::Clone>::clone(&self.e0e2),
            e1e2: <f32 as ::core::clone::Clone>::clone(&self.e1e2),
        }
    }
}
impl ::core::marker::Copy for Rotor
where
    for<'__> f32: ::core::marker::Copy,
{}
impl ::core::fmt::Debug for Rotor
where
    for<'__> f32: ::core::fmt::Debug,
{
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Rotor")
            .field("s", &self.s)
            .field("e0e1", &self.e0e1)
            .field("e0e2", &self.e0e2)
            .field("e1e2", &self.e1e2)
            .finish()
    }
}
impl Rotor {
    pub fn zero() -> Self {
        Self {
            s: <f32 as ::core::convert::From<i8>>::from(0),
            e0e1: <f32 as ::core::convert::From<i8>>::from(0),
            e0e2: <f32 as ::core::convert::From<i8>>::from(0),
            e1e2: <f32 as ::core::convert::From<i8>>::from(0),
        }
    }
}
pub fn test(a: Vector, b: Vector) -> Rotor {
    let _0 = a.e0;
    let _1 = a.e1;
    let _2 = a.e2;
    let _3 = b.e0;
    let _4 = b.e1;
    let _5 = b.e2;
    #[allow(clippy::needless_update)]
    Rotor {
        s: (_1 * _4) + (_2 * _5),
        e0e1: (_0 * _4) + (<f32 as ::core::convert::From<i8>>::from(-1i8) * _1 * _3),
        e0e2: (_0 * _5) + (<f32 as ::core::convert::From<i8>>::from(-1i8) * _2 * _3),
        e1e2: (_1 * _5) + (<f32 as ::core::convert::From<i8>>::from(-1i8) * _2 * _4),
        ..Rotor::zero()
    }
}
pub fn project(a: Vector, b: Bivector) -> Vector {
    let _0 = a.e0;
    let _1 = a.e1;
    let _2 = a.e2;
    let _3 = b.e0e1;
    let _4 = b.e0e2;
    let _5 = b.e1e2;
    #[allow(clippy::needless_update)]
    Vector {
        e0: (_2 * _3 * _5)
            + (<f32 as ::core::convert::From<i8>>::from(-1i8) * _1 * _4 * _5),
        e1: (<f32 as ::core::convert::From<i8>>::from(-1i8) * _1 * _5 * _5),
        e2: (<f32 as ::core::convert::From<i8>>::from(-1i8) * _2 * _5 * _5),
        ..Vector::zero()
    }
}
```

</details>
