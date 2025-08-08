# `geometric_algebra`

[![Latest Version](https://img.shields.io/crates/v/geometric_algebra.svg)](https://crates.io/crates/geometric_algebra)
[![Rust Documentation](https://docs.rs/geometric_algebra/badge.svg)](https://docs.rs/geometric_algebra)
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
    s: f32,
}
impl Scalar {
    pub fn zero() -> Self {
        Self {
            s: <f32 as ::core::convert::From<i8>>::from(0),
        }
    }
}
pub struct Vector {
    e0: f32,
    e1: f32,
    e2: f32,
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
    e0e1: f32,
    e0e2: f32,
    e1e2: f32,
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
    e0e1e2: f32,
}
impl Trivector {
    pub fn zero() -> Self {
        Self {
            e0e1e2: <f32 as ::core::convert::From<i8>>::from(0),
        }
    }
}
pub struct Rotor {
    s: f32,
    e0e1: f32,
    e0e2: f32,
    e1e2: f32,
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
