geometric_algebra::ga! {
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

fn main() {}
