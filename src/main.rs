geometric_algebra::ga! {
    element_type = f32;
    scalar_name = s;
    elements = [e0 = zero, e1 = positive_one, e2 = positive_one];

    group Scalar = s;
    group Vector = e0 + e1 + e2;
    group Bivector = e1*e2 + e0*e1 + e0*e2;
}

fn main() {}
