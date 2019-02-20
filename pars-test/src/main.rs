use pars_derive::*;

#[derive(ParsFromStr)]
#[pars = "$name: ($x, $y)"] // ?
struct NamedPos {
    name: String,
    x: isize,
    y: isize,
}

fn main() {
    assert_eq!(our_fun_function(), "$name: ($x, $y)");
}
