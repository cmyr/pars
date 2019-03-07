use pars;
use pars::ParsFromStr;

#[pars::re(r"(\w+): \(([+\-\d]+), ([+\-\d]+)\)")]
#[derive(Debug, PartialEq)]
struct NamedPos2 {
    name: String,
    x: isize,
    y: isize,
}

fn main() {
    let named_pos = NamedPos2::pars_from_str("hello: (32, -420)").unwrap();
    assert_eq!(named_pos, NamedPos2 { name: "hello".into(), x: 32, y: -420 });
}
