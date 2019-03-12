use pars;
use pars::ParsFromStr;

#[test]
fn regex_parse_smoke_test() {
    #[pars::re(r"(\w+): \(([+\-\d]+), ([+\-\d]+)\)")]
    #[derive(Debug, PartialEq)]
    struct NamedPos2 {
        name: String,
        x: isize,
        y: isize,
    }

    let named_pos = NamedPos2::pars_from_str("hello: (32, -420)").unwrap();
    assert_eq!(named_pos, NamedPos2 { name: "hello".into(), x: 32, y: -420 });
}

#[test]
fn fmt_parse_smoke_test() {
    #[pars::fmt("(#{x}, #{y}) (#{width}, #{height})")]
    #[derive(Debug, PartialEq)]
    struct Rect {
        x: usize,
        y: usize,
        width: usize,
        height: usize,
    }

    let rect = Rect::pars_from_str("(4, 10) (42, 69)").unwrap();
    assert_eq!(rect, Rect { x: 4, y: 10, width: 42, height: 69 });

    let not_rect = Rect::pars_from_str("4, 5, (420 808)");
    assert!(not_rect.is_err());
}

#[test]
fn fmt_derive_from_str() {
    #[pars::fmt("(#{x}, #{y}) (#{width}, #{height})", gen_from_str)]
    #[derive(Debug, PartialEq)]
    struct Rect {
        x: usize,
        y: usize,
        width: usize,
        height: usize,
    }

    let rect: Rect = "(4, 10) (42, 69)".parse().unwrap();
    assert_eq!(rect, Rect { x: 4, y: 10, width: 42, height: 69 });

    let not_rect = Rect::pars_from_str("4, 5, (420 808)");
    assert!(not_rect.is_err());
}

#[test]
fn re_derive_from_str() {
    #[pars::re(r"(\w+): \(([+\-\d]+), ([+\-\d]+)\)", gen_from_str)]
    #[derive(Debug, PartialEq)]
    struct NamedPos2 {
        name: String,
        x: isize,
        y: isize,
    }

    let named_pos: NamedPos2 = "hello: (32, -420)".parse().unwrap();
    assert_eq!(named_pos, NamedPos2 { name: "hello".into(), x: 32, y: -420 });
}

#[test]
fn named_fields_out_of_order() {
    #[pars::fmt("#{name}: #{age}")]
    #[derive(Debug, PartialEq)]
    struct Person {
        age: u32,
        name: String,
    }

    let someone = Person::pars_from_str("Bactra: 42").unwrap();
    assert_eq!(someone, Person { age: 42, name: "Bactra".into() })
}

#[test]
fn fmt_tuple_struct() {
    #[pars::fmt("(#{x}, #{y})")]
    struct Coord(usize, usize);

    let Coord(x, y) = Coord::pars_from_str("(42, 5)").unwrap();
    assert_eq!((x, y), (42, 5));
}

#[test]
fn re_tuple_struct() {
    #[pars::re(r"\((.+), (.+)\)")]
    struct Coord(usize, usize);

    let Coord(x, y) = Coord::pars_from_str("(42, 5)").unwrap();
    assert_eq!((x, y), (42, 5));
}
