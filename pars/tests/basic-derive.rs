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
