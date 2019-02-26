use pars;

//#[pars::fmt("$name: ($x, $y)")]
//#[derive(Debug, PartialEq)]
//struct NamedPos {
    //name: String,
    //x: isize,
    //y: isize,
//}

#[pars::re(r"(\w+): \(([+\-\d]+), ([+\-\d]+)\)")]
#[derive(Debug, PartialEq)]
struct NamedPos2 {
    name: String,
    x: isize,
    y: isize,
}

fn main() {
    //let named_pos = our_fun_function("someone: (3, 4)");
    //assert_eq!(named_pos.x, 3);
    //assert_eq!(named_pos.y, 4);
    //assert_eq!(named_pos.name, "someone");
    //println!("we got {:?}", named_pos);
//    let named_pos = NamedPos2::pars_from_str("hello: (32, -420)").unwrap();
//    assert_eq!(named_pos,
//               NamedPos2 {
//                   name: "hello".into(),
//                   x: 32,
//                   y: -420,
//               });
}

#[cfg(test)]
mod tests {
    use super::main;
    #[test]
    fn test() {
        main()
    }
}

