use pars;

//#[pars::fmt("$name: ($x, $y)")]
//#[derive(Debug, PartialEq)]
//struct NamedPos {
    //name: String,
    //x: isize,
    //y: isize,
//}

#[pars::re(r"(\w+): \(([+-\d]+), ([+-\d]_)\)")]
#[pars::fmt("$name: ($x, $y)")]
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
    println!("hi!")
}

#[cfg(test)]
mod tests {
    use super::main;
    #[test]
    fn test() {
        main()
    }
}

