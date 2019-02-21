use pars_derive::*;

#[derive(Debug,PartialEq,ParsFromStr)]
#[pars = "$name: ($x, $y)"] // ?
struct NamedPos {
    name: String,
    x: isize,
    y: isize,
}

fn main() {
    let named_pos = our_fun_function("someone: (3, 4)");
    assert_eq!(named_pos.x, 3);
    assert_eq!(named_pos.y, 4);
    assert_eq!(named_pos.name, "someone");
    println!("we got {:?}", named_pos);
}

#[cfg(test)]
mod tests {
    use super::main;
    #[test]
    fn test() {
        main()
    }
}

