#[macro_use]
extern crate criterion;
extern crate pars;

use std::str::FromStr;
use pars::ParsFromStr;


use criterion::{Benchmark, Criterion};

static SAMPLE_DATA: &'static str = include_str!("../resources/aoc_2018_day3.txt");

fn fmt_parse() -> usize {

    #[pars::fmt("##{id} @ #{x},#{y}: #{w}x#{h}")]
    #[allow(dead_code)]
    struct ClaimFmt {
        id: usize,
        x: usize,
        y: usize,
        w: usize,
        h: usize,
    }

    SAMPLE_DATA.lines()
        .map(|s| ClaimFmt::pars_from_str(s).unwrap())
        .count()
}

fn re_parse() -> usize {

    #[pars::re(r"#(.+) @ (.+),(.+): (.+)x(.+)")]
    #[allow(dead_code)]
    struct ClaimRe {
        id: usize,
        x: usize,
        y: usize,
        w: usize,
        h: usize,
    }
    SAMPLE_DATA.lines()
        .map(|s| ClaimRe::pars_from_str(s).unwrap())
        .count()
}

fn manual_parse() -> usize {

    #[allow(dead_code)]
    struct Claim {
        id: usize,
        x: usize,
        y: usize,
        w: usize,
        h: usize,
    }

    impl FromStr for Claim {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, ()> {
            let mut iter = s.split_whitespace();
            let id = iter.next().unwrap()[1..].parse().unwrap();
            iter.next(); // skip @
            let mut xy = iter.next().unwrap().split(',');
            let x = xy.next().unwrap().parse().unwrap();
            let y = xy.next().unwrap().trim_matches(':').parse().unwrap();
            let mut size = iter.next().unwrap().split('x');
            let w = size.next().unwrap().parse().unwrap();
            let h = size.next().unwrap().parse().unwrap();
            Ok(Claim { id, x, y, w, h })
        }
    }
    SAMPLE_DATA.lines()
        .map(|s| s.parse::<Claim>().unwrap())
        .count()
}

fn bench_compare(c: &mut Criterion) {
    c.bench(
        "Parsing",
        Benchmark::new("Manual", |b| b.iter(|| manual_parse()))
            .with_function("pars::fmt", |b| b.iter(|| fmt_parse()))
            .with_function("pars::re", |b| b.iter(|| re_parse()))
    );
}


criterion_group!(benches, bench_compare);
criterion_main!(benches);
