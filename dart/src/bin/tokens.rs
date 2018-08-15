extern crate gll_dart;
use gll_dart::with_tokens;
use std::env;
use std::fs;

fn main() {
    with_tokens(
        &fs::read_to_string(env::args().nth(1).unwrap()).unwrap(),
        |tokens| {
            let parser = tokens.parser;

            let v = tokens.collect::<Result<Vec<_>, _>>().unwrap();

            for token in v {
                println!("{:?}: {:?}", parser.input(token.node.range), token);
            }
        },
    );
}
