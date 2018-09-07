extern crate gll_lua;
extern crate luster;
use gll_lua::check_luster::Check;
use gll_lua::parse;
use luster::parser::parse_chunk;
use std::env;
use std::fs;

fn main() {
    let src = fs::read_to_string(env::args().nth(1).unwrap()).unwrap();
    let luster_chunk = parse_chunk(src.as_bytes());
    match luster_chunk {
        Ok(luster_chunk) => parse::Chunk::parse_with(&src[..], |_, result| match result {
            Ok(chunk) => {
                if let Err(e) = chunk.check(&luster_chunk) {
                    eprintln!("Check failed: {:?}", e);
                }
            }
            Err(e) => eprintln!("Error parsing with gll: {:?}", e),
        }),
        Err(e) => eprintln!("Error parsing with luster: {:?}", e),
    }
}
