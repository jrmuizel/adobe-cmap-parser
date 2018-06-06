extern crate adobe_cmap_parser;
use adobe_cmap_parser::*;
use std::fs::File;
use std::io::{Read, BufReader};
use std::path::Path;
use std::env;

fn main() {

    let f = File::open(env::args().nth(1).unwrap()).unwrap();
    let mut f = BufReader::new(f);
    let mut contents = Vec::new();
    f.read_to_end(&mut contents);

    let lexed = parse(&contents).expect("failed to parse");
    println!("{:?}", lexed);

    println!("map {:?}", get_unicode_map(&contents));

}
