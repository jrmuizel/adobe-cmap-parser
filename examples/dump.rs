extern crate cmap;
use cmap::*;
use std::fs::File;
use std::io::{Read, BufReader};
use std::path::Path;


fn main() {

    let f = File::open("example.cmap").unwrap();
    let mut f = BufReader::new(f);
    let mut contents = Vec::new();
    f.read_to_end(&mut contents);

    let lexed = parse(&contents).expect("failed to parse");
    println!("{:?}", lexed);

    println!("map {:?}", get_unicode_map(&contents));

}
