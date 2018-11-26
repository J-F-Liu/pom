#![feature(test)]
extern crate test;
use self::test::Bencher;

use std::fs::File;
use std::io::Read;

extern crate pom;

#[path = "../examples/json.rs"]
mod json;

#[bench]
fn json_byte(b: &mut Bencher) {
	let mut file = File::open("assets/data.json").unwrap();
	let mut input = Vec::new();
	file.read_to_end(&mut input).unwrap();

	b.iter(|| {
		json::json().parse(&input).ok();
	});
}
