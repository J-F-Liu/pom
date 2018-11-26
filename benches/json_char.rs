#![feature(test)]
extern crate test;
use self::test::Bencher;

use std::fs::File;
use std::io::Read;

extern crate pom;

#[path = "../examples/json_char.rs"]
mod json;

#[bench]
fn json_char(b: &mut Bencher) {
	let mut file = File::open("assets/data.json").unwrap();
	let mut input = String::new();
	file.read_to_string(&mut input).unwrap();
	let chars: Vec<char> = input.chars().collect();

	b.iter(|| {
		json::json().parse(&chars).ok();
	});
}
