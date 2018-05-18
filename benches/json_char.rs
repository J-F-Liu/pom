#![feature(test)]

extern crate test;
use test::Bencher;

use std::fs::File;
use std::io::Read;

extern crate pom;

#[path = "../examples/json_char.rs"]
mod json;

#[bench]
fn json_char(b: &mut Bencher) {
	let mut file = File::open("assets/data.json").unwrap();
	let mut data = String::new();
	file.read_to_string(&mut data).unwrap();
	let data: Vec<char> = data.chars().collect();

	b.iter(|| {
		json::json().parse(&data).ok();
	});
}
