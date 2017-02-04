#![feature(test)]
extern crate test;
use test::Bencher;

use std::fs::File;
use std::io::Read;

extern crate pom;
use pom::DataInput;
use pom::Input;

#[path = "../examples/json.rs"]
mod json;

#[bench]
fn json_byte(b: &mut Bencher) {
	let mut file = File::open("assets/data.json").unwrap();
	let mut data = Vec::new();
	file.read_to_end(&mut data).unwrap();

	let mut input = DataInput::new(&data);
	b.iter(|| {
		input.jump_to(0);
		json::json().parse(&mut input).ok();
	});
}
