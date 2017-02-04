#![feature(test)]
extern crate test;
use test::Bencher;

use std::fs::File;
use std::io::Read;

extern crate pom;
use pom::TextInput;
use pom::Input;

#[path = "../examples/json_char.rs"]
mod json;

#[bench]
fn json_char(b: &mut Bencher) {
	let mut file = File::open("assets/data.json").unwrap();
	let mut data = String::new();
	file.read_to_string(&mut data).unwrap();

	let mut input = TextInput::new(&data);
	b.iter(|| {
		input.jump_to(0);
		json::json().parse(&mut input).ok();
	});
}
