#![feature(test)]
#![feature(conservative_impl_trait)]
extern crate test;
use test::Bencher;

use std::fs::File;
use std::io::Read;

extern crate pom;

#[path = "../examples/json.rs"]
mod json;

#[bench]
fn json_byte(b: &mut Bencher) {
	let mut file = File::open("assets/data.json").unwrap();
	let mut data = Vec::new();
	file.read_to_end(&mut data).unwrap();

	b.iter(|| {
		json::json().parse(&data).ok();
	});
}
