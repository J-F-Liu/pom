#![feature(conservative_impl_trait)]
extern crate pom;
use pom::{Parser, Error};
use pom::combinator::*;
use pom::char_class::*;

use std::str::from_utf8;

fn spaces<'a>() -> Combinator<impl Parser<'a, u8, Output=()>> {
	one_of(b" ").repeat(1..).discard()
}

fn identifier<'a>() -> Combinator<impl Parser<'a, u8, Output=&'a str>> {
	let id = is_a(alpha).repeat(1..) + is_a(alphanum).repeat(0..);
	id.collect().convert(from_utf8)
}

fn declaration<'a>() -> Combinator<impl Parser<'a, u8, Output=&'a str>> {
	seq(b"var") * spaces() * identifier().expect("var")
	|
	seq(b"let") * spaces() * identifier()
}

#[test]
fn test_expect() {
	let code = b"let card";
	assert_eq!(declaration().parse(code), Ok("card"));

	let code = b"var 0card";
	assert_eq!(declaration().parse(code).map_err(|err| {
		match err {
			Error::Expect{..} => true,
			_ => false,
		}
	}), Err(true));
}
