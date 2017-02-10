extern crate pom;
use pom::combinator::*;

fn main() {
	let input = b"abcde";
	let parser = sym(b'a') * one_of(b"ab") - sym(b'c') + seq(b"de");
	let output = parser.parse(input);
	assert_eq!(output, Ok( (b'b', &b"de"[..]) ) );
}
