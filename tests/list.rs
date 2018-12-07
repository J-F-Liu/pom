extern crate pom;

use pom::parser::*;
use pom::Parser;

fn spaces() -> Parser<u8, ()> {
	one_of(b" ").repeat(1..).discard()
}

fn works() -> Parser<u8, Vec<u8>> {
	list(one_of(b"abc"), spaces() * seq(b"and") - spaces())
}

fn dangle() -> Parser<u8, (Vec<u8>, &'static [u8])> {
	list(one_of(b"abc"), spaces() * seq(b"and") - spaces()) + seq(b" and")
}

#[test]
fn test_list() {
	let one = b"a and b and c";
	assert_eq!(works().parse(one), Ok(vec![b'a', b'b', b'c']));

	let two = b"a and b and c and ";
	assert_eq!(
		dangle().parse(two),
		Ok((vec![b'a', b'b', b'c'], &b" and"[..]))
	);
}
