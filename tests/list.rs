extern crate pom;

use pom::DataInput;
use pom::parser::*;

fn spaces() -> Parser<'static, u8, ()> {
	one_of(b" ").repeat(1..).discard()
}

fn works() -> Parser<'static, u8, Vec<u8>> {
	list(one_of(b"abc"), spaces() * seq(b"and") - spaces())
}

fn dangle() -> Parser<'static, u8, (Vec<u8>, Vec<u8>)> {
	list(one_of(b"abc"), spaces() * seq(b"and") - spaces()) + seq(b" and")
}

#[test]
fn test_list() {
	let mut one = DataInput::new(b"a and b and c");
	assert_eq!(works().parse(&mut one), Ok(vec![b'a', b'b', b'c']));

	let mut two = DataInput::new(b"a and b and c and ");
	assert_eq!(dangle().parse(&mut two), Ok((vec![b'a', b'b', b'c'], vec![b' ', b'a', b'n', b'd'])));
}
