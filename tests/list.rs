extern crate pom;
use pom::Parser;
use pom::combinator::*;

fn spaces<'a>() -> Combinator<impl Parser<'a, u8, Output=()>> {
	one_of(b" ").repeat(1..).discard()
}

fn works<'a>() -> Combinator<impl Parser<'a, u8, Output=Vec<u8>>> {
	list(one_of(b"abc"), spaces() * seq(b"and") - spaces())
}

fn dangle<'a>() -> Combinator<impl Parser<'a, u8, Output=(Vec<u8>, &'a [u8])>> {
	list(one_of(b"abc"), spaces() * seq(b"and") - spaces()) + seq(b" and")
}

#[test]
fn test_list() {
	let one = b"a and b and c";
	assert_eq!(works().parse(one), Ok(vec![b'a', b'b', b'c']));

	let two = b"a and b and c and ";
	assert_eq!(dangle().parse(two), Ok((vec![b'a', b'b', b'c'], &[b' ', b'a', b'n', b'd'][..])));
}
