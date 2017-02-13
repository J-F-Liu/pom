extern crate pom;
use pom::DataInput;
use pom::parser::*;

fn main() {
	let mut input = DataInput::new(b"abcde");
	let parser = sym(b'a') * none_of(b"AB") - sym(b'c') + seq(b"de");
	let output = parser.parse(&mut input);
	assert_eq!(output, Ok( (b'b', vec![b'd', b'e']) ) );
}
