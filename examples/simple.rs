use pom::parser::*;

fn main() {
	let input = b"abcde";
	let parser = sym(b'a') * none_of(b"AB") - sym(b'c') + seq(b"de");
	let output = parser.parse(input);
	// assert_eq!(output, Ok( (b'b', &b"de"[..]) ) );
	println!("{:?}", output);
}
