use pom::parser::*;

fn example<'a>() -> Parser<'a, u8, Vec<u8>> {
	(sym(b'<') * none_of(b">").repeat(0..) - sym(b'>'))
		>> |tag| {
			(call(example) | none_of(b"<>").repeat(0..))
				- seq(b"</") - take(tag.len()).convert(move |t| if t == tag { Ok(()) } else { Err(()) })
				- sym(b'>')
		}
}

fn main() {
	let input = b"abcde";
	let parser = sym(b'a') * none_of(b"AB") - sym(b'c') + seq(b"de");
	let output = parser.parse(input);
	// assert_eq!(output, Ok( (b'b', &b"de"[..]) ) );
	println!("{:?}", output);
	println!("{:?}", example().parse("<app>bcb</app>".as_bytes()));
}
