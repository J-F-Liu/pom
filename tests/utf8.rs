// Regression tests for https://github.com/J-F-Liu/pom/issues/74
extern crate pom;

use pom::utf8;

// `take_bytes` must decode at the advancing position, not re-decode the first char.
#[test]
fn take_bytes_rejects_invalid_utf8_after_first_char() {
	let input = [b'A', 0xED, 0xA0, 0x80]; // 'A' followed by ill-formed surrogate U+D800
	assert!(utf8::take_bytes(4).parse(&input).is_err());
}

// `take` had the same loop bug: `take(2)` on `[b'A', 0xff]` must fail, not UB.
#[test]
fn take_rejects_invalid_utf8_after_first_char() {
	let input = [b'A', 0xff];
	assert!(utf8::take(2).parse(&input).is_err());
}

// `collect` on a user-constructed parser must validate instead of UB via from_utf8_unchecked.
#[test]
fn collect_rejects_invalid_utf8_from_custom_parser() {
	let parser = utf8::Parser::new(|_input: &[u8], start: usize| Ok(((), start + 1)));
	assert!(parser.collect().parse(&[0xff]).is_err());
}

// The fixed loops must still accept valid multi-byte input and advance correctly.
#[test]
fn take_and_skip_advance_by_char() {
	let input = "éx".as_bytes();
	assert_eq!(utf8::take(2).parse(input), Ok("éx"));
	assert_eq!(utf8::skip(2).parse(input), Ok(()));
	assert_eq!(utf8::take(1).parse_at(input, 2), Ok(("x", 3)));
}

// `take_bytes`/`skip_bytes` reject a range that splits a UTF-8 character.
#[test]
fn take_bytes_rejects_split_char() {
	let input = "é".as_bytes();
	assert!(utf8::take_bytes(1).parse(input).is_err());
	assert!(utf8::skip_bytes(1).parse(input).is_err());
	assert_eq!(utf8::take_bytes(2).parse(input), Ok("é"));
}
