// Example shows UTF-8 combinators intermixed with binary combinators

use pom::parser::*;
use pom::utf8;

fn main() {
	// A parser for MsgPack (but only messages encoding a string)
	let testcases: [Vec<u8>; 6] = [
		 vec![0b10100100,       0b11110000, 0b10011111, 0b10100100, 0b10010100], // 🤔, max-size 31 format
		 vec![0xd9, 4,          0b11110000, 0b10011111, 0b10011000, 0b10101110], // 😮, max-size 255 format
		 vec![0xda, 0, 4,       0b11110000, 0b10011111, 0b10100100, 0b10101111], // 🤯, max-size 2^16-1 format
		 vec![0xdb, 0, 0, 0, 4, 0b11110000, 0b10011111, 0b10010010, 0b10100101], // 💥, max-size 2^32-1 format
		 vec![0xc4, 4,          0b11110000, 0b10011111, 0b10011000, 0b10101110], // Valid MsgPack, but not a string (binary)
		 vec![0b10100100,       0b10010100, 0b10100100, 0b10011111, 0b11110000], // A MsgPack string, but invalid UTF-8
	];

	const MASK:u8    = 0b11100000; // size 31 format is denoted by 3 high bits == 101
	const SIZE_31:u8 = 0b10100000;

	fn rest_as_str<'a>() -> utf8::Parser<'a, &'a str> {
		utf8::any().repeat(0..).collect()
	}

	// Demo parser does not verify that the claimed length matches the actual length (but checking so is simple with >>)
	let parser =
		  (sym(0xdb) * any().repeat(4) * rest_as_str()) // 2^32-1 format
		| (sym(0xda) * any().repeat(2) * rest_as_str()) // 2^16-1 format
		| (sym(0xd9) * any()           * rest_as_str()) // 255 format
		| (is_a(|x| x&MASK == SIZE_31) * rest_as_str()) // 31 format
		- end();

	for testcase in testcases.iter() {
		println!("{:?}", parser.parse(testcase));
	}
}
