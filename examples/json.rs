#![feature(conservative_impl_trait)]

extern crate pom;
use pom::char_class::hex_digit;
use pom::parser::*;
use pom::Result;

use std::str::{self, FromStr};
use std::char::{decode_utf16, REPLACEMENT_CHARACTER};
use std::collections::HashMap;


#[derive(Debug, PartialEq)]
pub enum JsonValue {
	Null,
	Bool(bool),
	Str(String),
	Num(f64),
	Array(Vec<JsonValue>),
	Object(HashMap<String,JsonValue>)
}

fn space<'a>() -> impl Parser<'a, u8, ()> {
	discard(repeat(one_of(b" \t\r\n"), 0..))
}

fn number<'a>() -> impl Parser<'a, u8, f64> {
	let integer = Alt(discard((one_of(b"123456789"), repeat(one_of(b"0123456789"), 0..))), discard(sym(b'0')));
	let frac = (sym(b'.'), repeat(one_of(b"0123456789"), 1..));
	let exp = (one_of(b"eE"), opt(one_of(b"+-")), repeat(one_of(b"0123456789"), 1..));
	let number = (opt(sym(b'-')), integer, opt(frac), opt(exp));
	convert(collect(number), |v|f64::from_str(str::from_utf8(v).unwrap()))
}

fn string<'a>() -> impl Parser<'a, u8, String> {
	let special_char = Alt3(
		[sym(b'\\'), sym(b'/'), sym(b'"')],
		Alt(map(sym(b'b'), |_|b'\x08'), map(sym(b'f'), |_|b'\x0C')),
		Alt3(map(sym(b'n'), |_|b'\n'), map(sym(b'r'), |_|b'\r'), map(sym(b't'), |_|b'\t'))
	);
	let escape_sequence = map((sym(b'\\'), special_char), |(_, c)|c);
	let char_string = convert(repeat(Alt(none_of(b"\\\""), escape_sequence), 1..), |bytes|String::from_utf8(bytes));
	// let utf16_char = sym(b'\\') * sym(b'u') * is_a(hex_digit).repeat(4..5).convert(|digits|u16::from_str_radix(&String::from_utf8(digits).unwrap(), 16));
	// let utf16_string = utf16_char.repeat(1..).map(|chars|decode_utf16(chars).map(|r| r.unwrap_or(REPLACEMENT_CHARACTER)).collect::<String>());
	// let string = sym(b'"') * (char_string | utf16_string).repeat(0..) - sym(b'"');
	// string.map(|strings|strings.concat())
	map((sym(b'"'), char_string, sym(b'"')), |(_, v, _)|v)
}

fn array<'a>() -> impl Parser<'a, u8, Vec<JsonValue>> {
	let elems = list(value, (sym(b','), space()));
	map((sym(b'['), space(), elems, sym(b']')), |(_, _, v, _)|v)
}

fn object<'a>() -> impl Parser<'a, u8, HashMap<String, JsonValue>> {
	let member = map((string(), (space(), sym(b':'), space()), value), |(k, _, v)|(k, v));
	let members = list(member, (sym(b','), space()));
	let obj = (sym(b'{'), space(), members, sym(b'}'));
	map(obj, |(_, _, members, _)|members.into_iter().collect::<HashMap<_,_>>())
}

fn value<'a>(input: &'a [u8], start: usize) -> Result<(JsonValue, usize)> {
	map((
		Alt3(
		  Alt3(map(seq(b"null"), |_|JsonValue::Null), map(seq(b"true"), |_|JsonValue::Bool(true)), map(seq(b"false"), |_|JsonValue::Bool(false))),
		  Alt(map(number(), |num|JsonValue::Num(num)), map(string(), |text|JsonValue::Str(text))),
		  Alt(map(array(), |arr|JsonValue::Array(arr)), map(object(), |obj|JsonValue::Object(obj)))
		),
		space()),
	|(v, _)|v).parse(input, start)
}

pub fn json<'a>() -> impl Parser<'a, u8, JsonValue> {
	map((space(), value, end()), |(_, v, _)|v)
}

#[allow(dead_code)]
fn main() {
	let test = br#"
	{
		"Image": {
			"Width":  800,
			"Height": 600,
			"Title":  "View from 15th Floor",
			"Thumbnail": {
				"Url":    "http://www.example.com/image/481989943",
				"Height": 125,
				"Width":  100
			},
			"Animated" : false,
			"IDs": [116, 943, 234, 38793]
		}
	}"#;

	println!("{:?}", json().parse(test, 0));
}
