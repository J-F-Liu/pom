extern crate pom;
use pom::combinator::*;
use pom::Parser;

use std::str::FromStr;
use std::char::{decode_utf16, REPLACEMENT_CHARACTER};
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug, PartialEq)]
pub enum JsonValue {
	Null,
	Bool(bool),
	Number(f64),
	String(String),
	Array(Vec<JsonValue>),
	Object(HashMap<String,JsonValue>)
}

fn space<'a>() -> Combinator<impl Parser<'a, char, Output=()>> {
	one_of(" \t\r\n").repeat(0..).discard()
}

fn number<'a>() -> Combinator<impl Parser<'a, char, Output=f64>> {
	let integer = one_of("123456789") - one_of("0123456789").repeat(0..) | sym('0');
	let frac = sym('.') + one_of("0123456789").repeat(1..);
	let exp = one_of("eE") + one_of("+-").opt() + one_of("0123456789").repeat(1..);
	let number = sym('-').opt() + integer + frac.opt() + exp.opt();
	number.collect().convert(|v|f64::from_str(&String::from_iter(v.to_vec())))
}

fn string<'a>() -> Combinator<impl Parser<'a, char, Output=String>> {
	let special_char = sym('\\') | sym('/') | sym('"')
		| sym('b').map(|_|'\x08') | sym('f').map(|_|'\x0C')
		| sym('n').map(|_|'\n') | sym('r').map(|_|'\r') | sym('t').map(|_|'\t');
	let escape_sequence = sym('\\') * special_char;
	let char_string = (none_of("\\\"") | escape_sequence).repeat(1..).map(String::from_iter);
	let utf16_char = comb("\\u") * is_a(|c:char|c.is_digit(16)).repeat(4).map(String::from_iter).convert(|digits|u16::from_str_radix(&digits, 16));
	let utf16_string = utf16_char.repeat(1..).map(|chars|decode_utf16(chars).map(|r| r.unwrap_or(REPLACEMENT_CHARACTER)).collect::<String>());
	let string = sym('"') * (char_string | utf16_string).repeat(0..) - sym('"');
	string.map(|strings|strings.concat())
}

fn array<'a>() -> Combinator<impl Parser<'a, char, Output=Vec<JsonValue>>> {
	let elems = list(comb(value), sym(',') + space());
	sym('[') * space() * elems - sym(']')
}

fn object<'a>() -> Combinator<impl Parser<'a, char, Output=HashMap<String, JsonValue>>> {
	let member = string() - space() - sym(':') - space() + comb(value);
	let members = list(member, sym(',') + space());
	let obj = sym('{') * space() * members - sym('}');
	obj.map(|members|members.into_iter().collect::<HashMap<_,_>>())
}

// fn value<'a>() -> Combinator<impl Parser<'a, char, Output=JsonValue>> {
fn value<'a>(input: &'a [char], start: usize) -> pom::Result<(JsonValue, usize)> {
	(( comb("null").map(|_|JsonValue::Null)
	| comb("true").map(|_|JsonValue::Bool(true))
	| comb("false").map(|_|JsonValue::Bool(false))
	| number().map(|num|JsonValue::Number(num))
	| string().map(|text|JsonValue::String(text))
	| array().map(|arr|JsonValue::Array(arr))
	| object().map(|obj|JsonValue::Object(obj))
	) - space()).0.parse(input, start)
}

pub fn json<'a>() -> Combinator<impl Parser<'a, char, Output=JsonValue>> {
	space() * comb(value) - end()
}

#[allow(dead_code)]
fn main() {
	let test = r#"
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
		},
		"escaped characters": "\u2192\uD83D\uDE00\"\t\uD834\uDD1E"
	}"#.chars().collect::<Vec<char>>();

	println!("{:?}", json().parse(&test));
}
