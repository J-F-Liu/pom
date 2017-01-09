extern crate pom;
use pom::{Input, Train};
use pom::parser::*;

use std::str::FromStr;
use std::iter::FromIterator;
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

fn space() -> Parser<char, ()> {
	one_of(" \t\r\n").repeat(0..).discard()
}

fn number() -> Parser<char, f64> {
	let integer = one_of("123456789") - one_of("0123456789").repeat(0..) | term('0');
	let frac = term('.') + one_of("0123456789").repeat(1..);
	let exp = one_of("eE") + one_of("+-").opt() + one_of("0123456789").repeat(1..);
	let number = term('-').opt() + integer + frac.opt() + exp.opt();
	number.collect().map(|v|String::from_iter(v)).map(|s|f64::from_str(&s).unwrap())
}

fn string() -> Parser<char, String> {
	let special_char = term('\\') | term('/') | term('"')
		| term('b').map(|_|'\x08') | term('f').map(|_|'\x0C')
		| term('n').map(|_|'\n') | term('r').map(|_|'\r') | term('t').map(|_|'\t');
	let escape_sequence = term('\\') * special_char;
	let string = term('"') * (none_of("\\\"") | escape_sequence).repeat(0..) - term('"');
	string.map(|v|String::from_iter(v))
}

fn array() -> Parser<char, Vec<JsonValue>> {
	let elems = list(call(value), term(',') * space());
	let arr = term('[') * space() * elems.opt() - term(']');
	arr.map(|elems|elems.unwrap_or(vec![]))
}

fn object() -> Parser<char, HashMap<String, JsonValue>> {
	let member = string() - space() - term(':') - space() + call(value);
	let members = list(member, term(',') * space());
	let obj = term('{') * space() * members.opt() - term('}');
	obj.map(|members|members.unwrap_or(vec![]).into_iter().collect::<HashMap<String,JsonValue>>())
}

fn value() -> Parser<char, JsonValue> {
	( seq("null").map(|_|JsonValue::Null)
	| seq("true").map(|_|JsonValue::Bool(true))
	| seq("false").map(|_|JsonValue::Bool(false))
	| number().map(|num|JsonValue::Num(num))
	| string().map(|text|JsonValue::Str(text))
	| array().map(|arr|JsonValue::Array(arr))
	| object().map(|obj|JsonValue::Object(obj))
	) - space()
}

pub fn json() -> Parser<char, JsonValue> {
	space() * value() - eof()
}

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
        }
    }"#.knots();

	let mut input = Input::new(test.as_slice());
	println!("{:?}", json().parse(&mut input));
}
