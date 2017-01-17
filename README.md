# pom

PEG parser combinators created using operator overloading without macros.

[![Crates.io](https://img.shields.io/crates/v/pom.svg)](https://crates.io/crates/pom) 

## What is PEG?

PEG stands for parsing expression grammar, is a type of analytic formal grammar, i.e. it describes a formal language in terms of a set of rules for recognizing strings in the language.
Unlike CFGs, PEGs cannot be ambiguous; if a string parses, it has exactly one valid parse tree.
Each parsing function conceptually takes an input string as its argument, and yields one of the following results:
- success, in which the function may optionally move forward or consume one or more characters of the input string supplied to it, or
- failure, in which case no input is consumed.

Read more on [Wikipedia](https://en.wikipedia.org/wiki/Parsing_expression_grammar).

## What is parser combinator?

A parser combinator is a higher-order function that accepts several parsers as input and returns a new parser as its output.
Parser combinators enable a recursive descent parsing strategy that facilitates modular piecewise construction and testing.

Parsers built using combinators are straightforward to construct, readable, modular, well-structured and easily maintainable.
With operator overloading, a parser combinator can take the form of an infix operator, used to glue different parsers to form a complete rule. Parser combinators thereby enable parsers to be defined in an embedded style, in code which is similar in structure to the rules of the formal grammar.
And the code is easier to debug than macros.

## List of predefined parsers and combinators

|Basic Parsers|Description|
| --- | --- |
|empty()|Always success, consume no input.|
|end()  |Match end of input.|
|term(t)|Match a single terminal symbol *t*.|
|seq(s) |Match sequence of symbols.|
|list(p,s) |Match list of *p*, separated by *s*.|
|one_of(set) |Sucess when current input symbol is one of the set.|
|none_of(set)|Sucess when current input symbol is none of the set.|
|range(r)    |Sucess when the range contains current input symbol.|
|is_a(predict) |Sucess when predict return true on current input symbol.|
|not_a(predict)|Sucess when predict return false on current input symbol.|
|take(n)|Read *n* symbols.|
|skip(n)|Skip *n* symbols.|
|call(pf)|Call a parser factory, can used to create recursive parsers.|

|Parser Combinators|Description|
| --- | --- |
| p + q | Match p and q, if both success return a pair of results. |
| p - q | Match p and q, if both success return result of p. |
| p * q | Match p and q, if both success return result of q. |
| p >> q | Parse p and get result P, then parse and return result of q(P). |
| -p | Success when p success, doen't consume input. |
| !p | Success when p fail, doen't consume input. |
|p.opt()|Make parser optional. Returns an `Option`.|
|p.repeat(m..n)| `p.repeat(0..)` repeat p zero or more times<br>`p.repeat(1..)` repeat p one or more times<br>`p.repeat(1..4)` match p at least 1 and at most 3 times|
|p.map(f)|Convert parser result to desired value.|
|p.collect()|Collect all matched input symbols.|
|p.discard()|Discard parser output.|

## Example code
```rust
extern crate pom;
use pom::{Input};
use pom::parser::*;

let mut input = Input::new(b"abcde");
let parser = term(b'a') * none_of(b"AB") - term(b'c') + seq(b"de");
let output = parser.parse(&mut input);
assert_eq!(output, Ok( (b'b', vec![b'd', b'e']) ) );
```

### Example JSON parser
```rust
extern crate pom;
use pom::DataInput;
use pom::parser::*;

use std::str::FromStr;
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

fn space() -> Parser<u8, ()> {
	one_of(b" \t\r\n").repeat(0..).discard()
}

fn number() -> Parser<u8, f64> {
	let integer = one_of(b"123456789") - one_of(b"0123456789").repeat(0..) | term(b'0');
	let frac = term(b'.') + one_of(b"0123456789").repeat(1..);
	let exp = one_of(b"eE") + one_of(b"+-").opt() + one_of(b"0123456789").repeat(1..);
	let number = term(b'-').opt() + integer + frac.opt() + exp.opt();
	number.collect().map(|v|String::from_utf8(v).unwrap()).map(|s|f64::from_str(&s).unwrap())
}

fn string() -> Parser<u8, String> {
	let special_char = term(b'\\') | term(b'/') | term(b'"')
		| term(b'b').map(|_|b'\x08') | term(b'f').map(|_|b'\x0C')
		| term(b'n').map(|_|b'\n') | term(b'r').map(|_|b'\r') | term(b't').map(|_|b'\t');
	let escape_sequence = term(b'\\') * special_char;
	let string = term(b'"') * (none_of(b"\\\"") | escape_sequence).repeat(0..) - term(b'"');
	string.map(|v|String::from_utf8(v).unwrap())
}

fn array() -> Parser<u8, Vec<JsonValue>> {
	let elems = list(call(value), term(b',') * space());
	let arr = term(b'[') * space() * elems.opt() - term(b']');
	arr.map(|elems|elems.unwrap_or(vec![]))
}

fn object() -> Parser<u8, HashMap<String, JsonValue>> {
	let member = string() - space() - term(b':') - space() + call(value);
	let members = list(member, term(b',') * space());
	let obj = term(b'{') * space() * members.opt() - term(b'}');
	obj.map(|members|members.unwrap_or(vec![]).into_iter().collect::<HashMap<_,_>>())
}

fn value() -> Parser<u8, JsonValue> {
	( seq(b"null").map(|_|JsonValue::Null)
	| seq(b"true").map(|_|JsonValue::Bool(true))
	| seq(b"false").map(|_|JsonValue::Bool(false))
	| number().map(|num|JsonValue::Num(num))
	| string().map(|text|JsonValue::Str(text))
	| array().map(|arr|JsonValue::Array(arr))
	| object().map(|obj|JsonValue::Object(obj))
	) - space()
}

pub fn json() -> Parser<u8, JsonValue> {
	space() * value() - end()
}

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

	let mut input = DataInput::new(test);
	println!("{:?}", json().parse(&mut input));
}
```
You can run this example with the following command:
```
cargo run --example json
```

## Benchmark

| Parser           | Time to parse the same JSON file |
|------------------|----------------------------------|
| pom: json_byte   | 3,962 ns/iter (+/- 258)          |
| pom: json_char   | 4,194 ns/iter (+/- 87)           |
| [pest](https://github.com/dragostis/pest): json_char  | 13,359 ns/iter (+/- 811)          |
