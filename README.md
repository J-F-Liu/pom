# pom

PEG parser combinators created using operator overloading without macros.

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
|eof()  |Match end of file.|
|term(t)|Match terminal symbol *t*.|
|seq(s) |Match sequence of symbols.|
|one_of(set) |Sucess when current input symbol is one of the set.|
|none_of(set)|Sucess when current input symbol is none of the set.|
|range(r)    |Sucess when the range contains current input symbol.|
|is_a(predict) |Sucess when predict return true on current input symbol.|
|not_a(predict)|Sucess when predict return false on current input symbol.|
|take(n)|Read *n* symbols.|
|skip(n)|Skip *n* symbols.|

|Parser Combinators|Description|
| --- | --- |
| p + q | Match p and q, if both success return a pair of results. |
| p - q | Match p and q, if both success return result of p. |
| -p | Success when p success, doen't consume input. |
| !p | Success when p fail, doen't consume input. |
|p.opt()|Make parser optional.|
|p.repeat(m..n)| `p.repeat(0..)` repeat p zero or more times<br>`p.repeat(1..)` repeat p one or more times<br>`p.repeat(1..4)` match p at least 1 and at most 3 times|
|p.map(f)|Convert parser result to desired value.|
|p.collect()|Collect all matched input symbols.|
|p.discard()|Discard parser result.|

## Example code
```rust
extern crate pom;
use pom::{Input};
use pom::parser::*;

let mut input = Input::new(b"abcde");
let parser = term(b'a') + none_of(b"AB") - term(b'c') + seq(b"de");
let output = parser.parse(&mut input);
assert_eq!(output, Ok( ((b'a', b'b'), &b"de"[..]) ) );
```
