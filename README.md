# pom

Parser combinators created using operator overloading without macros.

## Usage

|Basic Parsers|Description|
| --- | --- |
|empty()|Always success, consume no input.|
|eof()  |Match end of file.|
|term(t)|Match terminal symbol *t*.|
|seq(s) |Match sequence of symbols.|
|one_of(set) |Sucess when current input symbol is one of the set.|
|none_of(set)|Sucess when current input symbol is none of the set.|
|range(r)|Sucess when the range contains current input symbol.|
|is_a(predict)|Sucess when predict return true on current input symbol.|
|not_a(predict)|Sucess when predict return false on current input symbol.|

|Parser Combinators|Description|
| --- | --- |
| p+q | Match p and q, if both success return a pair of results. |
| p-q | Match p and q, if both success return result of p. |
| -p | Success when p success, doen't consume input. |
| !p | Success when p fail, doen't consume input. |
|p.opt()|Make parser optional.|
|p.repeat(m..n)| `p.repeat(0..)` repeat p zero or more times<br>`p.repeat(1..)` repeat p one or more times<br>`p.repeat(1..4)` match p at least 1 and at most 3 times<br>|
|p.map(f)|Convert parser result to desired value.|

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
