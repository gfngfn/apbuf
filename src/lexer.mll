{
  open Parser
}

let space = [' ' '\t']
let break = ('\r' '\n' | '\r' | '\n')
let capital = ['A'-'Z']
let small = ['a'-'z']
let digit = ['0'-'9']
let identifier = (small (small | capital | digit | "_")*)
let constructor = (capital (small | capital | digit | "_")*)

rule token = parse
| space { token lexbuf }
| break { token lexbuf }
| ":=" { DEFEQ }
| "|" { BAR }
| "{" { BRECORD }
| "}" { ERECORD }
| "(" { LPAREN }
| ")" { RPAREN }
| ":" { COLON }
| "," { COMMA }
| ("@" (identifier as s)) {
    match s with
    | "output" -> META_OUTPUT
    | _        -> failwith "error at lexer (1)"
  }
| ("$" (identifier as x)) { VARIABLE(x) }
| "\"" { string (Buffer.create 256) lexbuf }
| identifier { IDENTIFIER(Lexing.lexeme lexbuf) }
| constructor { CONSTRUCTOR(Lexing.lexeme lexbuf) }
| eof { EOI }
| _ { failwith "error at lexer (2)" }

and string buf = parse
| "\\\"" { Buffer.add_char buf '"'; string buf lexbuf }
| ([^ '\\' '"' '\r' '\n']+ as s) { Buffer.add_string buf s; string buf lexbuf }
| '"' { let s = Buffer.contents buf in STRING(s) }
| (eof | '\r' | '\n') { failwith "error at lexer (3)" }
