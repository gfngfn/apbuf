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
| ("$" (identifier as x)) { VARIABLE(x) }
| identifier { IDENTIFIER(Lexing.lexeme lexbuf) }
| constructor { CONSTRUCTOR(Lexing.lexeme lexbuf) }
| eof { EOI }
