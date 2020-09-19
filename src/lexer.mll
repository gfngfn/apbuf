{
  open Types
  open Parser

  exception LexingError of error

  let get_pos = Range.from_lexbuf

  let fail (e : error) =
    raise (LexingError(e))
}

let space = [' ' '\t']
let break = ('\r' '\n' | '\r' | '\n')
let capital = ['A'-'Z']
let small = ['a'-'z']
let digit = ['0'-'9']
let lower = (small (small | capital | digit | "_")*)
let upper = (capital (small | capital | digit | "_")*)

rule token = parse
| space { token lexbuf }
| break { Lexing.new_line lexbuf; token lexbuf }
| ":=" { DEFEQ(get_pos lexbuf) }
| "=" { EQ(get_pos lexbuf) }
| "|" { BAR(get_pos lexbuf) }
| "{" { BRECORD(get_pos lexbuf) }
| "}" { ERECORD(get_pos lexbuf) }
| "(" { LPAREN(get_pos lexbuf) }
| ")" { RPAREN(get_pos lexbuf) }
| ":" { COLON(get_pos lexbuf) }
| "," { COMMA(get_pos lexbuf) }
| ("@" (lower as s)) {
    match s with
    | "output"           -> META_OUTPUT(get_pos lexbuf)
    | "language_version" -> META_LANGUAGE_VERSION(get_pos lexbuf)
    | "external"         -> META_EXTERNAL(get_pos lexbuf)
    | _                  -> fail (UnknownMeta(s))
  }
| ("$" (lower as x)) { VARIABLE(get_pos lexbuf, x) }
| "\"" { string (get_pos lexbuf) (Buffer.create 256) lexbuf }
| "/*" { comment (get_pos lexbuf) lexbuf; token lexbuf }
| lower { LOWER(get_pos lexbuf, Lexing.lexeme lexbuf) }
| upper { UPPER(get_pos lexbuf, Lexing.lexeme lexbuf) }
| eof { EOI }
| _ as c { fail (LexingInvalidCharacter{ character = c; range = get_pos lexbuf }) }

and string start buf = parse
| "\\\"" {
    Buffer.add_char buf '"';
    string start buf lexbuf
  }
| ([^ '\\' '"' '\r' '\n']+ as s) {
    Buffer.add_string buf s;
    string start buf lexbuf
   }
| '"' {
    let last = get_pos lexbuf in
    let s = Buffer.contents buf in
    STRING(Range.unite start last, s)
  }
| (eof | '\r' | '\n') {
    fail (EndOfLineInsideStringLiteral{ start = start; })
  }

and comment start = parse
| "*/" { () }
| eof  { fail (EndOfLineInsideComment{ start = start; }) }
| _    { comment start lexbuf }
