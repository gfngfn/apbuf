
# \[WIP\] APBuf (Algebraic Protocol Buffers)

```
// declarations
decls ::=
  | ident ':=' msg decls
  | ident '(' params ')' ':=' msg decls
  | (empty)

params ::=
  | x paramtail

paramtail ::=
  | ',' x paramtail
  | ','
  | (empty)

// a message format definition
msg ::=
  | ident
  | ident '(' msg argtail ')'
  | '{' record '}'
  | '(' variant ')'

args ::=
  | msg argtail

argtail ::=
  | ',' msg argtail
  | ','
  | (empty)

// record fields
record ::=
  | ident ':' msg ',' record
  | ident ':' msg
  | (empty)

// variant fields
variant ::=
  | '|' ctor ':' msg variantsub
  | ctor ':' msg variantsub

variantsub ::=
  | '|' ctor ':' msg variantsub
  | (empty)
```
