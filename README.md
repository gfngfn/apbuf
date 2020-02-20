
# \[WIP\] APBuf (Algebraic Protocol Buffers)

```
// declarations
decls ::=
  | ident ':=' msg decls
  | (empty)

// a message format definition
msg ::=
  | ident
  | '{' record '}'
  | '(' variant ')'

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
