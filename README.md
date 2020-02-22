
# \[WIP\] APBuf (Algebraic Protocol Buffers)

## Summary

*APBuf* (*Algebraic Protocol Buffers*) is a system for generating decoders/encoders from a single configuration file that specifies the data format. It is characterized by the native support of “labeled direct sum” (which corresponds to ADTs (= algebraic data types) in Haskell, OCaml, Elm, etc.).

* Taget formats:
  - JSON
* Target languages:
  - Elm [WIP]


## Syntax of configuration files

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


## Features we want to support in the future

* Automated check for asserting backward compatibility of the update of data formats
