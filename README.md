
# \[WIP\] APBuf (Algebraic Protocol Buffers)

## Summary

*APBuf* (*Algebraic Protocol Buffers*) is a system for generating decoders/encoders from a single configuration file that specifies the data format. It is characterized by the native support of “labeled direct sum” (which corresponds to ADTs (= algebraic data types) in Haskell, OCaml, Elm, etc.).

* Taget formats:
  - JSON
* Target languages:
  - Elm


## Example

The following configuration file:

```
@output "elm": "./gen"

geometry :=
  | Rectangle : rectangle_info(int)
  | Circle    : circle_info(int, rational)

rectangle_info($num) := {
  upper_left  : position($num),
  lower_right : position($num),
}

circle_info($cnum, $rnum) := {
  center : position($cnum),
  radius : $rnum,
}

position($num) := { x : $num, y : $num }

rational := { numerator : int, denominator : int }
```

produces Elm code that has the following API:

```elm
module APBufGen exposing (..)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)

type Geometry
  = Rectangle (RectangleInfo Int)
  | Circle (CircleInfo Int Rational)
decodeGeometry : Decoder Geometry
encodeGeometry : Geometry -> Value

type alias RectangleInfo a = { lower_right : Position a, upper_left : Position a }
decodeRectangleInfo : Decoder a -> Decoder (RectangleInfo a)
encodeRectangleInfo : (a -> Value) -> RectangleInfo a -> Value

type alias CircleInfo c r = { center : Position c, radius : r }
decodeCircleInfo : Decoder c -> Decoder r -> Decoder (CircleInfo c r)
encodeCircleInfo : (c -> Value) -> (r -> Value) -> CircleInfo c r -> Value

type alias Position a = { x : a, y : a }
decodePosition : Decoder a -> Decoder (Position a)
encodePosition : (a -> Value) -> { x : a, y : a } -> Value

type alias Rational = { denominator : Int, numerator : Int }
decodeRational : Decoder Rational
encodeRational : Rational -> Value
```


## Syntax of configuration files

```
toplevel ::=
  | meta decls

meta ::=
  | '@output' s ':' s

// declarations
decls ::=
  | ident ':=' msg decls
  | ident '(' params ')' ':=' msg decls
  | ident ':=' variant decls
  | ident '(' params ')' ':=' variant decls
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
