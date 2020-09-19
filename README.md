
# APBuf (Algebraic Protocol Buffers)

## Summary

*APBuf* (*Algebraic Protocol Buffers*) is a compiler that generates decoders/encoders from a single configuration file that specifies the data format. It is characterized by the native support of “labeled direct sum” (which corresponds to ADTs (= algebraic data types) in Haskell, OCaml, Elm, etc.).

In exchange for its clean representation, APBuf at least currently does not pursue high computational performance.

* Currently supported target formats:
  - JSON
* Currently supported target languages:
  - Elm
  - Scala


## How to install

Under the condition that `dune` and `make` are installed, invoke:

```console
$ make install
```

and then the executable file `apbuf` will be installed.


## Example

The following configuration file:

```
@language_version "0.0.1"
@output "elm": {
  dir    = "./gen/elm/src",
  module = "Bar",
}
@output "scala": {
  dir     = "./gen/play-scala-seed/app/assets/apbuf",
  package = "apbufgen",
  object  = "Bar",
}

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
module Bar exposing (..)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)

type Geometry
  = Rectangle (RectangleInfo Int)
  | Circle (CircleInfo Int Rational)
decodeGeometry : Decoder Geometry
encodeGeometry : Geometry -> Value

type alias RectangleInfo a = { lowerRight : Position a, upperLeft : Position a }
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

and also generates an implementation of the following API in Scala:

```scala
package apbufgen

import play.api.libs.json._

object Bar {
  sealed trait Geometry
  case class Rectangle(arg: RectangleInfo[Int]) extends Geometry
  case class Circle(arg: CircleInfo[Int, Rational]) extends Geometry
  def geometryReads(): Reads[Geometry]
  def geometryWrites(): Writes[Geometry]

  case class RectangleInfo[Num](lowerRight: Position[Num], upperLeft: Position[Num])
  def rectangleInfoReads[Num](Reads[Num]): Reads[RectangleInfo[Num]]
  def rectangleInfoWrites[Num](Writes[Num]): Writes[RectangleInfo[Num]]

  case class CircleInfo[Cnum, Rnum](center: Position[Cnum], radius: Rnum)
  def circleInfoReads[Cnum, Rnum](Reads[Cnum], Reads[Rnum]): Reads[CircleInfo[Cnum, Rnum]]
  def circleInfoWrites[Cnum, Rnum](Writes[Cnum], Writes[Rnum]): Writes[CircleInfo[Cnum, Rnum]]

  case class Position[Num](x: Num, y: Num)
  def positionReads[Num](Reads[Num]): Reads[Position[Num]]
  def positionWrites[Num](Writes[Num]): Writes[Position[Num]]

  case class Rational(denominator: Int, numerator: Int)
  def rationalReads(): Reads[Rational]
  def rationalWrites(): Writes[Rational]
}
```


## Syntax of configuration files

```
stringlit ::= (a string literal enclosed by double quotation marks)
ident ::= (a lowercased identifier)
$x ::= (a lowercased identifier with a preceding dollar sign)

toplevel ::=
  | metas decls

metas ::=
  | meta metas
  | (empty)

meta ::=
  | '@language_version' stringlit
  | '@output' stringlit ':' dict

dict ::=
  | '{' fields '}'

fields ::=
  | ident '=' metaval ',' fields
  | ident '=' metaval
  | (empty)

metaval ::=
  | stringlit
  | '[' metavals ']'

metavals ::=
  | metaval ',' metavals
  | metaval
  | (empty)

// a list of declarations
decls ::=
  | decl decls
  | (empty)

// a declaration
decl ::=
  | binder ':=' msg
  | binder ':=' '{' record '}'
  | binder ':=' variant
  | ident ':=' externals

binder ::=
  | ident
  | ident '(' params ')'

params ::=
  | $x ',' params
  | $x ','
  | $x

msg ::=
  | $x
  | ident
  | ident '(' args ')'

args ::=
  | msg ',' args
  | msg ','
  | msg

record ::=
  | ident ':' msg ',' record
  | ident ':' msg
  | (empty)

variant ::=
  | '|' ctor ':' msg variantsub
  | ctor ':' msg variantsub

variantsub ::=
  | '|' ctor ':' msg variantsub
  | (empty)

externals ::=
  | external externals
  | (empty)

external ::=
  | '@external' stringlit ':' dict
```


## Features we want to support in the future

* Automated check for asserting backward compatibility of the update of data formats
