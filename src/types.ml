
type identifier = string
[@@deriving show { with_path = false; }]

type key = string

type constructor = string

module RecordMap = Map.Make(String)

module VariantMap = Map.Make(String)

type message =
  | Name    of identifier
  | Record  of message RecordMap.t
  | Variant of (message option) VariantMap.t

type parsed_message =
  | PName    of identifier
  | PRecord  of (string * parsed_message) list
  | PVariant of (string * parsed_message option) list
[@@deriving show { with_path = false; }]
