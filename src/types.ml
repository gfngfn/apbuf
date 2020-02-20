
type identifier = string
[@@deriving show { with_path = false; }]

type key = string
[@@deriving show { with_path = false; }]

type constructor = string
[@@deriving show { with_path = false; }]

type parsed_message =
  | PName    of identifier
  | PRecord  of (string * parsed_message) list
  | PVariant of (string * parsed_message option) list
[@@deriving show { with_path = false; }]

type parsed_declarations = (identifier * parsed_message) list

module RecordMap = Map.Make(String)

let pp_record_map pp ppf rcdmap =
  Format.fprintf ppf "Record{@[@ ";
  rcdmap |> RecordMap.iter (fun k v ->
    Format.fprintf ppf "%s@ ->@ @[%a@],@ " k pp v
  );
  Format.fprintf ppf "@ @]}"

module VariantMap = Map.Make(String)

let pp_variant_map pp ppf variantmap =
  Format.fprintf ppf "Variant(@[@ ";
  variantmap |> VariantMap.iter (fun k vopt ->
    match vopt with
    | None    -> Format.fprintf ppf "%s,@ " k
    | Some(v) -> Format.fprintf ppf "%s@ ->@ @[%a@],@ " k pp v
  );
  Format.fprintf ppf "@ @])"

type message =
  | Name    of identifier
  | Record  of message RecordMap.t
      [@printer (pp_record_map pp_message)]
  | Variant of (message option) VariantMap.t
      [@printer (pp_variant_map pp_message)]
[@@deriving show { with_path = false; }]

module DeclMap = Map.Make(String)

type declarations = message DeclMap.t

type error =
  | MessageNameDefinedMoreThanOnce of { name : identifier; }
  | FieldDefinedMoreThanOnce       of { key : key; }
  | ConstructorDefinedMoreThanOnce of { constructor : constructor; }
[@@deriving show { with_path = false; }]

module ResultMonad : sig
  val ( >>= ) : ('a, error) result -> ('a -> ('b, error) result) -> ('b, error) result
  val error : error -> ('a, error) result
  val return : 'a -> ('a, error) result
end = struct

  let ( >>= ) res f =
    match res with
    | Ok(v)           -> f v
    | Error(_) as err -> err

  let error e =
    Error(e)

  let return v =
    Ok(v)

end
