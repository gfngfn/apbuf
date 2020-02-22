
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


let rec normalize_message (pmsg : parsed_message) : (message, error) result =
  let open ResultMonad in
  match pmsg with
  | PName(name) ->
      return (Name(name))

  | PRecord(prcd) ->
      prcd |> List.fold_left (fun res (key, pmsgsub) ->
        res >>= fun rcdmap ->
        if rcdmap |> RecordMap.mem key then
          error (FieldDefinedMoreThanOnce{ key = key; })
        else
          normalize_message pmsgsub >>= fun msgsub ->
          return (rcdmap |> RecordMap.add key msgsub)
      ) (return RecordMap.empty) >>= fun rcdmap ->
      return (Record(rcdmap))

  | PVariant(pvariant) ->
      pvariant |> List.fold_left (fun res (ctor, pmsgsubopt) ->
        res >>= fun variantmap ->
        if variantmap |> VariantMap.mem ctor then
          error (ConstructorDefinedMoreThanOnce{ constructor = ctor; })
        else
          begin
            match pmsgsubopt with
            | None          -> return None
            | Some(pmsgsub) -> normalize_message pmsgsub >>= fun msgsub -> return (Some(msgsub))
          end >>= fun msgsubopt ->
          return (variantmap |> VariantMap.add ctor msgsubopt)
      ) (return VariantMap.empty) >>= fun variantmap ->
      return (Variant(variantmap))


let normalize_declarations (pdecls : parsed_declarations) : (declarations, error) result =
  let open ResultMonad in
  pdecls |> List.fold_left (fun res (name, pmsg) ->
    res >>= fun declmap ->
    if declmap |> DeclMap.mem name then
      error (MessageNameDefinedMoreThanOnce{ name = name; })
    else
      normalize_message pmsg >>= fun msg ->
      return (declmap |> DeclMap.add name msg)
  ) (return DeclMap.empty)
