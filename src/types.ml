
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

type built_in_info = unit
  (* TODO *)

type parsed_definition =
  | PBuiltIn of built_in_info
  | PGiven   of parsed_message

type parsed_declarations = (identifier * parsed_definition) list

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

type definition =
  | BuiltIn of built_in_info
  | Given   of message

type declarations = definition DeclMap.t

type error =
  | MessageNameDefinedMoreThanOnce of { name : identifier; }
  | FieldDefinedMoreThanOnce       of { key : key; }
  | ConstructorDefinedMoreThanOnce of { constructor : constructor; }
  | UndefinedMessageName           of { name : identifier; }
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
  pdecls |> List.fold_left (fun res (name, pdef) ->
    res >>= fun declmap ->
    if declmap |> DeclMap.mem name then
      error (MessageNameDefinedMoreThanOnce{ name = name; })
    else
      match pdef with
      | PBuiltIn(info) ->
          return (declmap |> DeclMap.add name (BuiltIn(info)))

      | PGiven(pmsg) ->
          normalize_message pmsg >>= fun msg ->
          return (declmap |> DeclMap.add name (Given(msg)))
  ) (return DeclMap.empty)


let rec validate_message (is_defined : identifier -> bool) (msg : message) : (unit, error) result =
  let open ResultMonad in
  match msg with
  | Name(name) ->
      if is_defined name then
        return ()
      else
        error (UndefinedMessageName{ name = name; })

  | Record(rcd) ->
      RecordMap.fold (fun _key vmsg res ->
        res >>= fun () ->
        validate_message is_defined vmsg
      ) rcd (return ())

  | Variant(variant) ->
      VariantMap.fold (fun _ctor argmsgopt res ->
        res >>= fun () ->
        match argmsgopt with
        | None         -> return ()
        | Some(argmsg) -> validate_message is_defined argmsg
      ) variant (return ())


let validate_declarations (decls : declarations) : (unit, error) result =
  let is_defined name =
    decls |> DeclMap.mem name
  in
  let open ResultMonad in
  DeclMap.fold (fun _ def res ->
    res >>= fun () ->
    match def with
    | BuiltIn(_) ->
        return ()

    | Given(msg) ->
        validate_message is_defined msg
  ) decls (return ())
