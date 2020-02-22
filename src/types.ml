
type identifier = string
[@@deriving show { with_path = false; }]

type variable = string
[@@deriving show { with_path = false; }]

type key = string
[@@deriving show { with_path = false; }]

type constructor = string
[@@deriving show { with_path = false; }]

type parsed_message =
  | PVariable of variable
  | PName    of identifier
  | PRecord  of (string * parsed_message) list
  | PVariant of (string * parsed_message option) list
[@@deriving show { with_path = false; }]

type built_in_info = unit
  (* TODO *)

type parameter = string

type parsed_definition_main =
  | PBuiltIn of built_in_info
  | PGiven   of parsed_message

type parsed_definition = {
  pdef_params : parameter list;
  pdef_main   : parsed_definition_main;
}

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
  | Variable of variable
  | Name    of identifier
  | Record  of message RecordMap.t
      [@printer (pp_record_map pp_message)]
  | Variant of (message option) VariantMap.t
      [@printer (pp_variant_map pp_message)]
[@@deriving show { with_path = false; }]

module DeclMap = Map.Make(String)

type definition_main =
  | BuiltIn of built_in_info
  | Given   of message

type definition = {
  def_params : parameter list;
  def_main   : definition_main;
}

type declarations = definition DeclMap.t

type error =
  | MessageNameDefinedMoreThanOnce of { name : identifier; }
  | FieldDefinedMoreThanOnce       of { key : key; }
  | ConstructorDefinedMoreThanOnce of { constructor : constructor; }
  | UndefinedMessageName           of { name : identifier; }
  | UndefinedVariable              of { variable : variable; }
  | VariableBoundMoreThanOnce      of { variable : variable; }
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
  | PVariable(x) ->
      return (Variable(x))

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
      begin
        match pdef.pdef_main with
        | PBuiltIn(info) ->
            return (BuiltIn(info))

        | PGiven(pmsg) ->
            normalize_message pmsg >>= fun msg ->
            return (Given(msg))
      end >>= fun defmain ->
      let def = { def_params = pdef.pdef_params; def_main = defmain } in
      return (declmap |> DeclMap.add name def)
  ) (return DeclMap.empty)


let rec validate_message (is_defined_identifier : identifier -> bool) (is_defined_variable : variable -> bool) (msg : message) : (unit, error) result =
  let iter = validate_message is_defined_identifier is_defined_variable in
  let open ResultMonad in
  match msg with
  | Variable(x) ->
      if is_defined_variable x then
        return ()
      else
        error (UndefinedVariable{ variable = x; })

  | Name(name) ->
      if is_defined_identifier name then
        return ()
      else
        error (UndefinedMessageName{ name = name; })

  | Record(rcd) ->
      RecordMap.fold (fun _key vmsg res ->
        res >>= fun () ->
        iter vmsg
      ) rcd (return ())

  | Variant(variant) ->
      VariantMap.fold (fun _ctor argmsgopt res ->
        res >>= fun () ->
        match argmsgopt with
        | None         -> return ()
        | Some(argmsg) -> iter argmsg
      ) variant (return ())


module ParamSet = Set.Make(String)


let validate_parameters (params : parameter list) : (ParamSet.t, error) result =
  let open ResultMonad in
  params |> List.fold_left (fun res x ->
    res >>= fun set ->
    if set |> ParamSet.mem x then
      error (VariableBoundMoreThanOnce{ variable = x; })
    else
      return (set |> ParamSet.add x)
  ) (return ParamSet.empty)


let validate_declarations (decls : declarations) : (unit, error) result =
  let is_defined_identifier name =
    decls |> DeclMap.mem name
  in
  let open ResultMonad in
  DeclMap.fold (fun _ def res ->
    res >>= fun () ->
    match def.def_main with
    | BuiltIn(_) ->
        return ()

    | Given(msg) ->
        validate_parameters def.def_params >>= fun set ->
        validate_message is_defined_identifier (fun v -> set |> ParamSet.mem v) msg
  ) decls (return ())


let built_in_declarations : parsed_declarations =
  let ( ==> ) (x : identifier) ((params, info) : parameter list * built_in_info) =
    (x, { pdef_params = params; pdef_main = PBuiltIn(info); })
  in
  [
    "int"    ==> ([], ());
    "string" ==> ([], ());
    "option" ==> (["v"], ());
    "list"   ==> (["v"], ());
  ]
