
module Key : sig
  type t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val from_snake_case : string -> t option
  val to_snake_case : t -> string
  val to_lower_camel_case : t -> string
end = struct
  include Name
end

module Constructor : sig
  type t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val from_upper_camel_case : string -> t option
  val to_snake_case : t -> string
  val to_lower_camel_case : t -> string
  val to_upper_camel_case : t -> string
end = struct
  include Name
end

type 'a ranged = Range.t * 'a
[@@deriving show { with_path = false; }]

type variable = string
[@@deriving show { with_path = false; }]

type constructor = string
[@@deriving show { with_path = false; }]

type parsed_message =
  | PVariable of variable ranged
  | PName    of string ranged * parsed_message list
[@@deriving show { with_path = false; }]

type parsed_variant = (constructor ranged * parsed_message option) list
[@@deriving show { with_path = false; }]

type parsed_record = (string ranged * parsed_message) list
[@@deriving show { with_path = false; }]

type built_in =
  | BBool
  | BInt
  | BString
  | BList   of variable
  | BOption of variable

type parsed_definition_main =
  | PBuiltIn      of built_in
  | PGivenNormal  of parsed_message
  | PGivenVariant of parsed_variant
  | PGivenRecord  of parsed_record

type parsed_definition = {
  pdef_params : (Range.t * variable) list;
  pdef_main   : parsed_definition_main;
}

type parsed_declarations = (string ranged * parsed_definition) list

type meta_spec =
  | MetaOutput of string ranged * string ranged

type top_level = meta_spec list * parsed_declarations

module RecordMap = Map.Make(Key)

let pp_record_map pp ppf rcdmap =
  Format.fprintf ppf "@[<hov2>*R{@ ";
  rcdmap |> RecordMap.iter (fun k v ->
    Format.fprintf ppf "%a ->@ @[<hov2>%a@],@ " Key.pp k pp v
  );
  Format.fprintf ppf "@]}"

module VariantMap = Map.Make(String)

let pp_variant_map pp ppf variant =
  Format.fprintf ppf "@[<hov2>*V(@ ";
  variant |> VariantMap.iter (fun k vopt ->
    match vopt with
    | None    -> Format.fprintf ppf "%s,@ " k
    | Some(v) -> Format.fprintf ppf "%s@ ->@ @[%a@],@ " k pp v
  );
  Format.fprintf ppf "@])"

type message =
  | Variable of variable ranged
  | Name     of Name.t ranged * message list
[@@deriving show { with_path = false; }]

type variant = (message option) VariantMap.t
  [@printer (pp_variant_map pp_message)]

type record = message RecordMap.t
  [@printer (pp_record_map pp_message)]

module DeclMap = Map.Make(Name)

type definition_main =
  | BuiltIn      of built_in
  | GivenNormal  of message
  | GivenVariant of variant
  | GivenRecord  of record

type definition = {
  def_params : (Range.t * variable) list;
  def_main   : definition_main;
}

type declarations = definition DeclMap.t

type error =
  | LexingInvalidCharacter         of { character : char; range : Range.t; }
  | EndOfLineInsideStringLiteral   of { start : Range.t; }
  | ParseErrorDetected             of { range : Range.t; }
  | MalformedName                  of { raw : string; range : Range.t; }
  | MalformedKey                   of { raw : string; range : Range.t; }
  | UnsupportedTarget              of { target : string; }
  | MessageNameDefinedMoreThanOnce of { name : Name.t; range : Range.t; }
  | FieldDefinedMoreThanOnce       of { key : Key.t; range : Range.t; }
  | ConstructorDefinedMoreThanOnce of { constructor : constructor; }
  | UndefinedMessageName           of { name : Name.t; }
  | UndefinedVariable              of { variable : variable; range : Range.t; }
  | VariableBoundMoreThanOnce      of { variable : variable; }
  | InvalidMessageNameApplication of {
      name           : Name.t;
      expected_arity : int;
      actual_arity   : int;
      range          : Range.t;
    }
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

module Alist : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
end = struct

  type 'a t = 'a list

  let empty = []

  let extend (acc : 'a t) (v : 'a) = v :: acc

  let append (acc : 'a t) (vs : 'a list) = List.rev_append vs acc

  let to_list = List.rev

end


let make_name ((rng, lower) : string ranged) : (Name.t ranged, error) result =
  let open ResultMonad in
  match Name.from_snake_case lower with
  | None       -> error (MalformedName{ raw = lower; range = rng; })
  | Some(name) -> return (rng, name)


let make_key ((rng, lower) : string ranged) : (Key.t ranged, error) result =
  let open ResultMonad in
  match Key.from_snake_case lower with
  | None      -> error (MalformedKey{ raw = lower; range = rng; })
  | Some(key) -> return (rng, key)


let rec normalize_message (pmsg : parsed_message) : (message, error) result =
  let open ResultMonad in
  match pmsg with
  | PVariable(x) ->
      return (Variable(x))

  | PName(rlower, pargs) ->
      make_name rlower >>= fun rname ->
      pargs |> List.fold_left (fun res argpmsg ->
        res >>= fun acc ->
        normalize_message argpmsg >>= fun argmsg ->
        return (Alist.extend acc argmsg)
      ) (return Alist.empty) >>= fun acc ->
      return (Name(rname, acc |> Alist.to_list))


let normalize_variant (pvariant : parsed_variant) : (variant, error) result =
  let open ResultMonad in
  pvariant |> List.fold_left (fun res ((_, ctor), pmsgsubopt) ->
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
  return variantmap


let normalize_record (prcd : parsed_record) : (record, error) result =
  let open ResultMonad in
  prcd |> List.fold_left (fun res (rlower, pmsgsub) ->
    res >>= fun rcdmap ->
    make_key rlower >>= fun (rng, key) ->
    if rcdmap |> RecordMap.mem key then
      error (FieldDefinedMoreThanOnce{ key = key; range = rng; })
    else
      normalize_message pmsgsub >>= fun msgsub ->
    return (rcdmap |> RecordMap.add key msgsub)
  ) (return RecordMap.empty) >>= fun rcdmap ->
  return rcdmap



let normalize_declarations (pdecls : parsed_declarations) : (declarations, error) result =
  let open ResultMonad in
  pdecls |> List.fold_left (fun res (rlower, pdef) ->
    res >>= fun declmap ->
    make_name rlower >>= fun (rng, name) ->
    if declmap |> DeclMap.mem name then
      error (MessageNameDefinedMoreThanOnce{ name = name; range = rng; })
    else
      begin
        match pdef.pdef_main with
        | PBuiltIn(info) ->
            return (BuiltIn(info))

        | PGivenNormal(pmsg) ->
            normalize_message pmsg >>= fun msg ->
            return (GivenNormal(msg))

        | PGivenVariant(pvariant) ->
            normalize_variant pvariant >>= fun variant ->
            return (GivenVariant(variant))

        | PGivenRecord(precord) ->
            normalize_record precord >>= fun record ->
            return (GivenRecord(record))
      end >>= fun defmain ->
      let def = { def_params = pdef.pdef_params; def_main = defmain } in
      return (declmap |> DeclMap.add name def)
  ) (return DeclMap.empty)


let rec validate_message (find_arity : Name.t -> int option) (is_defined_variable : variable -> bool) (msg : message) : (unit, error) result =
  let iter = validate_message find_arity is_defined_variable in
  let open ResultMonad in
  match msg with
  | Variable((rng, x)) ->
      if is_defined_variable x then
        return ()
      else
        error (UndefinedVariable{ variable = x; range = rng; })

  | Name((rng, name), args) ->
      begin
        match find_arity name with
        | Some(n_expected) ->
            let n_actual = List.length args in
            if n_actual == n_expected then
              args |> List.fold_left (fun res argmsg ->
                res >>= fun () ->
                iter argmsg
              ) (return ())
            else
              error (InvalidMessageNameApplication{
                name           = name;
                expected_arity = n_expected;
                actual_arity   = n_actual;
                range          = rng;
              })

        | None ->
            error (UndefinedMessageName{ name = name; })
      end


let validate_variant (find_arity : Name.t -> int option) (is_defined_variable : variable -> bool) (variant : variant) : (unit, error) result =
  let open ResultMonad in
  VariantMap.fold (fun _ctor argmsgopt res ->
    res >>= fun () ->
    match argmsgopt with
    | None         -> return ()
    | Some(argmsg) -> validate_message find_arity is_defined_variable argmsg
  ) variant (return ())


let validate_record (find_arity : Name.t -> int option) (is_defined_variable : variable -> bool) (record : record) : (unit, error) result =
  let open ResultMonad in
  RecordMap.fold (fun _key vmsg res ->
    res >>= fun () ->
    validate_message find_arity is_defined_variable vmsg
  ) record (return ())



module ParamSet = Set.Make(String)


let validate_parameters (params : (Range.t * variable) list) : (ParamSet.t, error) result =
  let open ResultMonad in
  params |> List.fold_left (fun res (_, x) ->
    res >>= fun set ->
    if set |> ParamSet.mem x then
      error (VariableBoundMoreThanOnce{ variable = x; })
    else
      return (set |> ParamSet.add x)
  ) (return ParamSet.empty)


let validate_declarations (decls : declarations) : (unit, error) result =
  let find_arity name =
    match decls |> DeclMap.find_opt name with
    | None      -> None
    | Some(def) -> Some(List.length def.def_params)
  in
  let open ResultMonad in
  DeclMap.fold (fun _ def res ->
    res >>= fun () ->
    validate_parameters def.def_params >>= fun set ->
    let is_defined_variable v = set |> ParamSet.mem v in
    match def.def_main with
    | BuiltIn(_) ->
        return ()

    | GivenNormal(msg) ->
        validate_message find_arity is_defined_variable msg

    | GivenVariant(variant) ->
        validate_variant find_arity is_defined_variable variant

    | GivenRecord(record) ->
        validate_record find_arity is_defined_variable record

  ) decls (return ())


let built_in_declarations : parsed_declarations =
  let ( !@ ) t = (Range.dummy t, t) in
  let ( ==> ) (x : string ranged) ((params, info) : (variable ranged) list * built_in) =
    (x, { pdef_params = params; pdef_main = PBuiltIn(info); })
  in
  [
    !@ "bool"   ==> ([], BBool);
    !@ "int"    ==> ([], BInt);
    !@ "string" ==> ([], BString);
    !@ "option" ==> ([(Range.dummy "option", "v")], BOption("v"));
    !@ "list"   ==> ([(Range.dummy "list", "v")], BList("v"));
  ]
