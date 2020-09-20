
let language_version = (0, 0, 2)


module Name : sig
  type t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val from_snake_case : string -> t option
  val to_snake_case : t -> string
  val to_lower_camel_case : t -> string
  val to_upper_camel_case : t -> string
  val bool   : t
  val int    : t
  val string : t
  val list   : t
  val option : t
end  = struct
  include NameScheme

  let make_exn s =
    match from_snake_case s with
    | Some(name) -> name
    | None       -> assert false

  let int    = make_exn "int"
  let bool   = make_exn "bool"
  let string = make_exn "string"
  let list   = make_exn "list"
  let option = make_exn "option"
end

module Variable : sig
  type t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val from_snake_case : string -> t option
  val to_snake_case : t -> string
  val to_lower_camel_case : t -> string
  val to_upper_camel_case : t -> string
end = struct
  include NameScheme
end

module Key : sig
  type t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val from_snake_case : string -> t option
  val to_snake_case : t -> string
  val to_lower_camel_case : t -> string
  val to_upper_camel_case : t -> string
end = struct
  include NameScheme
end

module Constructor : sig
  type t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val from_upper_camel_case : string -> t option
  val to_snake_case : t -> string
  val to_upper_camel_case : t -> string
  val none : t
  val some : t
end = struct
  include NameScheme

  let none =
    match from_upper_camel_case "None" with
    | Some(c) -> c | None -> assert false

  let some =
    match from_upper_camel_case "Some" with
    | Some(c) -> c | None -> assert false
end

module CommonConstant : sig
  val label_field : string
  val arg_field : string
  val key_for_json : Key.t -> string
  val label_for_json : Constructor.t -> string
end  = struct
  let label_field = "_label"
  let arg_field   = "_arg"
  let key_for_json = Key.to_snake_case
  let label_for_json = Constructor.to_upper_camel_case
end

type 'a ranged = Range.t * 'a
[@@deriving show { with_path = false; }]

type constructor = Constructor.t
[@@deriving show { with_path = false; }]

type parsed_name = string
[@@deriving show { with_path = false; }]

type parsed_variable = string
[@@deriving show { with_path = false; }]

type parsed_key = string
[@@deriving show { with_path = false; }]

type parsed_constructor = string
[@@deriving show { with_path = false; }]

type meta_value =
  | VString of string
  | VList   of (meta_value ranged) list

type parsed_dictionary =
  ((string ranged * meta_value ranged) list) ranged

type parsed_message =
  | PVariable of parsed_variable ranged
  | PName     of string ranged * parsed_message list
[@@deriving show { with_path = false; }]

type parsed_variant = (parsed_constructor ranged * parsed_message option) list
[@@deriving show { with_path = false; }]

type parsed_record = (parsed_key ranged * parsed_message) list
[@@deriving show { with_path = false; }]

type parsed_external = (string ranged * parsed_dictionary) list

type built_in =
  | BBool
  | BInt
  | BString
  | BList   of string
  | BOption of string

type parsed_definition_main =
  | PBuiltIn      of built_in
  | PGivenNormal  of parsed_message
  | PGivenVariant of parsed_variant
  | PGivenRecord  of parsed_record
  | PGivenExternal of parsed_external

type parsed_definition = {
  pdef_params : (Range.t * parsed_variable) list;
  pdef_main   : parsed_definition_main;
}

type parsed_declarations = (parsed_name ranged * parsed_definition) list

type meta_spec =
  | MetaOutput          of string ranged * parsed_dictionary
  | MetaLanguageVersion of string ranged

module Dict = Map.Make(String)

type dictionary = ((meta_value ranged) Dict.t) ranged

type top_level = meta_spec list * parsed_declarations

module RecordMap = Map.Make(Key)

let pp_record_map pp ppf rcdmap =
  Format.fprintf ppf "@[<hov2>*R{@ ";
  rcdmap |> RecordMap.iter (fun k v ->
    Format.fprintf ppf "%a ->@ @[<hov2>%a@],@ " Key.pp k pp v
  );
  Format.fprintf ppf "@]}"

module VariantMap = Map.Make(Constructor)

let pp_variant_map pp ppf variant =
  Format.fprintf ppf "@[<hov2>*V(@ ";
  variant |> VariantMap.iter (fun ctor vopt ->
    match vopt with
    | None    -> Format.fprintf ppf "%a,@ " Constructor.pp ctor
    | Some(v) -> Format.fprintf ppf "%a@ ->@ @[%a@],@ " Constructor.pp ctor pp v
  );
  Format.fprintf ppf "@])"

type message =
  | Variable of Variable.t ranged
  | Name     of Name.t ranged * message list
[@@deriving show { with_path = false; }]

type variant = (message option) VariantMap.t
  [@printer (pp_variant_map pp_message)]

type record = message RecordMap.t
  [@printer (pp_record_map pp_message)]

module ExternalMap = Map.Make(String)

type extern = dictionary ExternalMap.t

module DeclMap = Map.Make(Name)

type definition_main =
  | BuiltIn      of built_in
  | GivenNormal  of message
  | GivenVariant of variant
  | GivenRecord  of record
  | GivenExternal of extern

type definition = {
  def_params : (Range.t * Variable.t) list;
  def_main   : definition_main;
}

type declarations = definition DeclMap.t

type version = int * int * int
[@@deriving show { with_path = false; }]

type error =
  | NoLanguageVersionFound
  | MalformedVersionString         of string
  | IncompatibleVersion            of { providing : version; got : version }
  | LexingInvalidCharacter         of { character : char; range : Range.t; }
  | EndOfLineInsideStringLiteral   of { start : Range.t; }
  | EndOfLineInsideComment         of { start : Range.t; }
  | UnknownMeta                    of string
  | MandatoryKeyNotFound           of { range : Range.t; key : string; }
  | ParseErrorDetected             of { range : Range.t; }
  | KeySpecifiedMoreThanOnce       of { range : Range.t; key : string; }
  | MalformedName                  of { raw : string; range : Range.t; }
  | MalformedVariable              of { raw : string; range : Range.t; }
  | MalformedKey                   of { raw : string; range : Range.t; }
  | MalformedConstructor           of { raw : string; range : Range.t; }
  | UnsupportedTarget              of { target : string; }
  | MessageNameDefinedMoreThanOnce of { name : Name.t; range : Range.t; }
  | FieldDefinedMoreThanOnce       of { key : Key.t; range : Range.t; }
  | ConstructorDefinedMoreThanOnce of { constructor : Constructor.t; range : Range.t; }
  | UndefinedMessageName           of { name : Name.t; }
  | UndefinedVariable              of { variable : Variable.t; range : Range.t; }
  | VariableBoundMoreThanOnce      of { variable : Variable.t; }
  | InvalidMessageNameApplication of {
      name           : Name.t;
      expected_arity : int;
      actual_arity   : int;
      range          : Range.t;
    }
  | ExternalDefinedMoreThanOnce    of { key : string; range : Range.t; }
  | NoExternal                     of { format : string; name : Name.t; }
  | NoParameterAllowedForExternal
  | NotAStringValue                of Range.t
  | NotAListValue                  of Range.t
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


let make_variable ((rng, lower) : parsed_variable ranged) : (Variable.t ranged, error) result =
  let open ResultMonad in
  match Variable.from_snake_case lower with
  | None    -> error (MalformedVariable{ raw = lower; range = rng; })
  | Some(x) -> return (rng, x)


let make_key ((rng, lower) : string ranged) : (Key.t ranged, error) result =
  let open ResultMonad in
  match Key.from_snake_case lower with
  | None      -> error (MalformedKey{ raw = lower; range = rng; })
  | Some(key) -> return (rng, key)


let make_constructor ((rng, sctor) : string ranged) : (Constructor.t ranged, error) result =
  let open ResultMonad in
  match Constructor.from_upper_camel_case sctor with
  | None       -> error (MalformedConstructor{ raw = sctor; range = rng; })
  | Some(ctor) -> return (rng, ctor)


let normalize_dictionary ((rngd, pfields) : parsed_dictionary) : (dictionary, error) result =
  let open ResultMonad in
  pfields |> List.fold_left (fun res ((_, k), rv) ->
    res >>= fun dict ->
    if dict |> Dict.mem k then
      error (KeySpecifiedMoreThanOnce{ range = rngd; key = k; })
    else
      return (dict |> Dict.add k rv)
  ) (return Dict.empty) >>= fun dict ->
  return (rngd, dict)


let rec normalize_message (pmsg : parsed_message) : (message, error) result =
  let open ResultMonad in
  match pmsg with
  | PVariable(rlower) ->
      make_variable rlower >>= fun rx ->
      return (Variable(rx))

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
  pvariant |> List.fold_left (fun res (rupper, pmsgsubopt) ->
    res >>= fun variantmap ->
    make_constructor rupper >>= fun (rng, ctor) ->
    if variantmap |> VariantMap.mem ctor then
      error (ConstructorDefinedMoreThanOnce{ constructor = ctor; range = rng; })
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


let normalize_external (pextern : parsed_external) : (extern, error) result =
  let open ResultMonad in
  pextern |> List.fold_left (fun res ((rng, format), pdict) ->
    res >>= fun extmap ->
    if extmap |> ExternalMap.mem format then
      error (ExternalDefinedMoreThanOnce{ key = format; range = rng; })
    else
      normalize_dictionary pdict >>= fun dict ->
      return (extmap |> ExternalMap.add format dict)
  ) (return ExternalMap.empty)


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

        | PGivenExternal(pextern) ->
            normalize_external pextern >>= fun extern ->
            return (GivenExternal(extern))

      end >>= fun defmain ->
      pdef.pdef_params |> List.fold_left (fun res rlower ->
        res >>= fun paramacc ->
        make_variable rlower >>= fun rx ->
        return (Alist.extend paramacc rx)
      ) (return Alist.empty) >>= fun paramacc ->
      let def = { def_params = Alist.to_list paramacc; def_main = defmain } in
      return (declmap |> DeclMap.add name def)
  ) (return DeclMap.empty)


let rec validate_message (find_arity : Name.t -> int option) (is_defined_variable : Variable.t -> bool) (msg : message) : (unit, error) result =
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


let validate_variant (find_arity : Name.t -> int option) (is_defined_variable : Variable.t -> bool) (variant : variant) : (unit, error) result =
  let open ResultMonad in
  VariantMap.fold (fun _ctor argmsgopt res ->
    res >>= fun () ->
    match argmsgopt with
    | None         -> return ()
    | Some(argmsg) -> validate_message find_arity is_defined_variable argmsg
  ) variant (return ())


let validate_record (find_arity : Name.t -> int option) (is_defined_variable : Variable.t -> bool) (record : record) : (unit, error) result =
  let open ResultMonad in
  RecordMap.fold (fun _key vmsg res ->
    res >>= fun () ->
    validate_message find_arity is_defined_variable vmsg
  ) record (return ())



module ParamSet = Set.Make(Variable)


let validate_parameters (params : (Variable.t ranged) list) : (ParamSet.t, error) result =
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

    | GivenExternal(_) ->
        return ()

  ) decls (return ())


let built_in_declarations : parsed_declarations =
  let ( !@ ) t = (Range.dummy t, t) in
  let ( ==> ) (x : string ranged) ((params, info) : (parsed_variable ranged) list * built_in) =
    (x, { pdef_params = params; pdef_main = PBuiltIn(info); })
  in
  [
    !@ "bool"   ==> ([], BBool);
    !@ "int"    ==> ([], BInt);
    !@ "string" ==> ([], BString);
    !@ "option" ==> ([(Range.dummy "option", "v")], BOption("v"));
    !@ "list"   ==> ([(Range.dummy "list", "v")], BList("v"));
  ]


let get_value ((_, dict)) key =
  dict |> Dict.find_opt key


let get_mandatory_value ((rngd, dict) : dictionary) (key : string) : (meta_value ranged, error) result =
  let open ResultMonad in
  match dict |> Dict.find_opt key with
  | None     -> error (MandatoryKeyNotFound{ range = rngd; key = key; })
  | Some(rv) -> return rv


let validate_string_value ((rng, mv) : meta_value ranged) : (string, error) result =
  let open ResultMonad in
  match mv with
  | VString(s) -> return s
  | _          -> error (NotAStringValue(rng))


let get_mandatory_string (rdict : dictionary) (key : string) : (string, error) result =
  let open ResultMonad in
  get_mandatory_value rdict key >>= fun rmv ->
  validate_string_value rmv


let get_list validatef (rdict : dictionary) (key : string) default =
  let open ResultMonad in
  match get_value rdict key with
  | None ->
      return default

  | Some((rng, mv)) ->
      begin
        match mv with
        | VList(rmvs) ->
            rmvs |> List.fold_left (fun res rmv ->
              res >>= fun acc ->
              validatef rmv >>= fun v ->
              return (Alist.extend acc v)
            ) (return Alist.empty) >>= fun acc ->
          return (acc |> Alist.to_list)

        | _ ->
            error (NotAListValue(rng))
      end
