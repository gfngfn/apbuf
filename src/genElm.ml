
open Types

module Constant : GenMlScheme.CONSTANT = struct

  let fun_decode_field    = "Json.Decode.field"
  let fun_decode_and_then = ("Json.Decode.andThen", true)
  let fun_decode_map      = "Json.Decode.map"
  let fun_decode_succeed  = "Json.Decode.succeed"
  let fun_decode_fail     = "Json.Decode.fail"
  let fun_decode_string   = "Json.Decode.string"

  let fun_encode_object   = "Json.Encode.object"
  let fun_encode_string   = "Json.Encode.string"

  let type_decoder = "Json.Decode.Decoder"
  let type_value   = "Json.Encode.Value"


  let global_decoder name =
    "decode" ^ (Name.to_upper_camel_case name)


  let global_encoder name =
    "encode" ^ (Name.to_upper_camel_case name)


  let local_for_key key =
    "localKey" ^ (Key.to_upper_camel_case key)


  let local_for_parameter x =
    "localParam" ^ (Variable.to_upper_camel_case x)


  module ReservedWordSet = Set.Make(String)


  let reserved_words =
    ReservedWordSet.of_list [
      "type"; "alias"; "port";
      "if"; "then"; "else";
      "case"; "of";
      "let"; "in";
      "infix"; "left"; "right"; "non";
      "module"; "import"; "exposing"; "as";
      "effect"; "where"; "command"; "subscription";
    ]
      (* https://github.com/elm/compiler/blob/14af98cde81146abd5950be3d7ab0133e55898ef/compiler/src/Parse/Keyword.hs *)


  let key key =
    let s = Key.to_lower_camel_case key in
    if reserved_words |> ReservedWordSet.mem s then
      s ^ "_"
    else
      s


  let constructor =
    Constructor.to_upper_camel_case


  let builtin_type_candidates =
    let ( ==> ) x y = (x, y) in
    [
      Name.bool   ==> "Bool";
      Name.int    ==> "Int";
      Name.string ==> "String";
      Name.list   ==> "List";
      Name.option ==> "Maybe";
    ]


  let type_identifier name =
    match
      builtin_type_candidates |> List.find_map (fun (namex, s) ->
        if Name.compare namex name = 0 then Some(s) else None
      )
    with
    | Some(s) -> s
    | None    -> Name.to_upper_camel_case name


  let type_parameter x =
    "ty" ^ (Variable.to_upper_camel_case x)

end


module Output : sig

  type identifier
  val global_decoder : Name.t -> identifier
  val global_encoder : Name.t -> identifier
  val local_for_key : Key.t -> identifier
  val local_for_parameter : Variable.t -> identifier
  type tree
  val identifier : identifier -> tree
  val application : identifier -> tree list -> tree
  val general_application : tree -> tree -> tree
  val abstraction : identifier -> tree -> tree
  val string_literal : string -> tree
  val constructor : Types.constructor -> tree
  val tuple : tree list -> tree
  val list : tree list -> tree
  val record : tree RecordMap.t -> tree
  val record_field_access : tree -> Key.t -> tree
  val branching : tree VariantMap.t -> tree
  val decode_field_access : Key.t -> tree -> tree
  val access_argument : tree -> tree
  val and_then : tree -> tree -> tree
  val map : tree -> tree -> tree
  val succeed : tree -> tree
  val encode_record : tree -> tree
  val encode_branching : (tree option) VariantMap.t -> tree
  type type_identifier
  val type_identifier : Name.t -> type_identifier
  type type_parameter
  val type_parameter : Variable.t -> type_parameter
  type typ
  val type_name : type_identifier -> typ list -> typ
  val type_variable : type_parameter -> typ
  val record_type : (Key.t * typ) list -> typ
  val function_type : typ list -> typ -> typ
  val decoder_type : typ -> typ
  val encoder_type : typ -> typ
  type declaration
  val stringify_declaration : declaration -> string
  val define_value : identifier -> type_parameter list -> (identifier * typ) list -> typ -> tree -> declaration
  val define_value_by_text : identifier -> typ -> string -> declaration
  val built_in_decoder : built_in -> declaration
  val built_in_encoder : built_in -> declaration
  val define_type_alias : type_identifier -> type_parameter list -> typ -> declaration
  val define_data_type : type_identifier -> type_parameter list -> (Types.constructor * typ option) list -> declaration
  val define_type_by_text : type_identifier -> string -> declaration

end = struct

  include GenMlScheme.Make(Constant)


  let rec stringify_pattern (pat : pattern) : string =
    match pat with
    | StringPattern(s) ->
        Format.sprintf "\"%s\"" s

    | ConstructorPattern(sctor, patopt) ->
        begin
          match patopt with
          | None         -> sctor
          | Some(patsub) -> Format.sprintf "(%s %s)" sctor (stringify_pattern patsub)
        end

    | IdentifierPattern(Var(s)) ->
        s


  let rec stringify_tree (indent : int) (otree : tree) : string =
    match otree with
    | Identifier(Var(s)) ->
        s

    | Application{ applied = applied; arguments = args; } ->
        let s = stringify_tree indent applied in
        begin
          match args with
          | [] ->
              s

          | _ :: _ ->
              let sargs = args |> List.map (stringify_tree (indent + 1)) in
              Format.sprintf "(%s %s)" s (String.concat " " sargs)
        end

    | Abstract{ variable = Var(s); body = body; } ->
        Format.sprintf "(\\%s -> %s)" s (stringify_tree (indent + 1) body)

    | StringLiteral(s) ->
        Format.sprintf "\"%s\"" s

    | Constructor(ctor) ->
        ctor

    | Tuple(otrees) ->
        let ss = otrees |> List.map (stringify_tree (indent + 1)) in
        Format.sprintf "( %s )" (String.concat ", " ss)

    | List(otrees) ->
        let ss = otrees |> List.map (stringify_tree (indent + 1)) in
        Format.sprintf "[ %s ]" (String.concat ", " ss)

    | Record(orcd) ->
        let ss =
          RecordMap.fold (fun key otree acc ->
            let s = Format.sprintf "%s = %s" (Constant.key key) (stringify_tree (indent + 1) otree) in
            Alist.extend acc s
          ) orcd Alist.empty |> Alist.to_list
        in
        Format.sprintf "{ %s }" (String.concat ", " ss)

    | FieldAccess{ record = otree_record; key = key; } ->
        let skey = Constant.key key in
        let s = stringify_tree indent otree_record in
        Format.sprintf "%s.%s" s skey

    | Case{ subject = otree_subject; branches = branches } ->
        let sindent = "\n" ^ String.make ((indent + 1) * 2) ' ' in
        let ss =
          branches |> List.map (fun (pat, otree) ->
            Format.sprintf "%s%s -> %s" sindent (stringify_pattern pat) (stringify_tree (indent + 2) otree)
          )
        in
        Format.sprintf "(case %s of %s)" (stringify_tree (indent + 1) otree_subject) (String.concat "" ss)


  let rec stringify_type (ty : typ) : string =
    match ty with
    | TypeName(TypeIdentifier(s), tyargs) ->
        begin
          match tyargs with
          | [] ->
              s

          | _ :: _ ->
              let ss = tyargs |> List.map (fun ty -> " " ^ stringify_type ty) in
              Format.sprintf "(%s%s)" s (String.concat "" ss)
        end

    | TypeVariable(TypeParameter(a)) ->
        a

    | FuncType(tys1, ty2) ->
        let sdom = tys1 |> List.map stringify_type |> List.map (fun s -> s ^ " -> ") |> String.concat "" in
        Format.sprintf "(%s%s)"
          sdom
          (stringify_type ty2)

    | RecordType(tyrcd) ->
        let sr =
          tyrcd |> List.map (fun (key, ty) ->
            Format.sprintf "%s : %s" (Constant.key key) (stringify_type ty)
          ) |> String.concat ", "
        in
        Format.sprintf "{ %s }" sr


  let stringify_declaration (odecl : declaration) : string =
    match odecl with
    | DefVal{
        val_name    = Var(s);
        parameters  = oparams;
        return_type = tyret;
        body        = otree;
      } ->
        let typ =
          let tydoms = oparams |> List.map (fun (_, ty) -> ty) in
          function_type tydoms tyret
        in
        Format.sprintf "%s : %s\n%s%s = %s"
          s
          (stringify_type typ)
          s
          (String.concat "" (oparams |> List.map (fun (Var(s), _) -> " " ^ s)))
          (stringify_tree 1 otree)

    | DefValByText{
        val_name = Var(s);
        typ      = typ;
        text     = text;
      } ->
        Printf.sprintf "%s : %s\n%s = %s"
          s
          (stringify_type typ)
          s
          text

    | DefTypeAlias{
        type_name  = TypeIdentifier(s);
        parameters = otyparams;
        body       = ty;
      } ->
        Format.sprintf "type alias %s%s = %s"
          s
          (String.concat "" (otyparams |> List.map (fun (TypeParameter(a)) -> " " ^ a)))
          (stringify_type ty)

    | DefDataType{
        type_name  = TypeIdentifier(s);
        parameters = otyparams;
        patterns   = defs;
      } ->
        let ss =
          defs |> List.mapi (fun index (ctor, tyopt) ->
            let sarg =
              match tyopt with
              | None     -> ""
              | Some(ty) -> " "  ^ (stringify_type ty)
            in
            let sep = if index == 0 then "=" else "|" in
            Format.sprintf "  %s %s%s" sep (Constant.constructor ctor) sarg
          )
        in
        Format.sprintf "type %s%s\n%s"
          s
          (String.concat "" (otyparams |> List.map (fun (TypeParameter(a)) -> " " ^ a)))
          (String.concat "\n" ss)

    | DefTypeByText{
        type_name = TypeIdentifier(s);
        text      = text;
      } ->
        Printf.sprintf "type alias %s = %s"
          s
          text


  let built_in_decoder (builtin : built_in) : declaration =
    let dec = decoder_type in
    let ( !$ ) tp = TypeVariable(tp) in
    match builtin with
    | BBool ->
        DefVal{
          val_name    = global_decoder Name.bool;
          universal   = [];
          parameters  = [];
          return_type = dec (base Name.bool);
          body        = Identifier(Var("Json.Decode.bool"));
        }

    | BInt ->
        DefVal{
          val_name    = global_decoder Name.int;
          universal   = [];
          parameters  = [];
          return_type = dec (base Name.int);
          body        = Identifier(Var("Json.Decode.int"));
        }

    | BString ->
        DefVal{
          val_name    = global_decoder Name.string;
          universal   = [];
          parameters  = [];
          return_type = dec (base Name.string);
          body        = Identifier(Var("Json.Decode.string"));
        }

    | BList(_) ->
        let typaram = TypeParameter("a") in
        DefVal{
          val_name    = global_decoder Name.list;
          universal   = [ typaram ];
          parameters  = [];
          return_type = FuncType([ dec (!$ typaram) ], dec (TypeName(type_identifier Name.list, [!$ typaram])));
          body        = Identifier(Var("Json.Decode.list"));
        }

    | BOption(_) ->
        let ovar = Var("x") in
        let omap =
          VariantMap.empty
            |> VariantMap.add Constructor.none (succeed (Constructor("Nothing")))
            |> VariantMap.add Constructor.some (access_argument (map (Constructor("Just")) (Identifier(ovar))))
        in
        let typaram = TypeParameter("a") in
        DefVal{
          val_name    = global_decoder Name.option;
          universal   = [ typaram ];
          parameters  = [ (ovar, dec (!$ typaram)) ];
          return_type = dec (TypeName(type_identifier Name.option, [!$ typaram]));
          body        = branching omap;
        }


  let built_in_encoder (builtin : built_in) : declaration =
    let enc = encoder_type in
    let ( !$ ) tp = TypeVariable(tp) in
    match builtin with
    | BBool ->
        DefVal{
          val_name    = global_encoder Name.bool;
          universal   = [];
          parameters  = [];
          return_type = enc (base Name.bool);
          body        = Identifier(Var("Json.Encode.bool"));
        }

    | BInt ->
        DefVal{
          val_name    = global_encoder Name.int;
          universal   = [];
          parameters  = [];
          return_type = enc (base Name.int);
          body        = Identifier(Var("Json.Encode.int"));
        }

    | BString ->
        DefVal{
          val_name    = global_encoder Name.string;
          universal   = [];
          parameters  = [];
          return_type = enc (base Name.string);
          body        = Identifier(Var("Json.Encode.string"));
        }

    | BList(_) ->
        let typaram = TypeParameter("a") in
        DefVal{
          val_name    = global_encoder Name.list;
          universal   = [ typaram ];
          parameters  = [];
          return_type = FuncType([ enc (!$ typaram) ], enc (TypeName(type_identifier Name.list, [!$ typaram])));
          body        = Identifier(Var("Json.Encode.list"));
        }

    | BOption(_) ->
        let ovar_param = Var("x") in
        let ovar_toenc = Var("opt") in
        let ovar_toencsub = Var("sub") in
        let typaram = TypeParameter("a") in
        let body =
          abstraction ovar_toenc
            (Case{
              subject  = Identifier(ovar_toenc);
              branches = [
                (ConstructorPattern("Nothing", None),
                   encoded_none);
                (ConstructorPattern("Just", Some(IdentifierPattern(ovar_toencsub))),
                   encoded_some (general_application (Identifier(ovar_param)) (Identifier(ovar_toencsub))));
              ];
            })
        in
        DefVal{
          val_name    = global_encoder Name.option;
          universal   = [ typaram ];
          parameters  = [ (ovar_param, enc (!$ typaram)) ];
          return_type = enc (TypeName(type_identifier Name.option, [!$ typaram]));
          body        = body;
        }

end


let decoder_of_variable (x : Variable.t) : Output.tree =
  Output.identifier (Output.local_for_parameter x)


let rec decoder_of_name (name : Name.t) (args : message list) : Output.tree =
  let otrees = args |> List.map generate_message_decoder in
  Output.application (Output.global_decoder name) otrees


and decoder_of_record (rcd : message RecordMap.t) : Output.tree =
  let otree_ans =
    let orcd =
      rcd |> RecordMap.mapi (fun key _ ->
        Output.identifier (Output.local_for_key key)
      )
    in
    Output.succeed (Output.record orcd)
  in
  let acc =
    RecordMap.fold (fun key vmsg acc ->
      let otree_decv = generate_message_decoder vmsg in
      let otree_accessk = Output.decode_field_access key otree_decv in
      Alist.extend acc (key, otree_accessk)
    ) rcd Alist.empty
  in
  List.fold_right (fun (key, otree_accessk) otree_acc ->
    Output.and_then (Output.abstraction (Output.local_for_key key) otree_acc) otree_accessk
  ) (acc |> Alist.to_list) otree_ans


and decoder_of_variant (variant : (message option) VariantMap.t) : Output.tree =
  let ocases =
    variant |> VariantMap.mapi (fun ctor argmsgopt ->
      match argmsgopt with
      | None ->
          Output.succeed (Output.constructor ctor)

      | Some(argmsg) ->
          let otree_decarg = generate_message_decoder argmsg in
          Output.access_argument (Output.map (Output.constructor ctor) otree_decarg)
    )
  in
  Output.branching ocases


and generate_message_decoder (msg : message) : Output.tree =
  match msg with
  | Variable((_, x))      -> decoder_of_variable x
  | Name((_, name), args) -> decoder_of_name name args


let rec generate_message_type (msg : message) : Output.typ =
  match msg with
  | Variable((_, x)) ->
      Output.type_variable (Output.type_parameter x)

  | Name((_, name), args) ->
      let tys = args |> List.map generate_message_type in
      Output.type_name (Output.type_identifier name) tys


let generate_record_type (record : record) : Output.typ =
  let tyrcd =
    RecordMap.fold (fun key vmsg acc ->
      let ty = generate_message_type vmsg in
      Alist.extend acc (key, ty)
    ) record Alist.empty |> Alist.to_list
  in
  Output.record_type tyrcd


let make_decoder_parameters (params : (Variable.t ranged) list) =
  params |> List.map (fun (_, x) ->
    let ovar = Output.local_for_parameter x in
    let ty = Output.decoder_type (Output.type_variable (Output.type_parameter x)) in
    (ovar, ty)
  )


let make_encoder_parameters (params : (Variable.t ranged) list) =
  params |> List.map (fun (_, x) ->
    let ovar = Output.local_for_parameter x in
    let ty = Output.encoder_type (Output.type_variable (Output.type_parameter x)) in
    (ovar, ty)
  )


let encoder_of_variable (x : Variable.t) : Output.tree =
  Output.identifier (Output.local_for_parameter x)


let rec encoder_of_name (name : Name.t) (args : message list) : Output.tree =
  let otrees = args |> List.map generate_message_encoder in
  Output.application (Output.global_encoder name) otrees


and encoder_of_record (rcd : message RecordMap.t) : Output.tree =
  let x_record =
    match Variable.from_snake_case "temp" with
    | Some(s) -> Output.local_for_parameter s
    | None    -> assert false
        (* TODO: make less ad-hoc *)
  in
  let otrees =
    RecordMap.fold (fun key vmsg acc ->
      let otree_encoder = generate_message_encoder vmsg in
      let otree_encoded =
        Output.general_application otree_encoder (Output.record_field_access (Output.identifier(x_record)) key)
      in
      let otree_pair = Output.tuple [ Output.string_literal (CommonConstant.key_for_json key); otree_encoded ] in
      Alist.extend acc otree_pair
    ) rcd Alist.empty |> Alist.to_list
  in
  Output.abstraction x_record (Output.encode_record (Output.list otrees))


and encoder_of_variant (variant : (message option) VariantMap.t) : Output.tree =
  let encmap =
    variant |> VariantMap.map (fun msgopt ->
      match msgopt with
      | None      -> None
      | Some(msg) -> Some(generate_message_encoder msg)
    )
  in
  Output.encode_branching encmap


(** Given a message [msg] of type [T], [generate_message_encoder msg] generates
    the code representation of encoder of type [T].
    The return value is an representation of Elm code of type [T -> Value].
  *)
and generate_message_encoder (msg : message) : Output.tree =
  match msg with
  | Variable((_, x))      -> encoder_of_variable x
  | Name((_, name), args) -> encoder_of_name name args
(*
  | Record(rcd)           -> encoder_of_record rcd
*)

let generate (module_name : string) (imports : string list) (decls : declarations) : (string, error) result =
  let open ResultMonad in
  DeclMap.fold (fun name def res ->
    res >>= fun acc ->
    let ovar_decoder = Output.global_decoder name in
    let ovar_encoder = Output.global_encoder name in
    match def.def_main with
    | BuiltIn(builtin) ->
        let odecl_decoder = Output.built_in_decoder builtin in
        let odecl_encoder = Output.built_in_encoder builtin in
        let acc = Alist.append acc [ odecl_decoder; odecl_encoder; ] in
        return acc

    | GivenNormal(msg) ->
        let otyname = Output.type_identifier name in
        let otyparams = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
        let odecl_type =
          Output.define_type_alias otyname otyparams (generate_message_type msg)
        in
        let tyaliasmsg =
          let tyargs = otyparams |> List.map Output.type_variable in
          Output.type_name otyname tyargs
        in
        let odecl_decoder =
          let oparams = make_decoder_parameters def.def_params in
          let tyret = Output.decoder_type tyaliasmsg in
          let otree_decoder = generate_message_decoder msg in
          Output.define_value ovar_decoder otyparams oparams tyret otree_decoder
        in
        let odecl_encoder =
          let oparams = make_encoder_parameters def.def_params in
          let tyret = Output.encoder_type tyaliasmsg in
          let otree_encoder = generate_message_encoder msg in
          Output.define_value ovar_encoder otyparams oparams tyret otree_encoder
        in
        let acc = Alist.append acc [ odecl_type; odecl_decoder; odecl_encoder; ] in
        return acc

    | GivenRecord(record) ->
        let otyname = Output.type_identifier name in
        let otyparams = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
        let odecl_type =
          Output.define_type_alias otyname otyparams (generate_record_type record)
        in
        let tyaliasmsg =
          let tyargs = otyparams |> List.map Output.type_variable in
          Output.type_name otyname tyargs
        in
        let odecl_decoder =
          let oparams = make_decoder_parameters def.def_params in
          let tyret = Output.decoder_type tyaliasmsg in
          let otree_decoder = decoder_of_record record in
          Output.define_value ovar_decoder otyparams oparams tyret otree_decoder
        in
        let odecl_encoder =
          let oparams = make_encoder_parameters def.def_params in
          let tyret = Output.encoder_type tyaliasmsg in
          let otree_encoder = encoder_of_record record in
          Output.define_value ovar_encoder otyparams oparams tyret otree_encoder
        in
        let acc = Alist.append acc [ odecl_type; odecl_decoder; odecl_encoder; ] in
        return acc

    | GivenVariant(variant) ->
        let otyname = Output.type_identifier name in
        let otyparams = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
        let odecl_type =
          let otymain =
            VariantMap.fold (fun ctor argmsgopt acc ->
              let oargtyopt =
                match argmsgopt with
                | None         -> None
                | Some(argmsg) -> Some(generate_message_type argmsg)
              in
              Alist.extend acc (ctor, oargtyopt)
            ) variant Alist.empty |> Alist.to_list
          in
          Output.define_data_type otyname otyparams otymain
        in
        let tyaliasmsg =
          let tyargs = otyparams |> List.map Output.type_variable in
          Output.type_name otyname tyargs
        in
        let odecl_decoder =
          let oparams = make_decoder_parameters def.def_params in
          let tyret = Output.decoder_type tyaliasmsg in
          let otree_decoder = decoder_of_variant variant in
          Output.define_value ovar_decoder otyparams oparams tyret otree_decoder
        in
        let odecl_encoder =
          let oparams = make_encoder_parameters def.def_params in
          let tyret = Output.encoder_type tyaliasmsg in
          let otree_encoder = encoder_of_variant variant in
          Output.define_value ovar_encoder otyparams oparams tyret otree_encoder
        in
        let acc = Alist.append acc [ odecl_type; odecl_decoder; odecl_encoder; ] in
        return acc

    | GivenExternal(extern) ->
        begin
          match extern |> ExternalMap.find_opt "elm" with
          | None ->
              error (NoExternal{ format = "elm"; name = name })

          | Some(dict) ->
              begin
                match def.def_params with
                | _ :: _ ->
                    error NoParameterAllowedForExternal

                | [] ->
                    get_mandatory_string dict "type" >>= fun type_text ->
                    get_mandatory_string dict "decoder" >>= fun decoder_text ->
                    get_mandatory_string dict "encoder" >>= fun encoder_text ->
                    let otyname = Output.type_identifier name in
                    let odecl_type =
                      Output.define_type_by_text otyname type_text
                    in
                    let tyaliasmsg =
                      Output.type_name otyname []
                    in
                    let odecl_decoder =
                      let tyannot = Output.decoder_type tyaliasmsg in
                      Output.define_value_by_text ovar_decoder tyannot decoder_text
                    in
                    let odecl_encoder =
                      let tyannot = Output.encoder_type tyaliasmsg in
                      Output.define_value_by_text ovar_encoder tyannot encoder_text
                    in
                    let acc = Alist.append acc [ odecl_type; odecl_decoder; odecl_encoder; ] in
                    return acc
              end
        end

  ) decls (return Alist.empty) >>= fun acc ->
  let odecls = acc |> Alist.to_list in
  let sdecls =
    odecls |> List.map (fun odecl ->
      Output.stringify_declaration odecl ^ "\n\n"
    )
  in
  let (n1, n2, n3) = language_version in
  let s =
    List.concat [
      [
        Printf.sprintf "-- Auto-generated by APBuf %d.%d.%d\n" n1 n2 n3;
        Printf.sprintf "module %s exposing (..)\n" module_name;
        "import Json.Decode\n";
        "import Json.Encode\n";
      ];
      (imports |> List.map (fun s -> Printf.sprintf "import %s\n" s));
      [
        "\n";
      ];
      sdecls;
    ] |> String.concat ""
  in
  return s
