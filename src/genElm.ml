
open Types


module Output : sig

  type identifier
  type tree
  type type_identifier
  type type_parameter
  type typ
  type declaration
  val identifier : identifier -> tree
  val application : identifier -> tree list -> tree
  val general_application : tree -> tree -> tree
  val abstraction : identifier -> tree -> tree
  val string_literal : string -> tree
  val constructor : Types.constructor -> tree
  val tuple : tree list -> tree
  val list : tree list -> tree
  val record : tree RecordMap.t -> tree
  val record_field_access : tree -> Types.key -> tree
  val branching : tree VariantMap.t -> tree
  val global : Types.identifier -> identifier
  val local_for_key : Types.key -> identifier
  val local_for_parameter : Types.variable -> identifier
  val type_identifier : Types.identifier -> type_identifier
  val type_parameter : Types.variable -> type_parameter
  val field_access : Types.key -> tree -> tree
  val access_argument : tree -> tree
  val and_then : tree -> tree -> tree
  val map : tree -> tree -> tree
  val succeed : tree -> tree
  val encode_dict : tree -> tree
  val stringify_declaration : declaration -> string
  val define_value : identifier -> typ -> identifier list -> tree -> declaration
  val built_in : built_in -> declaration
  val type_name : type_identifier -> typ list -> typ
  val type_variable : type_parameter -> typ
  val record_type : (Types.key * typ) list -> typ
  val function_type : typ -> typ -> typ
  val decoder_type : typ -> typ
  val define_type_alias : type_identifier -> type_parameter list -> typ -> declaration
  val define_data_type : type_identifier -> type_parameter list -> (Types.constructor * typ option) list -> declaration

end = struct

  let label_key    = "_label"
  let argument_key = "_arg"

  type identifier =
    | Var of string

  type pattern =
    | StringPattern of string
    | IdentifierPattern of identifier

  type tree =
    | Identifier of identifier
    | Application of {
        applied   : tree;
        arguments : tree list;
      }
    | Abstract of {
        variable : identifier;
        body     : tree;
      }
    | StringLiteral of string
    | Constructor of string
    | Tuple of tree list
    | List of tree list
    | Record of tree RecordMap.t
    | FieldAccess of {
        record : tree;
        key    : string;
      }
    | Case of {
        subject  : tree;
        branches : (pattern * tree) list;
      }

  type type_identifier =
    | TypeIdentifier of string

  type type_parameter =
    | TypeParameter of string

  type typ =
    | TypeName     of type_identifier * typ list
    | TypeVariable of type_parameter
    | FuncType     of typ * typ
    | RecordType   of (Types.key * typ) list

  type declaration =
    | DefVal of {
        val_name   : identifier;
        typ        : typ;
        parameters : identifier list;
        body       : tree;
      }
    | DefTypeAlias of {
        type_name  : type_identifier;
        parameters : type_parameter list;
        body       : typ;
      }
    | DefDataType of {
        type_name  : type_identifier;
        parameters : type_parameter list;
        patterns   : (Types.constructor * typ option) list;
      }

  let identifier ovar =
    Identifier(ovar)

  let application ovar otrees =
    Application{
      applied   = Identifier(ovar);
      arguments = otrees;
    }

  let abstraction ovar otree =
    Abstract{
      variable = ovar;
      body     = otree;
    }

  let string_literal s =
    StringLiteral(s)

  let constructor ctor =
    Constructor(ctor)

  let record orcd =
    Record(orcd)

  let record_field_access otree key =
    FieldAccess{
      record = otree;
      key    = key;
    }

  let global name =
    Var("decode_" ^ name)

  let local_for_key x =
    Var("local_key_" ^ x)

  let local_for_parameter x =
    Var("local_param_" ^ x)

  let type_identifier name =
    let s =
      match name with
      | "bool"   -> "Bool"
      | "int"    -> "Int"
      | "string" -> "String"
      | "list"   -> "List"
      | "option" -> "Maybe"
      | _        -> "T_" ^ name
    in
    TypeIdentifier(s)

  let type_parameter x =
    TypeParameter(x)

  let field_access (key : key) (otree : tree) : tree =
    Application{
      applied   = Identifier(Var("Json.Decode.field"));
      arguments = [ StringLiteral(key); otree; ];
    }

  let and_then (otree_cont : tree) (otree_dec : tree) : tree =
    Application{
      applied   = Identifier(Var("Json.Decode.andThen"));
      arguments = [ otree_cont; otree_dec; ];
    }

  let map (otree_map : tree) (otree_dec : tree) : tree =
    Application{
      applied   = Identifier(Var("Json.Decode.map"));
      arguments = [ otree_map; otree_dec; ];
    }

  let succeed (otree : tree) =
    Application{
      applied   = Identifier(Var("Json.Decode.succeed"));
      arguments = [ otree; ];
    }

  let encode_dict (otree : tree) =
    Application{
      applied   = Identifier(Var("Json.Encode.dict"));
      arguments = [ otree ];
    }

  let access_argument (otree : tree) =
    Application{
      applied = Identifier(Var("Json.Decode.field"));
      arguments = [ StringLiteral(argument_key); otree; ]
    }

  let branching (omap : tree VariantMap.t) =
    let otree_accesslabel =
      field_access label_key (Identifier(Var("Json.Decode.string")))
    in
    let otree_cont =
      let ovar_temp = Var("temp") in
      let branches =
        let acc =
          VariantMap.fold (fun ctor branch acc ->
            Alist.extend acc (StringPattern(ctor), branch)
          ) omap Alist.empty
        in
        let ovar_other = Var("other") in
        let otree_err =
          Application{
            applied   = Identifier(Var("Json.Decode.fail"));
            arguments = [ Identifier(ovar_other) ];
          }
        in
        Alist.extend acc (IdentifierPattern(ovar_other), otree_err) |> Alist.to_list
      in
      Abstract{
        variable = ovar_temp;
        body = Case{
          subject  = Identifier(ovar_temp);
          branches = branches;
        };
      }
    in
    and_then otree_cont otree_accesslabel


  let general_application (otree1 : tree) (otree2 : tree) : tree =
    Application{
      applied   = otree1;
      arguments = [ otree2 ];
    }


  let tuple otrees =
    Tuple(otrees)


  let list otrees =
    List(otrees)


  let define_value ovar typ oparams otree =
    DefVal{
      val_name   = ovar;
      typ        = typ;
      parameters = oparams;
      body       = otree;
    }


  let stringify_pattern (pat : pattern) : string =
    match pat with
    | StringPattern(s) ->
        Format.sprintf "\"%s\"" s

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
            let s = Format.sprintf "%s = %s" key (stringify_tree (indent + 1) otree) in
            Alist.extend acc s
          ) orcd Alist.empty |> Alist.to_list
        in
        Format.sprintf "{ %s }" (String.concat ", " ss)

    | FieldAccess{ record = otree_record; key = key; } ->
        let s = stringify_tree indent otree_record in
        Format.sprintf "%s.%s" s key

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

    | FuncType(ty1, ty2) ->
        Format.sprintf "(%s -> %s)"
          (stringify_type ty1)
          (stringify_type ty2)

    | RecordType(tyrcd) ->
        let sr =
          tyrcd |> List.map (fun (key, ty) ->
            Format.sprintf "%s : %s" key (stringify_type ty)
          ) |> String.concat ", "
        in
        Format.sprintf "{ %s }" sr



  let stringify_declaration (odecl : declaration) : string =
    match odecl with
    | DefVal{
        val_name   = Var(s);
        typ        = typ;
        parameters = oparams;
        body       = otree;
      } ->
        Format.sprintf "%s : %s\n%s%s = %s"
          s
          (stringify_type typ)
          s
          (String.concat "" (oparams |> List.map (fun (Var(s)) -> " " ^ s)))
          (stringify_tree 1 otree)

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
            Format.sprintf "  %s %s%s" sep ctor sarg
          )
        in
        Format.sprintf "type %s%s\n%s"
          s
          (String.concat "" (otyparams |> List.map (fun (TypeParameter(a)) -> " " ^ a)))
          (String.concat "\n" ss)

  let decoder_type ty =
    TypeName(TypeIdentifier("Decoder"), [ty])

  let built_in (builtin : built_in) : declaration =
    let dec = decoder_type in
    let ( !$ ) tp = TypeVariable(tp) in
    let base s = (TypeName(type_identifier s, [])) in
    match builtin with
    | BBool ->
        DefVal{
          val_name   = global "bool";
          typ        = dec (base "bool");
          parameters = [];
          body       = Identifier(Var("Json.Decode.bool"));
        }

    | BInt ->
        DefVal{
          val_name   = global "int";
          typ        = dec (base "int");
          parameters = [];
          body       = Identifier(Var("Json.Decode.int"));
        }

    | BString ->
        DefVal{
          val_name   = global "string";
          typ        = dec (base "string");
          parameters = [];
          body       = Identifier(Var("Json.Decode.string"));
        }

    | BList(s) ->
        let typaram = type_parameter s in
        DefVal{
          val_name   = global "list";
          typ        = FuncType(dec (!$ typaram), dec (TypeName(type_identifier "list", [!$ typaram])));
          parameters = [];
          body       = Identifier(Var("Json.Decode.list"));
        }

    | BOption(s) ->
        let ovar = local_for_parameter s in
        let omap =
          VariantMap.empty
            |> VariantMap.add "None" (succeed (constructor "Nothing"))
            |> VariantMap.add "Some" (access_argument (map (constructor "Just") (Identifier(ovar))))
        in
        let typaram = type_parameter s in
        DefVal{
          val_name   = global "option";
          typ        = FuncType(dec (!$ typaram), dec (TypeName(type_identifier "option", [!$ typaram])));
          parameters = [ovar];
          body       = branching omap;
        }

  let type_name (ti : type_identifier) (tyargs : typ list) : typ =
    TypeName(ti, tyargs)

  let type_variable (tp : type_parameter) : typ =
    TypeVariable(tp)

  let record_type (tyrcd : (Types.key * typ) list) : typ =
    RecordType(tyrcd)

  let function_type (ty1 : typ) (ty2 : typ) : typ =
    FuncType(ty1, ty2)

  let define_type_alias (ti : type_identifier) (typarams : type_parameter list) (ty : typ) : declaration =
    DefTypeAlias{
      type_name  = ti;
      parameters = typarams;
      body       = ty;
    }

  let define_data_type (ti : type_identifier) (typarams : type_parameter list) (defs : (Types.constructor * typ option) list) =
    DefDataType{
      type_name  = ti;
      parameters = typarams;
      patterns   = defs;
    }

end


let decoder_of_variable (x : variable) : Output.tree =
  Output.identifier (Output.local_for_parameter x)


let rec decoder_of_name (name : identifier) (args : message list) : Output.tree =
  let otrees = args |> List.map generate_message_decoder in
  Output.application (Output.global name) otrees


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
      let otree_accessk = Output.field_access key otree_decv in
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
  | Record(rcd)           -> decoder_of_record rcd


let rec generate_message_type (msg : message) : Output.typ =
  match msg with
  | Variable((_, x)) ->
      Output.type_variable (Output.type_parameter x)

  | Name((_, name), args) ->
      let tys = args |> List.map generate_message_type in
      Output.type_name (Output.type_identifier name) tys

  | Record(rcd) ->
      let tyrcd =
        RecordMap.fold (fun key vmsg acc ->
          let ty = generate_message_type vmsg in
          Alist.extend acc (key, ty)
        ) rcd Alist.empty |> Alist.to_list
      in
      Output.record_type tyrcd


let make_function_type (params : (variable ranged) list) (ty : Output.typ) : Output.typ =
  List.fold_right (fun (_, x) ty ->
    let typaram = Output.type_parameter x in
    Output.function_type (Output.decoder_type (Output.type_variable typaram)) ty
  ) params (Output.decoder_type ty)


let generate_decoder (module_name : string) (decls : declarations) =
  let odecls =
    DeclMap.fold (fun name def acc ->
      let ovar = Output.global name in
      let oparams = def.def_params |> List.map (fun (_, x) -> Output.local_for_parameter x) in
      match def.def_main with
      | BuiltIn(builtin) ->
          let odecl = Output.built_in builtin in
          Alist.extend acc odecl

      | GivenNormal(msg) ->
          let otyname = Output.type_identifier name in
          let otyparam = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
          let odecl_val =
            let tyargs = otyparam |> List.map Output.type_variable in
            let ty = make_function_type def.def_params (Output.type_name otyname tyargs) in
            let otree = generate_message_decoder msg in
            Output.define_value ovar ty oparams otree
          in
          let odecl_type =
            Output.define_type_alias otyname otyparam (generate_message_type msg)
          in
          Alist.extend (Alist.extend acc odecl_type) odecl_val

      | GivenVariant(variant) ->
          let otyname = Output.type_identifier name in
          let otyparam = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
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
            Output.define_data_type otyname otyparam otymain
          in
          let odecl_val =
            let tyargs = otyparam |> List.map Output.type_variable in
            let ty = make_function_type def.def_params (Output.type_name otyname tyargs) in
            let otree = decoder_of_variant variant in
            Output.define_value ovar ty oparams otree
          in
          Alist.extend (Alist.extend acc odecl_type) odecl_val

    ) decls Alist.empty |> Alist.to_list
  in
  let sdecls =
    odecls |> List.map (fun odecl ->
      Output.stringify_declaration odecl ^ "\n\n"
    )
  in
  List.append [
    Format.sprintf "module %s exposing (..)\n" module_name;
    "import Json.Decode exposing (Decoder)\n";
    "\n";
  ] sdecls |> String.concat ""


let encoder_of_variable (x : variable) : Output.tree =
  Output.identifier (Output.local_for_parameter x)


let rec encoder_of_name (name : identifier) (args : message list) : Output.tree =
  let otrees = args |> List.map generate_message_encoder in
  Output.application (Output.global name) otrees


and encoder_of_record (rcd : message RecordMap.t) : Output.tree =
  let x_record = Output.local_for_parameter "temp" in
  let otrees =
    RecordMap.fold (fun key vmsg acc ->
      let otree_encoder = generate_message_encoder vmsg in
      let otree_encoded =
        Output.general_application otree_encoder (Output.record_field_access (Output.identifier(x_record)) key)
      in
      let otree_pair = Output.tuple [ Output.string_literal key; otree_encoded ] in
      Alist.extend acc otree_pair
    ) rcd Alist.empty |> Alist.to_list
  in
  Output.abstraction x_record (Output.encode_dict (Output.list otrees))


(** Given a message [msg] of type [T], [generate_message_encoder msg] generates
    the code representation of encoder of type [T].
    The return value is an representation of Elm code of type [T -> Value].
  *)
and generate_message_encoder (msg : message) : Output.tree =
  match msg with
  | Variable((_, x))      -> encoder_of_variable x
  | Name((_, name), args) -> encoder_of_name name args
  | Record(rcd)           -> encoder_of_record rcd


let generate_encoder (module_name : string) (decls : declarations) =
  let odecls =
    DeclMap.fold (fun _name def acc ->
      match def.def_main with
      | BuiltIn(_builtin) ->
          acc (* TODO *)

      | GivenNormal(_msg) ->
          acc (* TODO *)

      | GivenVariant(_variant) ->
          acc (* TODO *)
    ) decls Alist.empty |> Alist.to_list
  in
  let sdecls =
    odecls |> List.map (fun odecl ->
      Output.stringify_declaration odecl ^ "\n\n"
    )
  in
  List.append [
    Format.sprintf "module %s exposing (..)\n" module_name;
    "import Json.Encode\n";
    "\n";
  ] sdecls |> String.concat ""
