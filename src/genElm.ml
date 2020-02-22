
open Types


module Output : sig

  type identifier
  type tree
  type declaration
  val identifier : identifier -> tree
  val application : identifier -> tree list -> tree
  val abstraction : identifier -> tree -> tree
  val string_literal : string -> tree
  val constructor : Types.constructor -> tree
  val record : tree RecordMap.t -> tree
  val branching : tree VariantMap.t -> tree
  val global : Types.identifier -> identifier
  val local_for_key : Types.key -> identifier
  val local_for_parameter : Types.variable -> identifier
  val field_access : Types.key -> tree -> tree
  val access_argument : tree -> tree
  val and_then : tree -> tree -> tree
  val map : tree -> tree -> tree
  val succeed : tree -> tree
  val stringify_declaration : declaration -> string
  val define_value : identifier -> identifier list -> tree -> declaration

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
        applied   : identifier;
        arguments : tree list;
      }
    | Abstract of {
        variable : identifier;
        body     : tree;
      }
    | StringLiteral of string
    | Constructor of string
    | Record of tree RecordMap.t
    | Case of {
        subject  : tree;
        branches : (pattern * tree) list;
      }

  type declaration =
    | DefVal of {
        val_name   : identifier;
        parameters : identifier list;
        body       : tree;
      }

  let identifier ovar =
    Identifier(ovar)

  let application ovar otrees =
    Application{
      applied   = ovar;
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

  let global name =
    Var("decode_" ^ name)

  let local_for_key x =
    Var("local_key_" ^ x)

  let local_for_parameter x =
    Var("local_param_" ^ x)

  let field_access (key : key) (otree : tree) : tree =
    Application{
      applied   = Var("Json.Decode.field");
      arguments = [ StringLiteral(key); otree; ];
    }

  let and_then (otree_cont : tree) (otree_dec : tree) : tree =
    Application{
      applied   = Var("Json.Decode.andThen");
      arguments = [ otree_cont; otree_dec; ];
    }

  let map (otree_map : tree) (otree_dec : tree) : tree =
    Application{
      applied   = Var("Json.Decode.map");
      arguments = [ otree_map; otree_dec; ];
    }

  let succeed (otree : tree) =
    Application{
      applied   = Var("Json.Decode.succeed");
      arguments = [ otree; ];
    }

  let access_argument (otree : tree) =
    Application{
      applied = Var("Json.Decoder.field");
      arguments = [ StringLiteral(argument_key); otree; ]
    }

  let branching omap =
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
            applied   = Var("Json.Decode.fail");
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


  let define_value ovar oparams otree =
    DefVal{
      val_name   = ovar;
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

    | Application{ applied = Var(s); arguments = args; } ->
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

    | Record(orcd) ->
        let ss =
          RecordMap.fold (fun key otree acc ->
            let s = Format.sprintf "%s = %s" key (stringify_tree (indent + 1) otree) in
            Alist.extend acc s
          ) orcd Alist.empty |> Alist.to_list
        in
        Format.sprintf "{ %s }" (String.concat ", " ss)

    | Case{ subject = otree_subject; branches = branches } ->
        let sindent = "\n" ^ String.make ((indent + 1) * 2) ' ' in
        let ss =
          branches |> List.map (fun (pat, otree) ->
            Format.sprintf "%s%s -> %s" sindent (stringify_pattern pat) (stringify_tree (indent + 2) otree)
          )
        in
        Format.sprintf "(case %s of %s)" (stringify_tree (indent + 1) otree_subject) (String.concat "" ss)


  let stringify_declaration (odecl : declaration) : string =
    match odecl with
    | DefVal{
        val_name   = Var(s);
        parameters = oparams;
        body       = otree;
      } ->
        Format.sprintf "%s%s = %s"
          s
          (String.concat "" (oparams |> List.map (fun (Var(s)) -> " " ^ s)))
          (stringify_tree 1 otree)

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
    Output.record orcd
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


and generate_message_decoder (msg : message) =
  match msg with
  | Variable(x)      -> decoder_of_variable x
  | Name(name, args) -> decoder_of_name name args
  | Record(rcd)      -> decoder_of_record rcd


let generate_decoder (decls : declarations) =
  let acc =
    DeclMap.fold (fun name def acc ->
      let ovar = Output.global name in
      let oparams = def.def_params |> List.map Output.local_for_parameter in
      match def.def_main with
      | BuiltIn(_) ->
          acc

      | GivenNormal(msg) ->
          let otree = generate_message_decoder msg in
          let odecl = Output.define_value ovar oparams otree in
          Alist.extend acc odecl

      | GivenVariant(variant) ->
          let otree = decoder_of_variant variant in
(*
          let odecl_type =
            ODefType{
              type_name       = make_global_type_name msg;
              type_parameters = def.def_params |> List.map make_local_type_parameter;
              body            =
            }
          in
*)
          let odecl_val = Output.define_value ovar oparams otree in
          Alist.extend acc odecl_val

    ) decls Alist.empty
  in
  acc |> Alist.to_list |> List.map Output.stringify_declaration |> String.concat "\n"
