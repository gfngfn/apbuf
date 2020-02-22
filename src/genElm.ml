
open Types

type output_variable =
  | OVar of string
[@@deriving show { with_path = false; }]

type output_tree =
  | OIdentifier of output_variable
  | OApplication of {
      applied   : output_variable;
      arguments : output_tree list;
    }
  | OAbstract of {
      variable : output_variable;
      body     : output_tree;
    }
  | OStringLiteral of string
  | OConstructor of string
  | ORecord of output_tree RecordMap.t
      [@printer (pp_record_map pp_output_tree)]
  | OBranching of output_tree VariantMap.t
      [@printer (fun ppf _ -> Format.fprintf ppf "<branch>")]
[@@deriving show { with_path = false; }]

type output_declaration =
  | ODefVal of {
      val_name   : output_variable;
      parameters : output_variable list;
      body       : output_tree;
    }
[@@deriving show { with_path = false; }]


let make_global_val_name name =
  OVar("decode_" ^ name)


let make_local_val_for_key x =
  OVar("local_key_" ^ x)


let make_local_val_for_parameter x =
  OVar("local_param_" ^ x)


let field_access (key : key) (otree : output_tree) : output_tree =
  OApplication{
    applied   = OVar("Json.Decode.field");
    arguments = [ OStringLiteral(key); otree; ];
  }


let and_then (otree_cont : output_tree) (otree_dec : output_tree) : output_tree =
  OApplication{
    applied   = OVar("Json.Decode.andThen");
    arguments = [ otree_cont; otree_dec; ];
  }


let decode_map (otree_map : output_tree) (otree_dec : output_tree) : output_tree =
  OApplication{
    applied   = OVar("Json.Decode.map");
    arguments = [ otree_map; otree_dec; ];
  }


let decode_succeed (otree : output_tree) : output_tree =
  OApplication{
    applied   = OVar("Json.Decode.succeed");
    arguments = [ otree; ];
  }


let decoder_of_variable (x : variable) : output_tree =
  OIdentifier(make_local_val_for_parameter x)


let rec decoder_of_name (name : identifier) (args : message list) : output_tree =
  let otrees = args |> List.map generate_message_decoder in
  OApplication{
    applied   = make_global_val_name name;
    arguments = otrees;
  }


and decoder_of_record (rcd : message RecordMap.t) : output_tree =
  let otree_ans =
    ORecord(rcd |> RecordMap.mapi (fun key _ ->
      OIdentifier(make_local_val_for_key key)
    ))
  in
  let acc =
    RecordMap.fold (fun key vmsg acc ->
      let otree_decv = generate_message_decoder vmsg in
      let otree_accessk = field_access key otree_decv in
      Alist.extend acc (key, otree_accessk)
    ) rcd Alist.empty
  in
  List.fold_right (fun (key, otree_accessk) otree_acc ->
    and_then (OAbstract{ variable = make_local_val_for_key key; body = otree_acc; }) otree_accessk
  ) (acc |> Alist.to_list) otree_ans


and decoder_of_variant (variant : (message option) VariantMap.t) : output_tree =
  let ocases =
    variant |> VariantMap.mapi (fun ctor argmsgopt ->
      match argmsgopt with
      | None ->
          decode_succeed (OConstructor(ctor))

      | Some(argmsg) ->
          let otree_decarg = generate_message_decoder argmsg in
          decode_map (OConstructor(ctor)) otree_decarg
    )
  in
  OBranching(ocases)


and generate_message_decoder (msg : message) =
  match msg with
  | Variable(x)      -> decoder_of_variable x
  | Name(name, args) -> decoder_of_name name args
  | Record(rcd)      -> decoder_of_record rcd


let rec stringify_tree (otree : output_tree) : string =
  match otree with
  | OIdentifier(OVar(s)) ->
      s

  | OApplication{ applied = OVar(s); arguments = args; } ->
      begin
        match args with
        | [] ->
            s

        | _ :: _ ->
            let sargs = args |> List.map stringify_tree in
            Format.sprintf "(%s %s)" s (String.concat " " sargs)
      end

  | OAbstract{ variable = OVar(s); body = body; } ->
      Format.sprintf "(\\%s -> %s)" s (stringify_tree body)

  | OStringLiteral(s) ->
      Format.sprintf "\"%s\"" s

  | OConstructor(ctor) ->
      ctor

  | ORecord(orcd) ->
      let ss =
        RecordMap.fold (fun key otree acc ->
          let s = Format.sprintf "%s = %s" key (stringify_tree otree) in
          Alist.extend acc s
        ) orcd Alist.empty |> Alist.to_list
      in
      Format.sprintf "{ %s }" (String.concat ", " ss)

  | OBranching(_obr) ->
      "(branching)" (* TODO *)


let generate_decoder (decls : declarations) =
  let acc =
    DeclMap.fold (fun name def acc ->
      let ovar = make_global_val_name name in
      let oparams = def.def_params |> List.map make_local_val_for_parameter in
      match def.def_main with
      | BuiltIn(_) ->
          acc

      | GivenNormal(msg) ->
          let otree = generate_message_decoder msg in
          let odecl =
            ODefVal{
              val_name   = ovar;
              parameters = oparams;
              body       = otree;
            }
          in
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
          let odecl_val =
            ODefVal{
              val_name   = ovar;
              parameters = oparams;
              body       = otree;
            }
          in
          Alist.extend acc odecl_val

    ) decls Alist.empty
  in
  acc |> Alist.to_list |> List.map (function
  | ODefVal{
      val_name   = OVar(s);
      parameters = oparams;
      body       = otree;
    } ->
      Format.sprintf "%s %s = %s"
        s
        (String.concat " " (oparams |> List.map (fun (OVar(s)) -> s)))
        (stringify_tree otree)
  ) |> String.concat "\n"
