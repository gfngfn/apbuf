
open Types

type output_variable =
  | OVar of string

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
  | ORecord of output_tree RecordMap.t
  | OBranching of (output_tree option) VariantMap.t

type output_declaration =
  | ODefType
  | ODefVal of {
      val_name   : output_variable;
      parameters : output_variable list;
      body       : output_tree;
    }


let make_global_val_name name =
  OVar("decode_" ^ name)


let make_local_val_name x =
  OVar("local_" ^ x)


let field_access (key : key) (otree : output_tree) : output_tree =
  OApplication{
    applied   = OVar("Json.Decode.field");
    arguments = [ OStringLiteral(key); otree; ];
  }


let and_then (otree_cont : output_tree) (otree : output_tree) : output_tree =
  OApplication{
    applied   = OVar("Json.Decode.andThen");
    arguments = [ otree_cont; otree; ];
  }


let decoder_of_variable (x : variable) : output_tree =
  OIdentifier(make_local_val_name x)


let rec decoder_of_name (name : identifier) (args : message list) : output_tree =
  let otrees = args |> List.map generate_message_decoder in
  OApplication{
    applied   = make_global_val_name name;
    arguments = otrees;
  }


and decoder_of_record (rcd : message RecordMap.t) : output_tree =
  let otree_ans =
    ORecord(rcd |> RecordMap.mapi (fun key _ ->
      OIdentifier(make_local_val_name key)
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
    and_then (OAbstract{ variable = make_local_val_name key; body = otree_acc; }) otree_accessk
  ) (acc |> Alist.to_list) otree_ans


and decoder_of_variant (variant : (message option) VariantMap.t) : output_tree =
  let ocases =
    variant |> VariantMap.map (fun argmsgopt ->
      match argmsgopt with
      | None         -> None
      | Some(argmsg) -> Some(generate_message_decoder argmsg)
    )
  in
  OBranching(ocases)


and generate_message_decoder (msg : message) =
  match msg with
  | Variable(x)      -> decoder_of_variable x
  | Name(name, args) -> decoder_of_name name args
  | Record(rcd)      -> decoder_of_record rcd
  | Variant(variant) -> decoder_of_variant variant


let generate_decoder (decls : declarations) =
  DeclMap.fold (fun name def acc ->
    match def.def_main with
    | BuiltIn(_) ->
        acc

    | Given(msg) ->
        let otree = generate_message_decoder msg in
        let odecl =
          ODefVal{
            val_name   = make_global_val_name name;
            parameters = def.def_params |> List.map make_local_val_name;
            body       = otree;
          }
        in
        Alist.extend acc odecl

  ) decls Alist.empty
