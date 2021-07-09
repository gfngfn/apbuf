
open Types


module type CONSTANT = sig
  val global_decoder : Name.t -> string
  val global_encoder : Name.t -> string
  val local_for_key : Key.t -> string
  val local_for_parameter : Variable.t -> string
  val key : Key.t -> string
  val constructor : Constructor.t -> string
  val type_identifier : Name.t -> string
  val type_parameter : Variable.t -> string
end


module Make(Constant : CONSTANT) = struct

  type identifier =
    | Var of string


  let global_decoder name   = Var(Constant.global_decoder name)
  let global_encoder name   = Var(Constant.global_encoder name)
  let local_for_key key     = Var(Constant.local_for_key key)
  let local_for_parameter x = Var(Constant.local_for_parameter x)


  type pattern =
    | StringPattern      of string
    | ConstructorPattern of string * pattern option
    | IdentifierPattern  of identifier


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
        key    : Key.t;
      }
    | Case of {
        subject  : tree;
        branches : (pattern * tree) list;
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
    Constructor(Constant.constructor ctor)


  let record orcd =
    Record(orcd)


  let record_field_access otree key =
    FieldAccess{
      record = otree;
      key    = key;
    }


  let decode_field_access_raw (skey : string) (otree : tree) : tree =
    Application{
      applied   = Identifier(Var("Json.Decode.field"));
      arguments = [ StringLiteral(skey); otree; ];
    }


  let decode_field_access (key : Key.t) (otree : tree) : tree =
    decode_field_access_raw (CommonConstant.key_for_json key) otree


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


  let encode_record (otree : tree) =
    Application{
      applied   = Identifier(Var("Json.Encode.object"));
      arguments = [ otree ];
    }


  let access_argument (otree : tree) =
    Application{
      applied = Identifier(Var("Json.Decode.field"));
      arguments = [ StringLiteral(CommonConstant.arg_field); otree; ]
    }


  let branching (omap : tree VariantMap.t) =
    let otree_accesslabel =
      decode_field_access_raw CommonConstant.label_field (Identifier(Var("Json.Decode.string")))
    in
    let otree_cont =
      let ovar_temp = Var("temp") in
      let branches =
        let acc =
          VariantMap.fold (fun ctor branch acc ->
            Alist.extend acc (StringPattern(CommonConstant.label_for_json ctor), branch)
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


  let encode_variant (jctor : string) (argopt : tree option) : tree =
    let otree_label = general_application (Identifier(Var("Json.Encode.string"))) (string_literal jctor) in
    let otree_label_keyval = tuple [ string_literal CommonConstant.label_field; otree_label ] in
    let entries =
      match argopt with
      | None      -> [ otree_label_keyval ]
      | Some(arg) -> [ otree_label_keyval; tuple [ string_literal CommonConstant.arg_field; arg ] ]
    in
    encode_record (list entries)


  let encode_branching (variant : (tree option) VariantMap.t) : tree =
    let ovar_toenc = Var("temp") in
    let branches =
      VariantMap.fold (fun ctor encopt acc ->
        let ovar_toencsub = Var("sub") in
        let (otreeopt, paramopt) =
          match encopt with
          | None ->
              (None, None)

          | Some(enc) ->
              (Some(general_application enc (identifier ovar_toencsub)), Some(IdentifierPattern(ovar_toencsub)))
        in
        let jctor = CommonConstant.label_for_json ctor in
        let pat = ConstructorPattern(jctor, paramopt) in
        Alist.extend acc (pat, encode_variant jctor otreeopt)
      ) variant Alist.empty |> Alist.to_list
    in
    Abstract{
      variable = ovar_toenc;
      body = Case{
        subject  = Identifier(ovar_toenc);
        branches = branches;
      }
    }


  let encoded_none : tree =
    encode_variant "None" None


  let encoded_some (otree_arg : tree) : tree =
    encode_variant "Some" (Some(otree_arg))


  type type_identifier =
    | TypeIdentifier of string


  let type_identifier name =
    TypeIdentifier(Constant.type_identifier name)


  type type_parameter =
    | TypeParameter of string


  let type_parameter x =
    TypeParameter(Constant.type_parameter x)


  type typ =
    | TypeName     of type_identifier * typ list
    | TypeVariable of type_parameter
    | FuncType     of typ * typ
    | RecordType   of (Key.t * typ) list


  let decoder_type ty =
    TypeName(TypeIdentifier("Json.Decode.Decoder"), [ty])


  let encoder_type ty =
    FuncType(ty, TypeName(TypeIdentifier("Json.Encode.Value"), []))


  let base s =
    TypeName(type_identifier s, [])


  let type_name (ti : type_identifier) (tyargs : typ list) : typ =
    TypeName(ti, tyargs)


  let type_variable (tp : type_parameter) : typ =
    TypeVariable(tp)


  let record_type (tyrcd : (Key.t * typ) list) : typ =
    RecordType(tyrcd)


  let function_type (ty1 : typ) (ty2 : typ) : typ =
    FuncType(ty1, ty2)


  type declaration =
    | DefVal of {
        val_name   : identifier;
        typ        : typ;
        parameters : identifier list;
        body       : tree;
      }
    | DefValByText of {
        val_name : identifier;
        typ      : typ;
        text     : string;
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
    | DefTypeByText of {
        type_name : type_identifier;
        text      : string;
      }


  let define_value ovar typ oparams otree =
    DefVal{
      val_name   = ovar;
      typ        = typ;
      parameters = oparams;
      body       = otree;
    }


  let define_value_by_text ovar typ text =
    DefValByText{
      val_name = ovar;
      typ      = typ;
      text     = text;
    }


  let define_type_alias (ti : type_identifier) (typarams : type_parameter list) (ty : typ) : declaration =
    DefTypeAlias{
      type_name  = ti;
      parameters = typarams;
      body       = ty;
    }


  let define_type_by_text (ti : type_identifier) (text : string) : declaration =
    DefTypeByText{
      type_name = ti;
      text      = text;
    }


  let define_data_type (ti : type_identifier) (typarams : type_parameter list) (defs : (Types.constructor * typ option) list) =
    DefDataType{
      type_name  = ti;
      parameters = typarams;
      patterns   = defs;
    }

end
