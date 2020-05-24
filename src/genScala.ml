
open Types


module Output : sig
  type identifier
  val local_for_parameter : Types.variable -> identifier
  val global_reads : Name.t -> identifier
  val global_writes : Name.t -> identifier
  type type_identifier
  val type_identifier : Name.t -> type_identifier
  type type_parameter
  val type_parameter : Types.variable -> type_parameter
  type typ
  val type_variable : type_parameter -> typ
  val type_name : type_identifier -> typ list -> typ
  val reads_type : typ -> typ
  val writes_type : typ -> typ
  val func_type : typ -> typ -> typ
  type tree
  val identifier : identifier -> tree
  val application : tree -> tree list -> tree
  val make_record_reads : type_identifier -> (typ * tree) RecordMap.t -> tree
  val make_record_writes : type_identifier -> (typ * tree) RecordMap.t -> tree
  val make_variant_reads : type_identifier -> ((typ * tree) option) VariantMap.t -> tree
  val make_variant_writes : type_identifier -> ((typ * tree) option) VariantMap.t -> tree
  type declaration
  val define_variant_case_class : type_identifier -> type_parameter list -> (typ option) VariantMap.t -> declaration
  val define_record_case_class : type_identifier -> type_parameter list -> typ RecordMap.t -> declaration
  val define_type_alias : type_identifier -> type_parameter list -> typ -> declaration
  val define_method : identifier -> (identifier * typ) list -> typ -> tree -> declaration
  val stringify_declaration : declaration -> string
end = struct

  type identifier =
    | Var of string

  let global_reads name =
    Var(Name.lower_camel_case name ^ "Reads")

  let global_writes name =
    Var(Name.lower_camel_case name ^ "Writes")

  let local_for_parameter x =
    Var("local_param_" ^ x)

  type type_identifier =
    | TypeIdentifier of string

  let type_identifier (name : Name.t) =
    TypeIdentifier(Name.upper_camel_case name)

  type type_parameter =
    | TypeParameter of string

  let type_parameter (x : Types.variable) =
    TypeParameter(x)

  type typ =
    | TypeName     of type_identifier * typ list
    | TypeVariable of type_parameter
    | FuncType     of typ * typ

  let type_variable tv =
    TypeVariable(tv)

  let type_name tyident typs =
    TypeName(tyident, typs)

  let reads_type oty =
    TypeName(TypeIdentifier("Reads"), [oty])

  let writes_type oty =
    TypeName(TypeIdentifier("Writes"), [oty])

  let func_type oty1 oty2 =
    FuncType(oty1, oty2)

  let rec stringify_type (oty : typ) =
    match oty with
    | TypeName(TypeIdentifier(tynm), tyargs) ->
        begin
          match tyargs with
          | []     -> tynm
          | _ :: _ -> Printf.sprintf "%s[%s]" tynm (tyargs |> List.map stringify_type |> String.concat ", ")
        end

    | TypeVariable(TypeParameter(x)) ->
        x

    | FuncType(oty1, oty2) ->
        let sty1 = stringify_type oty1 in
        let sty2 = stringify_type oty2 in
        Printf.sprintf "(%s) => %s" sty1 sty2

  type tree =
    | Identifier of identifier
    | Application of {
        applied   : tree;
        arguments : tree list;
      }
    | RecordReads of {
        type_name : type_identifier;
        entries   : (typ * tree) RecordMap.t;
      }
    | RecordWrites of {
        type_name : type_identifier;
        entries   : (typ * tree) RecordMap.t;
      }
    | VariantReads of {
        type_name    : type_identifier;
        constructors : ((typ * tree) option) VariantMap.t;
      }
    | VariantWrites of {
        type_name    : type_identifier;
        constructors : ((typ * tree) option) VariantMap.t;
      }

  let identifier ident =
    Identifier(ident)

  let application ot ots =
    Application{ applied = ot; arguments = ots; }

  let make_record_reads tyid entries =
    RecordReads{
      type_name = tyid;
      entries   = entries;
    }

  let make_record_writes tyid entries =
    RecordWrites{
      type_name = tyid;
      entries   = entries;
    }

  let make_variant_reads tyid ctors =
    VariantReads{
      type_name    = tyid;
      constructors = ctors;
    }

  let make_variant_writes tyid ctors =
    VariantWrites{
      type_name    = tyid;
      constructors = ctors;
    }

  type declaration =
    | DefVariantCaseClass of {
        type_name    : type_identifier;
        type_params  : type_parameter list;
        constructors : (typ option) VariantMap.t;
      }
    | DefRecordCaseClass of {
        type_name   : type_identifier;
        type_params : type_parameter list;
        fields      : typ RecordMap.t;
      }
    | DefTypeAlias of {
        type_name   : type_identifier;
        type_params : type_parameter list;
        type_real   : typ
      }
    | DefMethod of {
        method_name : identifier;
        params      : (identifier * typ) list;
        return_type : typ;
        body        : tree;
      }

  let define_variant_case_class tyid typarams ctors =
    DefVariantCaseClass{
      type_name    = tyid;
      type_params  = typarams;
      constructors = ctors;
    }

  let define_record_case_class tyid typarams fields =
    DefRecordCaseClass{
      type_name   = tyid;
      type_params = typarams;
      fields      = fields;
    }

  let define_type_alias tyid typarams oty =
    DefTypeAlias{
      type_name   = tyid;
      type_params = typarams;
      type_real   = oty;
    }

  let define_method ident vtparams otyret otree =
    DefMethod{
      method_name = ident;
      params      = vtparams;
      return_type = otyret;
      body        = otree;
    }

  let rec stringify_tree (otree : tree) =
    match otree with
    | Identifier(Var(varnm)) ->
        varnm

    | Application{ applied = otfun; arguments = otargs; } ->
        let sfun = stringify_tree otfun in
        let sargs = otargs |> List.map stringify_tree in
        Printf.sprintf "%s(%s)" sfun (String.concat ", " sargs)

    | RecordReads{ type_name = TypeIdentifier(tynm); entries = entries; } ->
        let smain =
          RecordMap.fold (fun key (oty, otree_decoder) acc ->
            let skey = Key.lower_camel_case key in
            let sty = stringify_type oty in
            let sdec = stringify_tree otree_decoder in
            let s = Printf.sprintf "(JsPath \\ \"%s\").read[%s](%s)" skey sty sdec in
            Alist.extend acc s
          ) entries Alist.empty |> Alist.to_list |> String.concat " and "
        in
        Printf.sprintf "(%s)(%s.apply _)" smain tynm

    | RecordWrites{ type_name = TypeIdentifier(tynm); entries = entries; } ->
        let smain =
          RecordMap.fold (fun key (oty, otree_decoder) acc ->
            let skey = Key.lower_camel_case key in
            let sty = stringify_type oty in
            let sdec = stringify_tree otree_decoder in
            let s = Printf.sprintf "(JsPath \\ \"%s\").write[%s](%s)" skey sty sdec in
            Alist.extend acc s
          ) entries Alist.empty |> Alist.to_list |> String.concat " and "
        in
        Printf.sprintf "(%s)(unlift(%s.unapply))" smain tynm

    | VariantReads{ type_name = TypeIdentifier(_tynm); constructors = ctors; } ->
        let scases =
          VariantMap.fold (fun ctor otreeopt acc ->
            let s =
              match otreeopt with
              | None ->
                  Printf.sprintf "case \"%s\" => Reads.pure(%s)" ctor ctor

              | Some(oty, otree_decoder) ->
                  let sty = stringify_type oty in
                  let sdec = stringify_tree otree_decoder in
                  Printf.sprintf "case \"%s\" => (JsPath \\ \"arg\").read[%s](%s).flatMap { (x) => Reads.pure(%s(x)) }" ctor sty sdec ctor
            in
            Alist.extend acc s

          ) ctors Alist.empty |> Alist.to_list |> String.concat " "
        in
        Printf.sprintf "(JsPath \\ \"label\").read[String].flatMap { (label: String) => label match { %s }}" scases

    | VariantWrites{ type_name = TypeIdentifier(tynm); constructors = ctors; } ->
        let scases =
          VariantMap.fold (fun ctor otreeopt acc ->
            let s =
              match otreeopt with
              | None ->
                  Printf.sprintf "case %s() => (JsPath \\ \"label\").write[%s](%s)" ctor tynm ctor

              | Some(oty, otree_encoder) ->
                  let sty = stringify_type oty in
                  let senc = stringify_tree otree_encoder in
                  let slabel =
                    Printf.sprintf "(JsPath \\ \"label\").write[String](%s)(StringWrites)" ctor
                  in
                  let sarg =
                    Printf.sprintf "(JsPath \\ \"arg\").write[%s](arg)(%s)" sty senc
                  in
                  Printf.sprintf "case %s(arg) => (%s and %s)(unlift(%s.unapply))" ctor slabel sarg sty
            in
            Alist.extend acc s

          ) ctors Alist.empty |> Alist.to_list |> String.concat " "
        in
        Printf.sprintf "(Writes.apply { (x: %s) => val writer = x match { %s }; Json.toJson(x)(writer) })" tynm scases

  let make_parameter_string otyparams =
    let styparams = otyparams |> List.map (function TypeParameter(s) -> s) in
    match styparams with
    | []     -> ""
    | _ :: _ -> "[" ^ (String.concat ", " styparams) ^ "]"

  let stringify_declaration (odecl : declaration) =
    match odecl with
    | DefVariantCaseClass{
        type_name    = TypeIdentifier(tynm);
        type_params  = otyparams;
        constructors = ctors;
      } ->
        let paramseq = make_parameter_string otyparams in
        let smain = Printf.sprintf "%s%s" tynm paramseq in
        let sctors =
          VariantMap.fold (fun ctornm otyopt acc ->
            let s =
              match otyopt with
              | None ->
                  Printf.sprintf "case class %s%s() extends %s\n" ctornm paramseq smain

              | Some(oty) ->
                  let sty = stringify_type oty in
                  Printf.sprintf "case class %s%s(arg: %s) extends %s\n" ctornm paramseq sty smain
            in
            Alist.extend acc s
          ) ctors Alist.empty |> Alist.to_list
        in
        (Printf.sprintf "sealed trait %s\n" smain) :: sctors
        |> String.concat ""

    | DefRecordCaseClass{
        type_name   = TypeIdentifier(tynm);
        type_params = otyparams;
        fields      = fields;
      } ->
        let paramseq = make_parameter_string otyparams in
        let sfields =
          RecordMap.fold (fun key oty acc ->
            let skey = Key.lower_camel_case key in
            let sty = stringify_type oty in
            let s = Printf.sprintf "%s: %s" skey sty in
            Alist.extend acc s
          ) fields Alist.empty
            |> Alist.to_list |> String.concat ", "
        in
        Printf.sprintf "case class %s%s(%s)\n" tynm paramseq sfields

    | DefTypeAlias{
        type_name   = TypeIdentifier(tynm);
        type_params = otyparams;
        type_real   = oty;
      } ->
        let paramseq = make_parameter_string otyparams in
        let sty = stringify_type oty in
        Printf.sprintf "type %s%s = %s\n" tynm paramseq sty

    | DefMethod{
        method_name = Var(varnm);
        return_type = _oty;
        params      = ovtparams;
        body        = otree;
      } ->
        let sparams =
          ovtparams |> List.map (function (Var(sv), oty) ->
            let sty = stringify_type oty in
            Printf.sprintf "%s: %s" sv sty
          )
        in
        let paramseq = String.concat ", " sparams in
        let sbody = stringify_tree otree in
        Printf.sprintf "def %s(%s) = { %s }\n" varnm paramseq sbody

end


let rec generate_message_type (msg : message) : Output.typ =
  match msg with
  | Variable((_, x)) ->
      Output.type_variable (Output.type_parameter x)

  | Name((_, name), args) ->
      let typs = args |> List.map generate_message_type in
      Output.type_name (Output.type_identifier name) typs


let rec generate_message_decoder_or_encoder (namef : Name.t -> Output.identifier) (msg : message) : Output.tree =
  match msg with
  | Variable((_, x)) ->
      Output.identifier (Output.local_for_parameter x)

  | Name((_, name), args) ->
      let otrees = args |> List.map (generate_message_decoder_or_encoder namef) in
      Output.application (Output.identifier (namef name)) otrees


let generate_message_decoder : message -> Output.tree =
  generate_message_decoder_or_encoder Output.global_reads


let generate_message_encoder : message -> Output.tree =
  generate_message_decoder_or_encoder Output.global_writes


let decoder_of_record (tyid : Output.type_identifier) (record : record) : Output.tree =
  let entries =
    record |> RecordMap.map (fun msg ->
      let otree_decoder = generate_message_decoder msg in
      let typ = generate_message_type msg in
      (typ, otree_decoder)
    )
  in
  Output.make_record_reads tyid entries


let encoder_of_record (tyid : Output.type_identifier) (record : record) : Output.tree =
  let entries =
    record |> RecordMap.map (fun msg ->
      let otree_encoder = generate_message_encoder msg in
      let typ = generate_message_type msg in
      (typ, otree_encoder)
    )
  in
  Output.make_record_writes tyid entries


let decoder_of_variant (tyid : Output.type_identifier) (variant : variant) : Output.tree =
  let ctors =
    variant |> VariantMap.map (Option.map (fun msg ->
      let oty = generate_message_type msg in
      let otree_decoder = generate_message_decoder msg in
      (oty, otree_decoder)
    ))
  in
  Output.make_variant_reads tyid ctors


let encoder_of_variant (tyid : Output.type_identifier) (variant : variant) : Output.tree =
  let ctors =
    variant |> VariantMap.map (Option.map (fun msg ->
      let oty = generate_message_type msg in
      let otree_encoder = generate_message_encoder msg in
      (oty, otree_encoder)
    ))
  in
  Output.make_variant_writes tyid ctors


let generate (module_name : string) (package_name : string) (decls : declarations) =
  let odecls : Output.declaration list =
    DeclMap.fold (fun name def acc ->
      match def.def_main with
      | BuiltIn(_builtin) ->
          acc  (* TEMPORARY; TODO *)

      | GivenNormal(msg) ->
          let otyname = Output.type_identifier name in
          let otyparams = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
          let otymsg = Output.type_name otyname (otyparams |> List.map Output.type_variable) in
          let odecl_type : Output.declaration =
            let oty = generate_message_type msg in
            Output.define_type_alias otyname otyparams oty
          in
          let odecl_reads : Output.declaration =
            let ovar_reads = Output.global_reads name in
            let ovtparams =
              def.def_params |> List.map (fun (_, x) ->
                let otyparam = Output.type_parameter x in
                (Output.local_for_parameter x, Output.reads_type (Output.type_variable otyparam))
              )
            in
            let otyret = Output.reads_type otymsg in
            let otree_decoder = generate_message_decoder msg in
            Output.define_method ovar_reads ovtparams otyret otree_decoder
          in
          let odecl_writes : Output.declaration =
            let ovar_writes = Output.global_writes name in
            let ovtparams =
              def.def_params |> List.map (fun (_, x) ->
                let otyparam = Output.type_parameter x in
                (Output.local_for_parameter x, Output.writes_type (Output.type_variable otyparam))
              )
            in
            let otree_encoder = generate_message_encoder msg in
            let otyret = Output.writes_type otymsg in
            Output.define_method ovar_writes ovtparams otyret otree_encoder
          in
          Alist.append acc [ odecl_type; odecl_reads; odecl_writes ]

      | GivenRecord(record) ->
          let otyname = Output.type_identifier name in
          let otyparams = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
          let otymsg = Output.type_name otyname (otyparams |> List.map Output.type_variable) in
          let odecl_type : Output.declaration =
            let fields = record |> RecordMap.map generate_message_type in
            Output.define_record_case_class otyname otyparams fields
          in
          let odecl_reads =
            let otree_decoder = decoder_of_record otyname record in
            let ovar_reads = Output.global_reads name in
            let ovtparams =
              def.def_params |> List.map (fun (_, x) ->
                let otyparam = Output.type_parameter x in
                (Output.local_for_parameter x, Output.reads_type (Output.type_variable otyparam))
              )
            in
            let otyret = Output.reads_type otymsg in
            Output.define_method ovar_reads ovtparams otyret otree_decoder
          in
          let odecl_writes =
            let otree_encoder = encoder_of_record otyname record in
            let ovar_writes = Output.global_writes name in
            let ovtparams =
              def.def_params |> List.map (fun (_, x) ->
                let otyparam = Output.type_parameter x in
                (Output.local_for_parameter x, Output.writes_type (Output.type_variable otyparam))
              )
            in
            let otyret = Output.writes_type otymsg in
            Output.define_method ovar_writes ovtparams otyret otree_encoder
          in
          Alist.append acc [ odecl_type; odecl_reads; odecl_writes ]

      | GivenVariant(variant) ->
          let otyname = Output.type_identifier name in
          let otyparams = def.def_params |> List.map (fun (_, x) -> Output.type_parameter x) in
          let otymsg = Output.type_name otyname (otyparams |> List.map Output.type_variable) in
          let odecl_type : Output.declaration =
            let ctors = variant |> VariantMap.map (Option.map generate_message_type) in
            Output.define_variant_case_class otyname otyparams ctors
          in
          let odecl_reads : Output.declaration =
            let otree_decoder = decoder_of_variant otyname variant in
            let ovar_reads = Output.global_reads name in
            let ovtparams =
              def.def_params |> List.map (fun (_, x) ->
                let otyparam = Output.type_parameter x in
                (Output.local_for_parameter x, Output.reads_type (Output.type_variable otyparam))
              )
            in
            let otyret = Output.reads_type otymsg in
            Output.define_method ovar_reads ovtparams otyret otree_decoder
          in
          let odecl_writes : Output.declaration =
            let otree_encoder = encoder_of_variant otyname variant in
            let ovar_writes = Output.global_writes name in
            let ovtparams =
              def.def_params |> List.map (fun (_, x) ->
                let otyparam = Output.type_parameter x in
                (Output.local_for_parameter x, Output.writes_type (Output.type_variable otyparam))
              )
            in
            let otyret = Output.writes_type otymsg in
            Output.define_method ovar_writes ovtparams otyret otree_encoder
          in
          Alist.append acc [ odecl_type; odecl_reads; odecl_writes ]

    ) decls Alist.empty |> Alist.to_list
  in
  let sdecls =
    odecls |> List.map (fun odecl ->
      "  " ^ Output.stringify_declaration odecl ^ "\n\n"
    )
  in
  List.concat [
    [
      Format.sprintf "package %s\n" package_name;
      "\n";
      "import play.api.libs.json._\n";
      "import play.api.libs.json.Reads._\n";
      "import play.api.libs.functional.syntax._\n";
      "\n";
      Format.sprintf "object %s {\n" module_name;
    ];
    sdecls;
    [
      "}\n";
    ];
  ] |> String.concat ""
