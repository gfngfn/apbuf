
open Types


module Output : sig
  type identifier
  val local_for_parameter : Types.variable -> identifier
  val global_reads : Name.t -> identifier
  type type_identifier
  val type_identifier : Name.t -> type_identifier
  type type_parameter
  val type_parameter : Types.variable -> type_parameter
  type typ
  val type_variable : type_parameter -> typ
  val type_name : type_identifier -> typ list -> typ
  type tree
  val identifier : identifier -> tree
  val application : tree -> tree list -> tree
  val make_record_reads : (Key.t * typ * tree) list -> tree
  type declaration
  val stringify_declaration : declaration -> string
end = struct

  type identifier =
    | Var of string

  let global_reads name =
    Var(Name.lower_camel_case name ^ "Reads")

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

  type tree =
    | Identifier of identifier
    | Application of {
        applied   : tree;
        arguments : tree list;
      }
    | RecordReads of {
        fields : (Key.t * typ * tree) list;
      }

  let identifier ident =
    Identifier(ident)

  let application ot ots =
    Application{ applied = ot; arguments = ots; }

  let make_record_reads fields =
    RecordReads{ fields = fields; }

  type declaration =
    | DefCaseClass of {
        type_name    : type_identifier;
        type_params  : type_parameter list;
        constructors : (string * typ) list;
      }
    | DefVal of {
        val_name : identifier;
        typ      : typ;
        params   : identifier list;
        body     : tree;
      }

  let stringify_declaration _odecl =
    "" (* TODO *)

end


let rec generate_message_type (msg : message) : Output.typ =
  match msg with
  | Variable((_, x)) ->
      Output.type_variable (Output.type_parameter x)

  | Name((_, name), args) ->
      let typs = args |> List.map generate_message_type in
      Output.type_name (Output.type_identifier name) typs

  | Record(_rcd) ->
      failwith "not implemented yet."


let rec generate_message_decoder (msg : message) : Output.tree =
  match msg with
  | Variable((_, x)) ->
      Output.identifier (Output.local_for_parameter x)

  | Name((_, name), args) ->
      let otrees = args |> List.map generate_message_decoder in
      Output.application (Output.identifier (Output.global_reads name)) otrees

  | Record(rcd) ->
      let entries =
        RecordMap.fold (fun key vmsg acc ->
          let otree = generate_message_decoder vmsg in
          let typ = generate_message_type vmsg in
          Alist.extend acc (key, typ, otree)
        ) rcd Alist.empty |> Alist.to_list
      in
      Output.make_record_reads entries


let generate (module_name : string) (package_name : string) (decls : declarations) =
  let odecls : Output.declaration list =
    DeclMap.fold (fun _name def acc ->
      match def.def_main with
      | BuiltIn(_builtin)      -> acc (* TODO *)
      | GivenNormal(_msg)      -> acc (* TODO *)
      | GivenVariant(_variant) -> acc (* TODO *)
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
