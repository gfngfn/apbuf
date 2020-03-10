
open Types


module Output : sig
  type identifier
  val local_for_parameter : Types.variable -> identifier
  val global_reads : Name.t -> identifier
  type tree
  val identifier : identifier -> tree
  val application : tree -> tree list -> tree
  val make_record_reads : (Key.t * tree) list -> tree
  type type_identifier
  type type_parameter
  type typ
  type declaration
  val stringify_declaration : declaration -> string
end = struct

  type identifier =
    | Var of string

  let global_reads name =
    Var(Name.lower_camel_case name ^ "Reads")

  let local_for_parameter x =
    Var("local_param_" ^ x)

  type tree =
    | Identifier of identifier
    | Application of {
        applied   : tree;
        arguments : tree list;
      }
    | RecordReads of {
        fields : (Key.t * tree) list;
      }

  let identifier ident =
    Identifier(ident)

  let application ot ots =
    Application{ applied = ot; arguments = ots; }

  let make_record_reads fields =
    RecordReads{ fields = fields; }

  type type_identifier = unit

  type type_parameter = unit

  type typ =
    | TypeName     of type_identifier * typ list
    | TypeVariable of type_parameter
    | FuncType     of typ * typ

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
          Alist.extend acc (key, otree)
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
