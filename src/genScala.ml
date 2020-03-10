
open Types


module Output : sig
  type identifier
  type tree
  type type_identifier
  type type_parameter
  type typ
  type declaration
  val local_for_parameter : Types.variable -> identifier
  val global_reads : Name.t -> identifier
  val identifier : identifier -> tree
  val application : tree -> tree list -> tree
  val make_record_reads : (Key.t * tree) list -> tree
  val stringify_declaration : declaration -> string
end = struct
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


let generate (package_name : string) (decls : declarations) =
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
      Output.stringify_declaration odecl ^ "\n\n"
    )
  in
  List.append [
    Format.sprintf "package %s\n" package_name;
    "\n";
    "import play.api.libs.json._\n";
    "import play.api.libs.json.Reads._\n";
    "import play.api.libs.functional.syntax._\n";
    "\n";
  ] sdecls |> String.concat ""
