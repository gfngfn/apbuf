
open Types


module Output : sig
  type identifier
  type tree
  val local_for_parameter : Types.variable -> identifier
  val global_reads : Name.t -> identifier
  val identifier : identifier -> tree
  val application : tree -> tree list -> tree
  val make_record_reads : (Key.t * tree) list -> tree
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


let generate (decls : declarations) =
  ()
