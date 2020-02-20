
open Cmdliner
open Types


let rec normalize_message (pmsg : parsed_message) : (message, error) result =
  let open ResultMonad in
  match pmsg with
  | PName(name) ->
      return (Name(name))

  | PRecord(prcd) ->
      prcd |> List.fold_left (fun res (key, pmsgsub) ->
        res >>= fun rcdmap ->
        if rcdmap |> RecordMap.mem key then
          error (FieldDefinedMoreThanOnce{ key = key; })
        else
          normalize_message pmsgsub >>= fun msgsub ->
          return (rcdmap |> RecordMap.add key msgsub)
      ) (return RecordMap.empty) >>= fun rcdmap ->
      return (Record(rcdmap))

  | PVariant(pvariant) ->
      pvariant |> List.fold_left (fun res (ctor, pmsgsubopt) ->
        res >>= fun variantmap ->
        if variantmap |> VariantMap.mem ctor then
          error (ConstructorDefinedMoreThanOnce{ constructor = ctor; })
        else
          begin
            match pmsgsubopt with
            | None          -> return None
            | Some(pmsgsub) -> normalize_message pmsgsub >>= fun msgsub -> return (Some(msgsub))
          end >>= fun msgsubopt ->
          return (variantmap |> VariantMap.add ctor msgsubopt)
      ) (return VariantMap.empty) >>= fun variantmap ->
      return (Variant(variantmap))


let normalize_declarations (pdecls : parsed_declarations) : (declarations, error) result =
  let open ResultMonad in
  pdecls |> List.fold_left (fun res (name, pmsg) ->
    res >>= fun declmap ->
    if declmap |> DeclMap.mem name then
      error (MessageNameDefinedMoreThanOnce{ name = name; })
    else
      normalize_message pmsg >>= fun msg ->
      return (declmap |> DeclMap.add name msg)
  ) (return DeclMap.empty)


(** This is the core part of the program.
    The parameter [s] stands for an input file path already confirmed to be existent. *)
let main s =
  let fin = open_in s in
  let pdecls = Parser.toplevel Lexer.token (Lexing.from_channel fin) in
  match normalize_declarations pdecls with
  | Ok(decls) ->
      Format.printf "@[";
      decls |> DeclMap.iter (fun name msg ->
        Format.printf "%s@ :=@ @[%a@],@ " name pp_message msg
      );
      Format.printf "@]"

  | Error(e) ->
      Format.printf "! Error:@ %a" pp_error e


(** The spec for the anonimous argument that points to an input. *)
let arg_in =
  Arg.(required (pos 0 (some file) None (info [])))


let () =
  Term.(exit (eval (const main $ arg_in, info "apbuf")))
