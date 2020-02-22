
open Cmdliner
open Types


(** This is the core part of the program.
    The parameter [s] stands for an input file path already confirmed to be existent. *)
let main s =
  let fin = open_in s in
  let res =
    let open ResultMonad in
    let pdecls = Parser.toplevel Lexer.token (Lexing.from_channel fin) in
      (* TODO: handle parse error *)
    let pdecls = List.append built_in_declarations pdecls in
    normalize_declarations pdecls >>= fun decls ->
    validate_declarations decls >>= fun () ->
    return decls
  in
  match res with
  | Ok(decls) ->
      let pp_params ppf params =
        let s =
          match params with
          | []     -> ""
          | _ :: _ -> "(" ^ (params |> String.concat ", ") ^ ")"
        in
        Format.fprintf ppf "%s" s
      in
      Format.printf "@[";
      decls |> DeclMap.iter (fun name def ->
        let params = def.def_params in
        match def.def_main with
        | BuiltIn(_) -> Format.printf "%s%a (built-in),@ " name pp_params params
        | Given(msg) -> Format.printf "%s%a@ :=@ @[%a@],@ " name pp_params params pp_message msg
      );
      Format.printf "@]"

  | Error(e) ->
      Format.printf "! Error:@ %a" pp_error e


(** The spec for the anonimous argument that points to an input. *)
let arg_in =
  Arg.(required (pos 0 (some file) None (info [])))
    (* `Arg.file` asserts that the given path is existent. *)


let () =
  Term.(exit (eval (const main $ arg_in, info "apbuf")))
