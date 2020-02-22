
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
    normalize_declarations pdecls >>= fun decls ->
    validate_declarations decls >>= fun () ->
    return decls
  in
  match res with
  | Ok(decls) ->
      Format.printf "@[";
      decls |> DeclMap.iter (fun name def ->
        match def with
        | BuiltIn(_) ->
            Format.printf "%s (built-in),@ " name

        | Given(msg) ->
            Format.printf "%s@ :=@ @[%a@],@ " name pp_message msg
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
