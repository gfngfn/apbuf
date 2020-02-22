
open Cmdliner
open Types


let pp decls s =
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
    | BuiltIn(_)       -> Format.printf "%s%a (built-in),@ " name pp_params params
    | GivenNormal(msg) -> Format.printf "%s%a@ :=@ @[%a@],@ " name pp_params params pp_message msg
    | GivenVariant(v)  -> Format.printf "%s%a@ :=@ @[%a@],@ " name pp_params params (pp_variant_map pp_message) v
  );
  Format.printf "@]@,";
  Format.printf "OUTPUT:@ @[%s@]" s


(** This is the core part of the program.
    The parameter [path_in] stands for an input file path already confirmed to be existent. *)
let main path_in =
  let path_in =
    if Filename.is_relative path_in then
      Filename.concat (Sys.getcwd ()) path_in
    else
      path_in
  in
  let dir_in = Filename.dirname path_in in
  let fin = open_in path_in in
  let res =
    let open ResultMonad in
    let (meta, pdecls) = Parser.toplevel Lexer.token (Lexing.from_channel fin) in
      (* TODO: handle parse error *)
    close_in fin;
    match meta with
    | MetaOutput("elm", dir) ->
        let dir_out =
          if Filename.is_relative dir then
            Filename.concat dir_in dir
          else
            dir
        in
        let pdecls = List.append built_in_declarations pdecls in
        normalize_declarations pdecls >>= fun decls ->
        validate_declarations decls >>= fun () ->
        let module_name = "APBufGen" in
        let s = GenElm.generate_decoder module_name decls in
        return (Filename.concat dir_out (module_name ^ ".elm"), s)

    | _ ->
        failwith "not supported."
  in
  match res with
  | Ok((path_out, s)) ->
      Format.printf "output written on '%s' ...\n" path_out;
      let fout = open_out path_out in
      output_string fout s;
      close_out fout;
      print_endline "done."

  | Error(e) ->
      Format.printf "! Error:@ %a" pp_error e


(** The spec for the anonimous argument that points to an input. *)
let arg_in =
  Arg.(required (pos 0 (some file) None (info [])))
    (* `Arg.file` asserts that the given path is existent. *)


let () =
  Term.(exit (eval (const main $ arg_in, info "apbuf")))
