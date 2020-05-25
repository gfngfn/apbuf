
open Cmdliner
open Types


let pp decls s =
  let pp_params ppf params =
    let xs = params |> List.map snd |> List.map Variable.to_snake_case in
    let s =
      match xs with
      | []     -> ""
      | _ :: _ -> "(" ^ (xs |> String.concat ", ") ^ ")"
    in
    Format.fprintf ppf "%s" s
  in
  Format.printf "@[";
  decls |> DeclMap.iter (fun name def ->
    let params = def.def_params in
    match def.def_main with
    | BuiltIn(_)       -> Format.printf "%a%a (built-in),@ " Name.pp name pp_params params
    | GivenNormal(msg) -> Format.printf "%a%a@ :=@ @[%a@],@ " Name.pp name pp_params params pp_message msg
    | GivenVariant(v)  -> Format.printf "%a%a@ :=@ @[%a@],@ " Name.pp name pp_params params (pp_variant_map pp_message) v
    | GivenRecord(r)   -> Format.printf "%a%a@ :=@ @[%a@],@ " Name.pp name pp_params params (pp_record_map pp_message) r
  );
  Format.printf "@]@,";
  Format.printf "OUTPUT:@ @[%s@]" s


let generate_elm (dir_out : string) (decls : declarations) : unit =
  let module_name = "APBufGen" in
  let s = GenElm.generate module_name decls in
  let path_out = Filename.concat dir_out (module_name ^ ".elm") in
  Format.printf "writing output on '%s' ...\n" path_out;
  let fout = open_out path_out in
  output_string fout s;
  close_out fout;
  print_endline "done."


let generate_scala (dir_out : string) (decls : declarations) : unit =
  let module_name = "APBufGen" in
  let package_name = "apbufgen" in
  let s = GenScala.generate module_name package_name decls in
  let path_out = Filename.concat dir_out (module_name ^ ".scala") in
  Format.printf "writing output on '%s' ...\n" path_out;
  let fout = open_out path_out in
  output_string fout s;
  close_out fout;
  print_endline "done."


let output_loop dir_in (metas : meta_spec list) (decls : declarations) =
  let open ResultMonad in
  metas |> List.fold_left (fun prev meta ->
    prev >>= fun () ->
    match meta with
    | MetaOutput((_, "elm"), (_, dir)) ->
        validate_declarations decls >>= fun () ->
        let dir_out =
          if Filename.is_relative dir then
            Filename.concat dir_in dir
          else
            dir
        in
        generate_elm dir_out decls;
        return ()

    | MetaOutput((_, "scala"), (_, dir)) ->
        validate_declarations decls >>= fun () ->
        let dir_out =
          if Filename.is_relative dir then
            Filename.concat dir_in dir
          else
            dir
        in
        generate_scala dir_out decls;
        return ()

    | MetaOutput((_, other), _) ->
        error (UnsupportedTarget{ target = other; })
  ) (return ())


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
  let res =
    let open ResultMonad in
    ParserInterface.process path_in >>= fun (meta, pdecls) ->
    let pdecls = List.append built_in_declarations pdecls in
    normalize_declarations pdecls >>= fun decls ->
    output_loop dir_in meta decls
  in
  match res with
  | Ok(()) ->
      print_endline "finished."

  | Error(e) ->
      Format.printf "! Error:@ %a" pp_error e


(** The spec for the anonimous argument that points to an input. *)
let arg_in =
  Arg.(required (pos 0 (some file) None (info [])))
    (* `Arg.file` asserts that the given path is existent. *)


let () =
  Term.(exit (eval (const main $ arg_in, info "apbuf")))
