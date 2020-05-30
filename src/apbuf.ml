
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


let validate_dictionary ((rngd, pfields) : parsed_dictionary) : (dictionary, error) result =
  let open ResultMonad in
  pfields |> List.fold_left (fun res ((_, k), rv) ->
    res >>= fun dict ->
    if dict |> Dict.mem k then
      error (KeySpecifiedMoreThanOnce{ range = rngd; key = k; })
    else
      return (dict |> Dict.add k rv)
  ) (return Dict.empty) >>= fun dict ->
  return (rngd, dict)


let get_mandatory_value ((rngd, dict) : dictionary) key : (meta_value ranged, error) result =
  let open ResultMonad in
  match dict |> Dict.find_opt key with
  | None     -> error (MandatoryKeyNotFound{ range = rngd; key = key; })
  | Some(rv) -> return rv


let validate_string_value (_rng, mv) : (string, error) result =
  let open ResultMonad in
  match mv with
  | VString(s) -> return s


let get_mandatory_string rdict key =
  let open ResultMonad in
  get_mandatory_value rdict key >>= fun rmv ->
  validate_string_value rmv


let get_dir_out (dir_in : string) (rdict : dictionary) : (string, error) result =
  let open ResultMonad in
  get_mandatory_value rdict "dir" >>= fun rmv ->
  validate_string_value rmv >>= fun dir ->
  let dir_out =
    if Filename.is_relative dir then
      Filename.concat dir_in dir
    else
      dir
  in
  return dir_out


let generate_elm (dir_in : string) (rdict : dictionary) (decls : declarations) : (unit, error) result =
  let open ResultMonad in
  get_dir_out dir_in rdict >>= fun dir_out ->
  get_mandatory_string rdict "module" >>= fun module_name ->
  let s = GenElm.generate module_name decls in
  let path_out = Filename.concat dir_out (module_name ^ ".elm") in
  Format.printf "writing output on '%s' ...\n" path_out;
  let fout = open_out path_out in
  output_string fout s;
  close_out fout;
  print_endline "done.";
  return ()


let generate_scala (dir_in : string) (rdict : dictionary) (decls : declarations) : (unit, error) result =
  let open ResultMonad in
  get_dir_out dir_in rdict >>= fun dir_out ->
  get_mandatory_string rdict "package" >>= fun package_name ->
  get_mandatory_string rdict "object" >>= fun object_name ->
  let s = GenScala.generate object_name package_name decls in
  let path_out = Filename.concat dir_out (object_name ^ ".scala") in
  Format.printf "writing output on '%s' ...\n" path_out;
  let fout = open_out path_out in
  output_string fout s;
  close_out fout;
  print_endline "done.";
  return ()


let output_loop dir_in (metas : meta_spec list) (decls : declarations) =
  let open ResultMonad in
  metas |> List.fold_left (fun prev meta ->
    prev >>= fun () ->
    match meta with
    | MetaOutput((_, "elm"), pdict) ->
        validate_dictionary pdict >>= fun rdict ->
        validate_declarations decls >>= fun () ->
        generate_elm dir_in rdict decls

    | MetaOutput((_, "scala"), pdict) ->
        validate_dictionary pdict >>= fun rdict ->
        validate_declarations decls >>= fun () ->
        generate_scala dir_in rdict decls

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
      Format.printf "! Error:@ %a" pp_error e;
      exit 1


(** The spec for the anonimous argument that points to an input. *)
let arg_in =
  Arg.(required (pos 0 (some file) None (info [])))
    (* `Arg.file` asserts that the given path is existent. *)


let () =
  Term.(exit (eval (const main $ arg_in, info "apbuf")))
