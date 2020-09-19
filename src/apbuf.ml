
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
    | GivenExternal(_) -> Format.printf "%a%a@ :=@ (external),@ " Name.pp name pp_params params
  );
  Format.printf "@]@,";
  Format.printf "OUTPUT:@ @[%s@]" s


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
  GenElm.generate module_name decls >>= fun s ->
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
  GenScala.generate object_name package_name decls >>= fun s ->
  let path_out = Filename.concat dir_out (object_name ^ ".scala") in
  Format.printf "writing output on '%s' ...\n" path_out;
  let fout = open_out path_out in
  output_string fout s;
  close_out fout;
  print_endline "done.";
  return ()


let parse_version (s : string) : (version, error) result =
  let open ResultMonad in
  match String.split_on_char '.' s with
  | [s1; s2; s3] ->
      begin
        try
          let n1 = int_of_string s1 in
          let n2 = int_of_string s2 in
          let n3 = int_of_string s3 in
          return (n1, n2, n3)
        with
        | Invalid_argument(_) ->
            error (MalformedVersionString(s))
      end

  | _ ->
      error (MalformedVersionString(s))


let is_compatible_version ~got:((nA1, nA2, nA3) : version) ~providing:((nB1, nB2, nB3) : version) : bool =
  if nA1 = 0 && nB1 = 0 then
    nA2 = nB2 && nA3 <= nB3
  else
    nA1 = nB1 && (nA2 < nB2 || (nA2 = nB2 && nA3 <= nB3))


let validate_meta dir_in (metas : meta_spec list) : ((declarations -> (unit, error) result) list, error) result =
  let open ResultMonad in
  metas |> List.fold_left (fun prev meta ->
    prev >>= fun (version_found, kacc) ->
    match meta with
    | MetaOutput((_, "elm"), pdict) ->
        normalize_dictionary pdict >>= fun rdict ->
        let k = generate_elm dir_in rdict in
        return (version_found, Alist.extend kacc k)

    | MetaOutput((_, "scala"), pdict) ->
        normalize_dictionary pdict >>= fun rdict ->
        let k = generate_scala dir_in rdict in
        return (version_found, Alist.extend kacc k)

    | MetaOutput((_, other), _) ->
        error (UnsupportedTarget{ target = other; })

    | MetaLanguageVersion((_, s)) ->
        parse_version s >>= fun vsn ->
        if is_compatible_version ~got:vsn ~providing:language_version then
          return (true, kacc)
        else
          error (IncompatibleVersion{ providing = language_version; got = vsn })

  ) (return (false, Alist.empty)) >>= fun (version_found, kacc) ->
  if version_found then
    return (Alist.to_list kacc)
  else
    error NoLanguageVersionFound


let output (ks : (declarations -> (unit, error) result) list) (decls : declarations) : (unit, error) result =
  let open ResultMonad in
  ks |> List.fold_left (fun res k ->
    res >>= fun () ->
    k decls
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
    ParserInterface.process path_in >>= fun (metas, pdecls) ->
    validate_meta dir_in metas >>= fun outputks ->
    let pdecls = List.append built_in_declarations pdecls in
    normalize_declarations pdecls >>= fun decls ->
    validate_declarations decls >>= fun () ->
    output outputks decls
  in
  match res with
  | Ok(()) ->
      print_endline "finished."

  | Error(e) ->
      Format.printf "! Error:@ %a@," pp_error e;
      exit 1


(** The spec for the anonimous argument that points to an input. *)
let arg_in =
  Arg.(required (pos 0 (some file) None (info [])))
    (* `Arg.file` asserts that the given path is existent. *)


let () =
  Term.(exit (eval (const main $ arg_in, info "apbuf")))
