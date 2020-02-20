
open Cmdliner
open Types


let print s =
  let fin = open_in s in
  let decls = Parser.toplevel Lexer.token (Lexing.from_channel fin) in
  Format.printf "@[";
  decls |> List.iter (fun (name, msg) ->
    Format.printf "%s := %a@ " name pp_parsed_message msg
  );
  Format.printf "@]"


let arg_in =
  Arg.(required & pos 0 (some file) None (info []))


let () =
  Term.(exit (eval (const print $ arg_in, info "hello-world")))
