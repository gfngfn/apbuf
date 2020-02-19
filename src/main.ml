
open Cmdliner


let print s =
  print_endline ("Hello, world! " ^ s)


let arg_in =
  Arg.(required & pos 0 (some file) None (info []))


let () =
  Term.(exit (eval (const print $ arg_in, info "hello-world")))
