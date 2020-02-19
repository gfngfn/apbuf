
open Cmdliner


let print () =
  print_endline "Hello, world!"


let () =
  Term.(exit (eval (const print $ const (), info "hello-world")))
