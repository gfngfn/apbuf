(lang dune 2.5)
(using menhir 2.1)
(name apbuf)
(version 0.0.2)

(generate_opam_files true)

(source (github gfngfn/APBuf))
(authors "gfngfn")
(maintainers "gfngfn")

(package
 (name apbuf)
 (synopsis "Algebraic Protocol Buffers")
 (description "Algebraic Protocol Buffers")
 (depends
  (dune (>= 2.5.1))
  (ppx_deriving (>= 1.1.0))
  ))
