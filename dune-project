(lang dune 3.15)
(name bf-reusable)
(using menhir 3.0)

(generate_opam_files true)

(maintainers "roodni")
(authors "roodni")
(source (github roodni/bf-reusable))

(package
 (name bf-reusable)
 (synopsis "A programming language to generate brainf*ck code")
 (depends
  (ocaml (>= 4.14))
  (ounit2 :with-test)
  menhir
  js_of_ocaml
  fileutils))