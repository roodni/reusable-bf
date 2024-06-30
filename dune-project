(lang dune 3.15)
(name reusable-bf)
(using menhir 3.0)

(generate_opam_files true)

(maintainers "roodni")
(authors "roodni")
(source (github roodni/reusable-bf))

(package
 (name reusable-bf)
 (synopsis "A reusable meta language of brainf*ck")
 (depends
  (ocaml (>= 4.14))
  (ounit2 :with-test)
  menhir
  js_of_ocaml
  js_of_ocaml-ppx_deriving_json
  fileutils))