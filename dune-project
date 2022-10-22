(lang dune 3.5)
(name bf-reusable)
(using menhir 2.1)

(generate_opam_files true)

(maintainers "roodni")
(authors "roodni")
(homepage "https://github.com/roodni/bf-reusable")
(bug_reports "https://github.com/roodni/bf-reusable/issues")

(package
 (name bf-reusable)
 (synopsis "A programming language to generate brainf*ck code")
 (depends
  (ocaml (>= 4.12))
  (ounit2 :with-test)
  menhir fileutils))