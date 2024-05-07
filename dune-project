(lang dune 3.12)

(name lice)

(using menhir 2.1)

(using directory-targets 0.1)

(license "Apache-2.0")

(maintainers "Gabriel Santamaria <gaby.santamaria@outlook.fr>")

(authors "Gabriel Santamaria <gaby.santamaria@outlook.fr>")

(source
 (github gabyfle/lice))

(generate_opam_files true)

(package
 (name lice)
 (synopsis "The Lice Programming language libraries and interpreter")
 (homepage "https://lice.gabyfle.dev/")
 (depends
  (ocaml
   (>= 4.08.0))
  (ANSITerminal
   (>= 0.8.5)))
 (description
  "This package provides the Lice programming language libraries and interpreter. Lice is a simple programming language that is easy to learn and use. It is an imperative programming language that is meant to be interoperable with OCaml."))
