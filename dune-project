(lang dune 3.13)

(using menhir 3.0)

(name stella_typechecker)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name stella_typechecker)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  base-unix
  base
  stdio
  menhir
  cmdliner
  fmt
  logs
  fpath
  ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
