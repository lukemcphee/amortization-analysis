(lang dune 3.11)

(name mortgage)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mortgage)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune printbox printbox-text owl owl-plplot ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
