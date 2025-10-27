(lang dune 3.20)

(name fp-openAddress-Hashmap-dict)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name fp-openAddress-Hashmap-dict)
 (synopsis "Open addressing hashmap implementation in OCaml")
 (description "A functional implementation of a dictionary using open addressing hash table")
 (depends 
  ocaml
  dune
  (alcotest :with-test))
 (tags
  ("hashmap" "dictionary" "functional" "data-structures")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
