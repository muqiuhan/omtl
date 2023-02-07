(lang dune 3.0)

(name omtl)

(generate_opam_files true)

(source
 (github muqiuhan/omtl))

(authors "Muqiu Han")

(maintainers "Muqiu Han")

(license MIT)

(documentation https://github.com/omtl)

(package
 (allow_empty)
 (name omtl)
 (synopsis "OCaml Minimalist Testing Library")
 (description "OCaml Minimalist Testing Library")
 (depends
  (ocaml
   (>= 4.13))
  dune)
 (tags
  (testing "test framework")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
