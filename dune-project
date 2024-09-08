(lang dune 3.16)

(generate_opam_files true)

(source
 (github lunaflight/exploding-kittens))

(authors lunaflight)

(maintainers lunaflight)

(package
 (name exploding_kittens)
 (synopsis "Exploding Kittens in OCaml")
 (depends
  (async
   (= "v0.17.0"))
  (core
   (= "v0.17.1"))
  dune
  (ocaml
   (>= 5.1.1))
  (re2
   (= "v0.17.0"))))
