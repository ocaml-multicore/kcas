(lang dune 3.14)

(name kcas)

(generate_opam_files true)

(implicit_transitive_deps false)

(authors
 "KC Sivaramakrishnan <kc@tarides.com>"
 "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(maintainers
 "Vesa Karvonen <vesa.a.j.k@gmail.com>"
 "KC Sivaramakrishnan <kc@tarides.com>")

(source
 (github ocaml-multicore/kcas))

(homepage "https://github.com/ocaml-multicore/kcas")

(license ISC)

(using mdx 0.4)

(package
 (name kcas)
 (synopsis
  "Software transactional memory based on lock-free multi-word compare-and-set")
 (description
  "A software transactional memory (STM) implementation based on an atomic lock-free multi-word compare-and-set (MCAS) algorithm enhanced with read-only compare operations and ability to block awaiting for changes.")
 (depends
  (ocaml
   (>= 4.13.0))
  (backoff
   (>= 0.1.0))
  (domain-local-await
   (>= 1.0.1))
  (domain-local-timeout
   (>= 1.0.1))
  (multicore-magic
   (>= 2.1.0))
  (domain_shims
   (and
    (>= 0.1.0)
    :with-test))
  (alcotest
   (and
    (>= 1.7.0)
    :with-test))
  (mdx
   (and
    (>= 2.3.0)
    :with-test))
  (sherlodoc
   (and
    (>= 0.2)
    :with-doc))
  (odoc
   (and
    (>= 2.4.1)
    :with-doc))))

(package
 (name kcas_data)
 (synopsis
  "Compositional lock-free data structures and primitives for communication and synchronization")
 (description
  "A library of compositional lock-free data structures and primitives for communication and synchronization implemented using kcas.")
 (depends
  (kcas
   (= :version))
  (multicore-magic
   (>= 2.1.0))
  (backoff
   (and
    (>= 0.1.0)
    :with-test))
  (domain-local-await
   (and
    (>= 1.0.1)
    :with-test))
  (domain_shims
   (and
    (>= 0.1.0)
    :with-test))
  (multicore-bench
   (and
    (>= 0.1.2)
    :with-test))
  (alcotest
   (and
    (>= 1.7.0)
    :with-test))
  (qcheck-core
   (and
    (>= 0.21.2)
    :with-test))
  (qcheck-stm
   (and
    (>= 0.3)
    :with-test))
  (mdx
   (and
    (>= 2.3.0)
    :with-test))
  (sherlodoc
   (and
    (>= 0.2)
    :with-doc))
  (odoc
   (and
    (>= 2.4.1)
    :with-doc))))
