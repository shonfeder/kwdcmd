(lang dune 2.7)

(name kwdcmd)
(version 0.0.1)

(generate_opam_files true)

(source (github shonfeder/kwdcmd))
(license MIT)
(authors "Shon Feder")
(maintainers "shon@informal.systems")

(package
 (name kwdcmd)
 (synopsis "TODO")
 (description "TODO")
 (depends
  (dune (> 2))
  (cmdliner (and (>= 1.0.4) (< 1.1.0)))
  (rresult (and (>= 0.6.0) (< 0.7.0)))))

;; Local Variables:
;; mode: dune
;; End:
