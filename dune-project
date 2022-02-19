(lang dune 2.7)

(name kwdcmd)
(version 0.0.1)

(generate_opam_files true)

(source (github shonfeder/kwdcmd))
(license MIT)
(authors "Shon Feder")
(maintainers "shon.feder@gmail.com")

(package
 (name kwdcmd)
 (synopsis "Keywords to Write Command Lines")
 (description "A lightweight and partial porcelain around Cmdliner

Cmdliner is a powerful library for composable command line interfaces, but it is
relatively low level.  Most users have to look up the documentation every time
they want to use the combinators correctly.

This thin wrapper library is best thought of as an executable cookbook for
common Cmdliner usage patterns, or as programmatic documentation encoded in the
type level.

The recipes are divided between module namespaces and the functions are
documented with named arguments.

This library is in early stages, and is growing as the need for patterns arises .")
 (depends
  (dune (> 2))
  (cmdliner (and (>= 1.1.0) (< 1.2.0)))
  (rresult (and (>= 0.7.0) (< 0.8.0)))
  (mdx (and :with-test (>= 1.10.1) (< 1.11.0)))))
