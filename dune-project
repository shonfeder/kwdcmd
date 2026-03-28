(lang dune 3.22)

(name kwdcmd)

(version 0.0.1)

(generate_opam_files true)

(source
 (github shonfeder/kwdcmd))

(license MIT)

(authors "Shon Feder")

(maintainers "shon.feder@gmail.com")

(package
 (name kwdcmd)
 (synopsis "Keywords to Write Command Lines")
 (description
  "A lightweight and partial porcelain around Cmdliner\n\nCmdliner is a powerful library for composable command line interfaces, but it is\nrelatively low level.  Most users have to look up the documentation every time\nthey want to use the combinators correctly.\n\nThis thin wrapper library is best thought of as an executable cookbook for\ncommon Cmdliner usage patterns, or as programmatic documentation encoded in the\ntype level.\n\nThe recipes are divided between module namespaces and the functions are\ndocumented with named arguments.\n\nThis library is in early stages, and is growing as the need for patterns arises .")
 (depends
  (dune
   (>= 3.22))
  (cmdliner
   (>= 2.0.0))
  (mdx
   (and
    :with-test
    (>= 2.5.0)))
  (fpath :with-test)))
