# Table of Contents

1.  [Ideals](#org3bc9ff2)
2.  [Installation](#org227e5ab)
3.  [Examples](#org15f8ac9)
    1.  [Single commands](#org13fc160)
    2.  [Sub commands](#orge1334f9)

*A lightweight and partial porcelain around [Cmdliner](https://erratique.ch/software/cmdliner} )*

Cmdliner is a powerful library for composable command line interfaces, but it is
relatively low level.  Most users have to look up the documentation every time
they want to use the combinators correctly.

This thin wrapper library is best thought of as an executable cookbook for
common Cmdliner usage patterns, or as programmatic documentation encoded in the
type level.

The recipes are divided between module namespaces and the functions are
documented with named arguments.

This library is in early stages, and is growing as the need for patterns arises .


<a id="org3bc9ff2"></a>

# Ideals

-   Remain true to Cmdliner&rsquo;s compositional principles (no side-effecting
    shortcuts or other chicanery).
-   Unfamiliar users should be able to write their CLIs within 10 minutes of
    reading the docs.
-   Familiar users should find everything they need to know to write clean,
    expressive CLI&rsquo;s self-documented in the type signatures of the library&rsquo;s
    modules and functions.

Please [open an issue](https://github.com/shonfeder/kwdcmd/issues/new ) or [add a feature](https://github.com/shonfeder/kwdcmd/blob/master/CONTRIBUTING.org) if you think the library could be improved
to better meet these ideals.


<a id="org227e5ab"></a>

# Installation

Pin to master with [opam](https://opam.ocaml.org/doc/Install.html) via

    opam pin https://github.com/shonfeder/kwdcmd.git

Or append `#0.0.n` to the URL to pin a specific release.


<a id="org15f8ac9"></a>

# Examples


<a id="org13fc160"></a>

## Single commands

An interface for a simple project generation tool:

``` ocaml
open Kwdcmd

(** Application configuration *)
type kind =
    | Bin
    | Lib

type config =
    { name : string
    ; kind : kind
    }

(** Program configure via the CLI *)
let run = fun _config -> Ok ()

(** CLI entrypoint *)
let () =
    Exec.run ~name:"My application" ~version:"0.0.1" ~doc:"project generator"
    @@ let+ name =
        Required.pos
            "NAME"
            ~conv:Arg.string
            ~nth:0
            ~doc:"The name of the new project"
            ()
        and+ kind =
        Optional.(
            flag_choice
            ~default:Bin
            [ c ~name:"bin" Bin ~doc:"create an executable binary"
            ; c ~name:"lib" Lib ~doc:"create a library"
            ])
        in
        run { name; kind }
```



<a id="orge1334f9"></a>

## Sub commands

A sub-command interface to a simple utility for looking up emoji:

``` ocaml
module Example_cli (Progn : sig 
    val lookup_name : string -> (unit, _ err) cmd_result
    val lookup_unicode : string -> (unit, _ err) cmd_result
    val emojify : Fpath.t -> (unit, _ err) cmd_result
  end ) 
  = struct
    open Kwdcmd

    let () =
        Exec.commands
        ~name:"emojitsu"
        ~version:"0.0.1"
        ~doc:"Techniques for dealing with emoji"
        [ ( cmd
                ~name:"find-name"
                ~doc:"Find the name of an emoji given its unicode"
            @@ let+ unicode = Required.pos "UNICODE" ~conv:Arg.string ~nth:0 () in
                Progn.lookup_name unicode )
        ; ( cmd
                ~name:"find-unicode"
                ~doc:"Find the unicode of an emoji given its name"
            @@ let+ name = Required.pos "EMOJI_NAME" ~conv:Arg.string ~nth:0 () in
                Progn.lookup_unicode name )
        ; ( cmd
                ~name:"emojify"
                ~doc:
                "Replace all names of the form :emoji_name: with the corresponding \
                    unicode in the given file"
            @@ let+ name =
                Required.pos
                    "FILE"
                    ~conv:Arg.(conv (Fpath.of_string, Fpath.pp))
                    ~nth:0
                    ()
                in
                Progn.emojify name )
        ]
end
```
