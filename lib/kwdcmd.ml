(** {1 Kwdcmd: Keywords to Write Command Lines}

    Kwdcmd is a lightweight and partial porcelain around
    {{:https://erratique.ch/software/cmdliner} Cmdliner}.

    Cmdliner is a powerful library for composable command line interfaces, but
    it is relatively low level. Use of the combinators without reference to the
    docs requires a deep and thorough knowledge of the library. Kwdcmd wants to
    be able to be used without having to consult the docs.

    This thin wrapper library is best thought of as an executable cookbook for
    common Cmdliner usage patterns, or as programmatic documentation encoded in
    the type level.

    The recipes are divided between module namespaces and detailed with named
    function arguments.

    {2 Ideals}

    - Remain true to the Cmdliners compositional principles (no side-effecting
      shortcuts or other chicanery).
    - Unfamiliar users should be able to write their CLIs within 10 minutes of
      reading the docs.
    - Familiar users should find everything they need to know to write clean,
      expressive CLIS's self-documented in the type signatures of the library's
      modules and functions.

    Please {{:https://github.com/shonfeder/kwdcmd/issues/new} open an issue} or
    {{:https://github.com/shonfeder/kwdcmd/blob/master/CONTRIBUTING.org} add a
    feature} if you think the library could be improved to better meet these
    ideals.

    {2 Usage}

    TODO

    {3 Example}

    {[
      open Kwdcmd

      (** Application configuration *)
      type kind =
        | Bin
        | Lib

      type config =
        { name : string
        ; kind : kind
        }

      (** Application executor *)
      let run : config -> unit = fun _config -> print_endline "TODO"

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
    ]} *)

open Cmdliner

(** {2 Binding operators}

    The beauty of Cmdliner lies in  its its composable, applicative API.  Use of
    this API is made cleaner by means of the binding operators.  Naturally, you
    are not bound to use these.

    The general schema is
    {[
      let+ value_1 = term_1
      and+ value_2 = term_2
      (* ... *)
      and+ value_n = term_n
      in
      program value_1 value_2 (* ... *) value_n
    ]}

    where each [value_i] will be the value obtained by parsing by the the CLI term
    specified in [term_i].

    See the example above for usage. *)

(** [(let+) is Term.(const f $ t)]*)
let ( let+ ) t f = Term.(const f $ t)

(** [(and+) is Term.(const (fun x y -> (x, y)) $ a $ b)]*)
let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)

(**/**)

(** A mid-level wrapper for constructing Cmdliner.Terms.t. Meant for internal use.*)
let add_info arg flags ?docs ?docv ?env ?doc () =
  arg Arg.(info ?docs ?docv ?env ?doc flags)

(**/**)

(** {2 Constructing terms}

    We build our CLIs by defining composable terms that transform CLI args into
    OCaml values. Kwdcmd breaks terms into 2 classes. *)

(** {3 Required terms}

    These must be suplied or the resulting program will fail with an error
    indicating the missing arguments. *)
module Required = struct
  (** [pos docv ~conv ~nth] is a positional argument at the [nth] position,
      giving a value derived by [conv] and named [dcov] in the help page. *)
  let pos docv ~conv ~nth =
    (* TODO find way to eliminate the need for nth *)
    (* Just to avoid name clashing *)
    let conv' = conv in
    add_info
      ~docv
      (fun info' -> Arg.(required & pos nth Arg.(some conv') None & info'))
      []

  let pos_all docv ~conv =
    let conv' = conv in
    add_info ~docv (fun info' -> Arg.(non_empty & pos_all conv' [] & info')) []
end

(** {3 Optional terms} *)
module Optional = struct
  let values docv ~flags ?(default = []) ~conv =
    let conv' = conv in
    add_info
      ~docv
      (fun info' -> Arg.(value & opt_all conv' default & info'))
      flags

  let value docv ~flags ~default ~conv =
    let conv' = conv in
    add_info ~docv (fun info' -> Arg.(value & opt conv' default & info')) flags

  (** A boolean flag set by any of the [flags] *)
  let flag ~flags = add_info (fun info' -> Arg.(value & flag & info')) flags

  (** All the values following from the [nth] position on *)
  let all_from docv ~conv ~nth ?(default = []) =
    let conv' = conv in
    add_info
      ~docv
      (fun info' -> Arg.(value & pos_right nth conv' default & info'))
      []

  let pos docv ~conv ~nth =
    let conv' = conv in
    add_info
      ~docv
      (fun info' -> Arg.(value & pos nth Arg.(some conv') None & info'))
      []

  type 'a choice = 'a * Cmdliner.Arg.info

  (** A choice flag for use with [flag_choices] or [flag_choice] *)
  let c ~doc ~flags : 'option -> 'option choice =
   fun option -> (option, Arg.info flags ~doc)

  (** Choose between any number of the given choice flags, can be repeated
      if none of the choices are provded, then use the default choices  *)
  let flag_choices ~(defaults : 'option list) (options : 'option choice list) =
    Arg.(value & vflag_all defaults options)

  (* TODO  Make exclusive *)

  (** Choose a single one of the given choice flags *)
  let flag_choice ~(default : 'option) (options : 'option choice list) =
    Arg.(value & vflag default options)
end

(** TODO Document with type annotations *)

type 'a cmd = 'a Term.t * Term.info

type 'err err = [> `Msg of string ] as 'err
(** Errors the program configured by the CLI can produce. This does not
    include errors resulting from parsing the CLI. Those are represented by
    [Term.result]. *)

type ('a, 'err) cmd_result = ('a, 'err err) result

(** A subcommand *)
let cmd ?man ~name ~doc : 'a Term.t -> 'a cmd =
 fun term -> (term, Term.info name ~doc ?man)

(** A custom help command *)
let help_cmd ?version ?doc ?sdocs ?exits ?man name =
  let term =
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Term.pure ()))
  in
  let info = Term.info name ?version ?doc ?sdocs ?exits ?man in
  (term, info)

(** {2 Executing CLIs } *)

module type Exec_handler = sig
  val err_handler : _ err -> unit
  (** [err_handler err] handles progra errors. *)

  val exit_handler : (_, _ err) result Term.result -> unit
  (** [exit_hander result] converts the [Term.result] from a CLI
      entrypoint into a suitable exit conditions. *)
end

(** Default exit and error handlers for program execution. *)
module Default_handler = struct
  type 'err err = [> `Msg of string ] as 'err
  (** See {!type:Exec_handler.err}*)

  (** [err_handler err] is the default handler for program execution errors:

      - [(`Msg msg)] results in an exit code of [1] with the [msg] printed to [stderr]
      - Any other [err] produces a failure *)
  let err_handler err : unit =
    match err with
    | `Msg m ->
        Printf.eprintf "error: %s" m;
        exit 1
    | _      -> failwith "Unexpected program error"

  (** [exit_hander result] converts the [Term.result] from a CLI entrypoint
      into a suitable exit conditions. It is the default exit handler for the
      entrypoints in {!module:Exec}.

    - Uncaught exceptions return code [3]
    - Parse errors or term errors ([`Error `Parse | `Error `Term]) return code [2]
    - Execution errors are handled by {!err_handler}. *)
  let exit_handler : 'a Term.result -> unit = function
    | `Error `Exn -> exit 3
    | `Error `Parse
    | `Error `Term ->
        exit 2
    | `Ok (Error err) -> err_handler err
    | _ -> ()
end

(** CLI entrypoints

    All the entrypoionts in {!module-type:Exec} expect toplevel terms that
    evalute to [('a, [> `Msg of string]) result]. *)
module type Exec = sig
  val commands :
       ?help:Format.formatter
    -> ?err:Format.formatter
    -> ?catch:bool
    -> ?env:(string -> string option)
    -> ?argv:string array
    -> ?default:('a, ([> `Msg of string ] as 'b)) result Term.t * Term.info
    -> ?doc:string
    -> ?sdocs:string
    -> ?exits:Term.exit_info list
    -> ?man:Manpage.block list
    -> ?version:string
    -> name:string
    -> (('a, 'b) result Term.t * Term.info) list
    -> unit
  (** Subcommand selector entrypoint.

      Example usage:

      {[
        open Kwdcmd

        let mode = Optional.(flag_choice ~default:"mode-a"
                              [ c ~name:"a" "mode-a" ~doc:"use mode a"
                              ; c ~name:"b" "mode-b" ~doc:"use mode b"
                              ])


        let () = commands ~name:"myprog" ~version:"9.0.0"
            [ ( cmd ~name:"add" ~doc:"add stuff"
                @@ let+ stuff =
                     Required.pos
                       "STUFF"
                       ~conv:Arg.string
                       ~nth:0
                       ~doc:"The stuff to add"
                and+ mode = mode
                in
                adder stuff mode )
            ; ( cmd ~name:"remove" ~doc:"remove things"
                @@ let+ stuff =
                     Required.pos
                       "THINGS"
                       ~conv:Arg.string
                       ~nth:1
                       ~doc:"The stuff to remove"
                and+ mode = mode
                in
                remover stuff mode )
            ]
      ]} *)

  val run :
       ?man:Manpage.block list
    -> name:string
    -> version:string
    -> doc:string
    -> ('a, [> `Msg of string ]) result Term.t
    -> unit
  (** A single cmd entrypoint. *)
end

(** Construct an {!module:Exec} module using the given {!Exec_handler}s *)
module Make_exec (Handler : Exec_handler) : Exec = struct
  let commands
      ?help
      ?err
      ?catch
      ?env
      ?argv
      ?default
      ?doc
      ?sdocs
      ?exits
      ?man
      ?version
      ~name
      (cmds : _ cmd list) =
    let default_cmd =
      match default with
      | Some d -> d
      | None   -> help_cmd ?version ?doc ?sdocs ?exits ?man name
    in
    Term.eval_choice ?help ?err ?catch ?env ?argv default_cmd cmds
    |> Handler.exit_handler

  let run ?man ~name ~version ~doc term =
    let info' = Term.info name ?man ~version ~doc in
    Term.eval (term, info') |> Handler.exit_handler
end

module Exec : Exec = Make_exec (Default_handler)
(** The default CLI executors, derived using the {!Default_handler}s for handling exits
    and errors.

    If your program can produce errors other than those of type [`Msg of string], then
    you should override {!val:Exec_handler.err_handler}. *)

(** {2 Re-exports from cmdliner} *)

(** [const v : 'a)] is a ['a Term.t]: i.e. is a term that evaluates to [v] *)
let const = Term.const

(** [f_term $ a_term] evalutes to [f a]: i.e., apply a functional term to
    argument terms *)
let ( $ ) = Term.( $ )

(** [unit] evalutes to [()], i.e. it is the unit term. Given a function
    [f : unit -> unit]), you can [let term = lift f $ unit] to execuate [f] when
    [term] is evaluated. *)
let unit = Term.pure ()

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage
