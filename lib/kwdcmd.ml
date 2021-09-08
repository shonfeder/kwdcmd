(** {1 KwdCmd: Keywords to Write Command Lines}

    A lightweight and partial porcelain around
    {{:https://erratique.ch/software/cmdliner} Cmdliner}. Cmdliner is a powerful
    library for composable command line interfaces, but it is relatively low
    level. Most users have to look up the documentation every time they want to
    use the combinators correctly.

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

    The beauty of Cmdliner lies in  its its applicative and composable API.
    Using this API is made much cleaner by means of binding operators.
    Naturally, you are not bound to use these. See the example above for usage. *)

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

    These must be suplied or the resulting program will fail with error
    indicating the missing arguments. *)
module Required = struct
  (** [pos docv ~conv ~nth] is a positional argument at the the [nth] position,
      giving a value derived by [conv] and named [dcov] in the help page. *)
  let pos docv ~conv ~nth =
    (* TODO find way to eliminate the need for nth *)
    (* Just to avoid name clashing *)
    let conv' = conv in
    add_info
      ~docv
      (fun info' -> Arg.(required & pos nth Arg.(some conv') None & info'))
      []
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

  (** A choice flag *)
  let c ~doc ~name : 'option -> 'option choice =
   fun option -> (option, Arg.info [ name ] ~doc)

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

(** Construct CLI entrypoints

    All the entrypoionts in {! Exec} expect toplevel terms that
    evalute to [('a, [> `Msg of string]) result]. Any [Error (`Msg msg)]
    results in an exit codes of [1] with the [msg] printed to [stderr].

    Aditionally:

    - Uncaught execptions return code [3]
    - Parse errors or term errors return code [2] *)
module Exec = struct
  (** [exit_hander result] converts Term.result values to suitable exit
      conditions. *)
  let exit_handler = function
    | `Error `Exn -> exit 3
    | `Error `Parse
    | `Error `Term ->
        exit 2
    | `Ok (Error (`Msg m)) ->
        Printf.eprintf "error: %s" m;
        exit 1
    | _ -> ()

  (* TODO Consider making the exit handlnig optional?  *)

  (** Subcommand selector entrypoint.

      Example usage:

      {[
        open Kwdcmd

        let mode = Optional.(flag_choice ~default:"mode-a"
                              [ c ~name:"a" "mode-a" ~doc:"use mode a"
                              ; c ~name:"b" "mode-b" ~doc:"use mode b"
                              ])


        let () = select ~name:"myprog" ~version:"9.0.0"
           [ cmd ~name:"add" ~doc:"add stuff"
             @@ let* stuff = Required.pos "STUFF" ~conv:Arg.string ~nth:0 ~doc:"The stuff to add" in
                let+ mode = mode
                in adder stuff mode
           ; cmd ~name:"remove" ~doc:"remove things"
             @@ let* stuff = Required.pos "THINGS" ~conv:Arg.string ~nth:0 ~doc:"The stuff to remove" in
                let+ mode = mode
                in remover stuff mode
           ]
      ]} *)
  let select
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
    |> exit_handler

  (** A single cmd entrypoint. *)
  let run ~name ~version ~doc term =
    let info' = Term.info name ~version ~doc in
    Term.eval (term, info') |> exit_handler
end

(** {2 Re-exports from cmdliner} *)

(** [const (v : 'a)] is a ['a Term.t]: i.e. is a term that evaluates to [v] *)
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
