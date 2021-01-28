(** {1} KwdCmd: Keywords to Write Command Lines

    Partial porcelain around {{:https://erratique.ch/software/cmdliner}
    Cmdliner}. Cmdliner is a powerful library for composable command line
    interfaces, but it is relatively low level, and I always have to look up the
    documentation to use the combinators correctly.

    This thin wrapper library is just an executable cookbook for common usage
    patterns of Cmdliner.

    The recipes are divided between module namespaces and detailed with named
    function arguments.

    The intended usage of this library is to

    {[ open KwdCmd ]}

    and configure the toplevel commands for your application

    {[ let doc = "A program the shows how to use KwdCmd."

    let cmds = [ (* TODO *) ]

    let () = Exec.select ~name:"Program Name" ~version:"0.1.0" ~doc Required
    cmds ]}

    **)

open Cmdliner

let add_info arg flags ?docs ?docv ?env ?doc () =
  arg Arg.(info ?docs ?docv ?env ?doc flags)

module Required = struct
  (* TODO find way to eliminate the need for nth *)
  let pos docv ~conv ~nth =
    (* Just to avoid name clashing *)
    let conv' = conv in
    add_info
      ~docv
      (fun info' -> Arg.(required & pos nth Arg.(some conv') None & info'))
      []
end

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

  (** Choose between one of the given choice flags *)
  let flag_choice ~(default : 'option) (options : 'option choice list) =
    Arg.(last & vflag_all [ default ] options)
end

(** TODO Document with type annotations *)

type 'a cmd = 'a Term.t * Term.info

let cmd ?man ~name ~doc : 'a Term.t -> 'a cmd =
 fun term -> (term, Term.info name ~doc ?man)

(** Construct CLI entrypoints *)
module Exec = struct
  let help_cmd ?version ?doc ?sdocs ?exits ?man name =
    let term =
      Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Term.pure ()))
    in
    let info = Term.info name ?version ?doc ?sdocs ?exits ?man in
    (term, info)

  (** Subcommands *)
  let select ?default ?doc ?sdocs ?exits ?man ?version ~name cmds =
    let default_cmd =
      match default with
      | Some d -> d
      | None   -> help_cmd ?version ?doc ?sdocs ?exits ?man name
    in
    Term.eval_choice default_cmd cmds

  (** A single cmd sequence *)
  let run ~name ~version ~doc term =
    let info' = Term.info name ~version ~doc in
    Term.(exit @@ eval (term, info'))
end

(* Re-exports *)

module Arg = Cmdliner.Arg

(** A constant value used to apply function to term arguments. *)
let const = Term.const

(** Apply a function to term arguments. *)
let ( $ ) = Term.( $ )

(** The unit term argument. *)
let unit = Term.pure ()

let ( let+ ) t f = Term.(const f $ t)

let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)
