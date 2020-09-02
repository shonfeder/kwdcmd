(** A wrapper around Cmdliner *)

open Cmdliner

let add_info arg flags ?docs ?docv ?env ?doc () =
  arg Arg.(info ?docs ?docv ?env ?doc flags)

module Required = struct
  (* TODO find way to eliminate the need for nth *)
  let pos docv ~conv ~nth =
    (* Just to avoid name clashing *)
    let conv' = conv in
    add_info ~docv
      (fun info' -> Arg.(required & pos nth Arg.(some conv') None & info'))
      []
end

module Optional = struct
  let values docv ~flags ?(default = []) ~conv =
    let conv' = conv in
    add_info ~docv
      (fun info' -> Arg.(value & opt_all conv' default & info'))
      flags

  let value docv ~flags ~default ~conv =
    let conv' = conv in
    add_info ~docv (fun info' -> Arg.(value & opt conv' default & info')) flags

  let flag ~flags = add_info (fun info' -> Arg.(value & flag & info')) flags

  let all_from docv ~conv ~nth ?(default = []) =
    let conv' = conv in
    add_info ~docv
      (fun info' -> Arg.(value & pos_right nth conv' default & info'))
      []
end

let cmd ?man ~name ~doc term = (Term.term_result term, Term.info name ~doc ?man)

let help_cmd ?version ?doc ?sdocs ?exits ?man name =
  let term =
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Term.pure ()))
  in
  let info = Term.info name ?version ?doc ?sdocs ?exits ?man in
  (term, info)

let select ?default ?doc ?sdocs ?exits ?man ?version ~name cmds =
  let default_cmd =
    match default with
    | Some d -> d
    | None   -> help_cmd ?version ?doc ?sdocs ?exits ?man name
  in
  Term.(exit @@ eval_choice default_cmd cmds)

(* TODO Support all of ~Term.info args *)
let exec ~name ~version ~doc term =
  let info' = Term.info name ~version ~doc in
  Term.(exit @@ eval (term, info'))

(* Re-exports *)

let ( $ ) = Term.( $ )

let const = Term.const

let unit = Term.pure ()
