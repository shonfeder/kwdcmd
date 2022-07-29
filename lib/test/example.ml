(* Example implementing the configuration for a command line rm

   Adapted from the Cmdliner documentatin  https://erratique.ch/software/cmdliner/doc/examples.html *)

(* Program stub *)

type prompt =
  | Always
  | Once
  | Never

let prompt_str = function
  | Always -> "always"
  | Once   -> "once"
  | Never  -> "never"

let rm prompt recurse files =
  Printf.printf
    "prompt = %s\nrecurse = %B\nfiles = %s\n"
    (prompt_str prompt)
    recurse
    (String.concat ", " files);
  Ok ()

(* Command line interface *)

open Kwdcmd

let man =
  [ `S Manpage.s_description
  ; `P
      "$(tname) removes each specified $(i,FILE). By default it does not \
       remove directories, to also remove them and their contents, use the \
       option $(b,--recursive) ($(b,-r) or $(b,-R))."
  ; `P
      "To remove a file whose name starts with a $(b,-), for example \
       $(b,-foo), use one of these commands:"
  ; `Pre "$(mname) $(b,-- -foo)"
  ; `Noblank
  ; `Pre "$(mname) $(b,./-foo)"
  ; `P "$(tname) removes symbolic links, not the files referenced by the links."
  ; `S Manpage.s_bugs
  ; `P "Report bugs to <bugs@example.org>."
  ; `S Manpage.s_see_also
  ; `P "$(b,rmdir)(1), $(b,unlink)(2)"
  ]

let () =
  Exec.run
    ~name:"rm"
    ~version:"%%VERSION%%"
    ~doc:"Remove files or directories"
    ~man
  @@ let+ files = Required.pos_all "FILE" ~conv:Arg.file ()
     and+ prompt =
       Optional.(
         flag_choice
           ~default:Always
           [ c ~doc:"Prompt before every removal." ~flags:[ "i" ] Always
           ; c
               ~doc:"Ignore nonexistent files and never prompt."
               ~flags:[ "f" ]
               Never
           ; c
               ~doc:
                 "Prompt once before removing more than three files, or when \
                  removing recursively."
               ~flags:[ "I" ]
               Once
           ])
     and+ recurse =
       Optional.flag
         ~doc:"Remove directories and their contents recursively."
         ~flags:[ "r"; "R"; "recursive" ]
         ()
     in
     rm prompt recurse files
