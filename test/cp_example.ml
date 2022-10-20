(* Implementation, we check the dest argument and print the args *)

let cp verbose recurse force srcs dest =
  let many = List.length srcs > 1 in
  if many && ((not (Sys.file_exists dest)) || not (Sys.is_directory dest)) then
    Error (dest ^ ": not a directory")
  else
    Ok
      (Printf.printf
         "verbose = %B\nrecurse = %B\nforce = %B\nsrcs = %s\ndest = %s\n"
         verbose
         recurse
         force
         (String.concat ", " srcs)
         dest)

(* Command line interface *)

open Kwdcmd

let () =
  Exec.run
    ~name:"cp"
    ~version:"%%VERSION%%"
    ~doc:"Copy files"
    ~man_xrefs:
      [ `Tool "mv"; `Tool "scp"; `Page ("umask", 2); `Page ("symlink", 7) ]
    ~man:[ `S Cmdliner.Manpage.s_bugs; `P "Email them to <bugs@example.org>." ]
  @@ let+ verbose =
       Optional.flag
         ~flags:[ "v"; "verbose" ]
         ~doc:"Print file names as they are copied."
         ()
     and+ recurse =
       Optional.flag
         ~flags:[ "r"; "R"; "recursive" ]
         ~doc:"Copy directories recursively."
         ()
     and+ force =
       Optional.flag
         ~flags:[ "f"; "force" ]
         ~doc: "If a destination file cannot be opened, remove it and try again."
         ()
     and+ srcs =
       Required.pos_all "SOURCE"
       ~doc:"Source file(s) to copy."
       ~conv:Arg.file
       ()
     and+ dest =
       let doc =
         "Destination of the copy. Must be a directory if there is more than \
          one $(i,SOURCE)."
       in
       let docv = "DEST" in
       Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv ~doc)
     in
     cp verbose recurse force srcs dest
