(* With reference to https://mbarbin.github.io/cmdlang/docs/tutorials/getting-started/#completing-the-program *)

open Kwdcmd

let () =
  Exec.command ~name:"calc" ~version:"%%VERSION%%" ~doc:"A simple calculator"
  @@ let+ op_str, op =
       Required.value
         "OPERATOR"
         ~flags:[ "op" ]
         ~conv:Arg.(enum [ ("add", ("+", ( +. ))); ("mult", ("*", ( *. ))) ])
         ()
     and+ a = Required.pos "a" ~nth:0 ~conv:Arg.float ()
     and+ b = Required.pos "b" ~nth:1 ~conv:Arg.float ()
     and+ verbose =
       Optional.flag ~flags:[ "verbose" ] ~doc:"print debug information" ()
     in
     if verbose then Printf.printf "op: %s, a: %f, b: %f\n" op_str a b;
     Ok (op a b |> print_float)
