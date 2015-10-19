(*

  Output functions for the OCaml implementation of π by Quadrature.


  Copyright © 2015  Russel Winder

*)


(* OCaml appears not to have the ability to find the number of cores built in. This code comes from:
http://stackoverflow.com/questions/16269393/how-to-get-the-number-of-cores-on-a-machine-with-ocaml *)
let cpu_count () =
  try match Sys.os_type with
  | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")
  | _ ->
      let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let close () = ignore (Unix.close_process_in i) in
      try Scanf.fscanf i "%d" (fun n -> close (); n) with e -> close (); raise e
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _
  | End_of_file | Unix.Unix_error (_, _, _) -> 1;;
(* End Stackoverflow code *)


let out banner pi n elapseTime =
  Printf.printf "==================== %s\n" banner;
  Printf.printf "\tπ = %.18f\n" pi;
  Printf.printf "\titeration count = %d\n" n;
  Printf.printf "\telapse time = %f\n" elapseTime;
  flush stdout;;

let outn banner pi n elapseTime taskCount =
  out (banner ^ ": task count: " ^ (string_of_int taskCount)) pi n elapseTime;
  Printf.printf "\tprocessor count = %d\n" (cpu_count ());
  flush stdout;;
