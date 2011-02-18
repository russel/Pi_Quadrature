(*

  An OCaml program to calculate Pi using quadrature.  This is an SPMD realization using OpenMPI under the
  OCaml MPI bindings.

  Copyright © 2010-2011 Russel Winder

*)

(* Sys.time measures processor time of the process and not elapse time.  The two are (roughly) the same for
single process programs.  For MPI programs however, there are multiple processes and so Sys.time is not
adequate to the task.  For now assume that Unix.gettimeofday is implemented on the execution platform. *)

let main ( ) =
  let n = 1000000000 in
  let delta = 1.0 /. ( float_of_int n ) in
  let startTime = Unix.gettimeofday ( ) in
  let nProcessors = Mpi.comm_size Mpi.comm_world in
  let myId = Mpi.comm_rank Mpi.comm_world in
  let sliceSize = n / nProcessors in
  let start = 1 + myId * sliceSize in 
  let finish = ( myId + 1 ) * sliceSize in
  let localSum = ref 0.0 in
  for i = start to finish do
    let x = ( ( float_of_int i ) -. 0.5 ) *. delta in
    localSum := ! localSum +. ( 1.0 /. ( 1.0 +. x *. x ) )
  done ;
  let sum = Mpi.reduce_float ( ! localSum ) Mpi.Float_sum 0 Mpi.comm_world in
  if myId == 0 then
    let pi = 4.0 *. sum *. delta  in
    let elapseTime = Unix.gettimeofday ( ) -. startTime in
    Printf.printf "==== OCaml MPI pi = %.18f\n" pi ;
    Printf.printf "==== OCaml MPI iteration count = %d\n" n ;
    Printf.printf "==== OCaml MPI elapse time = %f\n" elapseTime ;
    Printf.printf "==== OCaml MPI processor count = %d\n" nProcessors ;;

main ( ) ;;
