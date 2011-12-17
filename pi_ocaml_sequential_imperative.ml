(*

  Caml implementation of the sequential Pi by Quadrature using an imperative approach.
  
  Copyright © 2009–2011 Russel Winder

*)

let main ( ) =
  let n = 1000000000 in
  let delta = 1.0 /. ( float_of_int n ) in
  let startTime = Sys.time ( ) in
  let sum = ref 0.0 in
  for i = 1 to n do
    let x = ( ( float_of_int i ) -. 0.5 ) *. delta in
    sum := ! sum +. ( 1.0 /. ( 1.0 +. x *. x ) )
  done ;
  let pi = 4.0 *. delta *. ! sum in
  let elapseTime = Sys.time ( ) -. startTime in
  Printf.printf "==== OCaml Sequential Imperative pi = %.18f\n" pi ;
  Printf.printf "==== OCaml Sequential Imperative iteration count = %d\n" n ;
  Printf.printf "==== OCaml Sequential Imperative elapse time = %f\n" elapseTime ;
  exit 0 ;;

main ( ) ;;
