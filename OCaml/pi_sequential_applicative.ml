(*

  OCaml implementation of the sequential π by Quadrature using a purely applicative style, i.e. iteration
  by tail recursion.

  Copyright © 2009–2011, 2015  Russel Winder

*)

let rec compute i delta sum =
  let x = ((float_of_int i) -. 0.5) *. delta in
  if i = 0 then sum else compute (i - 1) delta (sum +. 1.0 /. (1.0 +. x *. x));;

let () =
  let n = 1000000000 in
  let delta = 1.0 /. (float_of_int n) in
  let startTime = Sys.time () in
  let pi = 4.0 *. delta *. (compute n delta 0.0) in
  let elapseTime = Sys.time () -. startTime in
  Output.out "Sequential Applicative" pi n elapseTime;;
