(*

  OCaml implementation of the sequential π by Quadrature using an imperative approach.

  Copyright © 2009–2011, 2015  Russel Winder

*)

let () =
  let n = 1000000000 in
  let delta = 1.0 /. (float_of_int n) in
  let startTime = Sys.time () in
  let sum = ref 0.0 in
  for i = 1 to n do
    let x = ((float_of_int i) -. 0.5) *. delta in
    sum := ! sum +. (1.0 /. (1.0 +. x *. x))
  done;
  let pi = 4.0 *. delta *. ! sum in
  let elapseTime = Sys.time () -. startTime in
  Output.out "Sequential Imperative" pi n elapseTime;;
