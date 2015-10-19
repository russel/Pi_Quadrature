(*

  OCaml implementation of the sequential π by Quadrature using an imperative approach and the threads
  library.  According to the OCaml manual you can use system threads but it is always by timesharing --
  which seems a bit inconsistent!

  Copyright © 2010–2011, 2015  Russel Winder

*)

type processSliceParameter = {start:int; finish:int; delta:float};;

let accumulator =
  object
    val mutable value = 0.0
    method get = value
    method add x = let m = Mutex.create () in
    Mutex.lock m;
    value <- value +. x;
    Mutex.unlock m
    method reset = value <- 0.0
  end;;

let processSlice args =
  let sum = ref 0.0 in
  for i = args.start to args.finish do
    let x = ((float_of_int i) -. 0.5) *. args.delta in
    sum := ! sum +. (1.0 /. (1.0 +. x *. x))
  done;
  accumulator#add ! sum;;

let execute numberOfThreads =
  let n = 1000000000 in
  let delta = 1.0 /. (float_of_int n) in
  let startTime = Sys.time () in
  let sliceSize = n / numberOfThreads in
  let processes = Array.make numberOfThreads (Thread.self ()) in
  accumulator#reset;
  for i = 0 to numberOfThreads - 1 do
    processes.(i) <- Thread.create processSlice {start = 1 + i * sliceSize; finish = (i + 1) * sliceSize; delta = delta}
  done;
  Array.iter Thread.join processes;
  let pi = 4.0 *. delta *. accumulator#get in
  let elapseTime = Sys.time () -. startTime in
  Output.outn "Threads Imperative Object" pi n elapseTime numberOfThreads;;

let () =
 execute 1;
 execute 2;
 execute 8;
 execute 32;;
