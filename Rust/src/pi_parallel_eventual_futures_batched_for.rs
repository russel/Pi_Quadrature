/*
 *  Parallel implementation of π by quadrature using a map of asynchronous function returning a future and
 *  chunking of partial sum calculation.
 *
 *  Copyright © 2013–2015  Russel Winder
 */

// std::sync::Future is deemed unstable and so is not present in 1.0.0 beta, must either use the nightly
// builds or use an external package such as eventual.  Choose the latter for now since it is the mainline
// of Rust.

extern crate eventual;
extern crate output;
extern crate time;

use std::vec::Vec;
use eventual::Async;
use eventual::Future;
use time::precise_time_s;
use output::output_n;

fn execute(number_of_tasks:u64) {
    let n = 1000000000u64;
    let delta = 1.0 / n as f64;
    let start_time = precise_time_s();
    let slice_size = n / number_of_tasks;
    let futures: Vec<Future<f64, ()>> = (0 .. number_of_tasks).map(|id| Future::spawn(move || {
        let mut sum = 0.0f64;
        for i in (1 + id * slice_size) .. ((id + 1) * slice_size) {
            let x = (i as f64 - 0.5) * delta;
            sum += 1.0 / (1.0 + x * x)
        }
        sum
    })).collect();
    let pi = 4.0 * delta * futures.into_iter().fold(0.0, |acc, i| acc + i.await().unwrap());
    let elapse_time = precise_time_s() - start_time;
    output_n("pi_parallel_futures".to_string(), pi, n, elapse_time, number_of_tasks)
}

fn main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32)
}
