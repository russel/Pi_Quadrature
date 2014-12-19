/*
 *  Sequential implementation of π by quadrature using imperative approach.
 *
 *  Copyright © 2013, 2014  Russel Winder
 */

extern crate time;
extern crate output;

use std::vec::Vec;
use std::sync::Future;
use time::precise_time_s;
use output::output_n;

fn execute(number_of_tasks:uint) {
    let n = 1000000000u;
    let delta = 1.0 / n as f64;
    let start_time = precise_time_s();
    let slice_size = n / number_of_tasks;
    let mut futures = Vec::from_fn(number_of_tasks, |id| Future::spawn(move || {
        let mut sum:f64 = 0.0;
        for i in range(1 + id * slice_size, (id + 1) * slice_size) {
            let x = (i as f64 - 0.5) * delta;
            sum += 1.0 / (1.0 + x * x)
        }
        sum
    }));
    let pi = 4.0 * delta * futures.iter_mut().fold(0.0, |acc, i| acc + i.get());
    let elapse_time = precise_time_s() - start_time;
    output_n("pi_parallel_futures".to_string(), pi, n, elapse_time, number_of_tasks)
}

fn main() {
    execute(1u);
    execute(2u);
    execute(8u);
    execute(32u)
}
