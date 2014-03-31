/*
 *  Sequential implementation of π by quadrature using imperative approach.
 *
 *  Copyright © 2013, 2014  Russel Winder
 */

extern crate sync;
extern crate time;
extern crate output;

use std::vec::Vec;
use sync::Future;
use time::precise_time_s;
use output::outputN;

fn execute(numberOfTasks:uint) {
    let n = 1000000000u;
    let delta = 1.0 / n as f64;
    let startTime = precise_time_s();
    let sliceSize = n / numberOfTasks;
    let mut futures = Vec::from_fn(numberOfTasks, |id| Future::spawn(proc() {
        let mut sum:f64 = 0.0;
        for i in range(1 + id * sliceSize, (id + 1) * sliceSize) {
            let x = (i as f64 - 0.5) * delta;
            sum += 1.0 / (1.0 + x * x)
        }
        sum
    }));
    let pi = 4.0 * delta * futures.mut_iter().fold(0.0, |acc, i| acc + i.get());
    let elapseTime = precise_time_s() - startTime;
    outputN("pi_parallel_futures", pi, n, elapseTime, numberOfTasks)
}

fn main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32)
}
