/*
 *  Sequential implementation of π by quadrature using imperative approach.
 *
 *  Copyright © 2013–2015  Russel Winder
 */

extern crate time;
extern crate output;

use time::precise_time_s;
use output::output;

fn main() {
    let n = 1000000000u64;
    let delta = 1.0 / n as f64;
    let start_time = precise_time_s();
    let pi = 4.0 * delta * (1 .. n).map(|i| {
        let x = (i as f64 - 0.5) * delta;
        1.0 / (1.0 + x * x)
    }).fold(0.0, |acc, x| acc + x);
    let elapse_time = precise_time_s() - start_time;
    output("pi_sequential_mapfold".to_string(), pi, n, elapse_time)
}
