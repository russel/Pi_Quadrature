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
    let mut sum = 0.0f64;
    for i in 1 .. n {
        let x = (i as f64 - 0.5) * delta;
        sum += 1.0 / (1.0 + x * x)
    }
    let pi = 4.0 * delta * sum;
    let elapse_time = precise_time_s() - start_time;
    output("Sequential For".to_string(), pi, n, elapse_time)
}
