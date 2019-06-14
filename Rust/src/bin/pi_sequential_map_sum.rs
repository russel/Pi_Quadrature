/*
 *  Sequential implementation of π by quadrature using imperative approach.
 *
 *  Copyright © 2013–2017, 2019  Russel Winder
 */

use time::precise_time_s;
use output::output;

fn main() {
    let n = 1_000_000_000u64;
    let delta = 1.0 / n as f64;
    let start_time = precise_time_s();
    let pi = 4.0 * delta * (1 .. n).map(|i| {
        let x = (i as f64 - 0.5) * delta;
        1.0 / (1.0 + x * x)
    }).sum::<f64>();
    let elapse_time = precise_time_s() - start_time;
    output("Sequential Map Sum", pi, n, elapse_time)
}
