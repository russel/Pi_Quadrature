/*
 *  Sequential implementation of π by quadrature using imperative approach.
 *
 *  Copyright © 2013  Russel Winder
 */

extern mod extra;
extern mod output;

use extra::time::precise_time_s;
use output::output;

fn main() {
    let n = 1000000000u;
    let delta = 1.0 / n as float;
    let startTime = precise_time_s();
    let mut sum = 0.0;
    for i in range(0u, n) {
        let x = (i as float - 0.5) * delta;
        sum += 1.0 / (1.0 + x * x);
    }
    let pi = 4.0 * delta * sum;
    let elapseTime = precise_time_s() - startTime;
    output("pi_sequential_for", pi, n, elapseTime);
}
