/*
 *  Sequential implementation of π by quadrature using imperative approach.
 *
 *  Copyright © 2013  Russel Winder
 */

extern mod extra;
extern mod output;

use std::vec::from_fn;
use extra::future::Future;
use extra::future::spawn;
use extra::time::precise_time_s;
use output::outputN;

fn sum_futures(futures:~[Future<float>])->float {
    let mut sum = 0.0;
    for f in futures.iter() { sum += f.get(); }
    sum
}

fn execute(numberOfTasks:uint) {
    let n = 1000000000u;
    let delta = 1.0 / n as float;
    let startTime = precise_time_s();
    let sliceSize = n / numberOfTasks;
    let futures = from_fn(numberOfTasks, |id| spawn(||->float {
        let mut sum = 0.0;
        for i in range(1 + id * sliceSize, (id + 1) * sliceSize) {
            let x = (i as float - 0.5) * delta;
            sum += 1.0 / (1.0 + x * x);
        }
        sum;
    }));
    let pi = 4.0 * delta * sum_futures(futures);
    let elapseTime = precise_time_s() - startTime;
    outputN("pi_sequential_spawn", pi, n, elapseTime, numberOfTasks);
}

fn main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32);
}
