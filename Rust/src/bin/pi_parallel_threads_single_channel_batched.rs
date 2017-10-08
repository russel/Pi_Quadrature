/*
 *  Parallel implementation of π by quadrature using threads and a single channel.
 *
 *  Copyright © 2015–2017  Russel Winder
 */

extern crate output;
extern crate time;

use std::thread;
use std::sync::mpsc;

use time::precise_time_s;
use output::output_n;

fn execute(number_of_threads:u64) {
    let n = 1_000_000_000u64;
    let delta = 1.0 / n as f64;
    let start_time = precise_time_s();
    let slice_size = n / number_of_threads;
    let (tx, rx) = mpsc::channel();
    for id in 0..number_of_threads {
        let tx = tx.clone();
        thread::spawn(move || {
   	    tx.send(((1 + id * slice_size) .. ((id + 1) * slice_size)).fold(0.0, |acc, i| {
    	        let x = (i as f64 - 0.5) * delta;
    	        acc + 1.0 / (1.0 + x * x)
    	    })).unwrap();
        });
    }
    let pi = 4.0 * delta * rx.iter().take(number_of_threads as usize).sum::<f64>();
    let elapse_time = precise_time_s() - start_time;
    output_n("Parallel Threads Single Channel Batched", pi, n, elapse_time, number_of_threads)
}

fn main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32)
}
