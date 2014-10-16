/*
 *  Output functions for the Rust realizations of  π by quadrature.
 *
 *  Copyright © 2013, 2014  Russel Winder
 */

#![crate_name = "output"]

use std::string::String;
use std::rt::default_sched_threads;

pub fn output(banner:String, pi:f64, n:uint, elapse_time:f64) {
    println!("======================== {}", banner);
    println!("\tπ = {:.18f}", pi);
    println!("\titeration count = {}", n);
    println!("\telapse time = {}", elapse_time);
}

pub fn output_n(banner:String, pi:f64, n:uint, elapse_time:f64, number_of_tasks:uint) {
    output(format!("{}, task count: {}", banner, number_of_tasks), pi, n, elapse_time);
    println!("\tnumber of scheduler threads = {}", default_sched_threads());
}
