/*
 *  Output functions for the Rust realizations of  π by quadrature.
 *
 *  Copyright © 2013–2017  Russel Winder
 */

#![crate_name = "output"]

// std::os::num_cpus has been removed from Rust, use the num_cpus package instead.
extern crate num_cpus;

pub fn output(banner: &str, pi: f64, n: u64, elapse_time: f64) {
    println!("======================== {}", banner);
    println!("\tπ = {:.18}", pi);
    println!("\titeration count = {}", n);
    println!("\telapse time = {}", elapse_time);
}

pub fn output_n(banner: &str, pi: f64, n: u64, elapse_time: f64, number_of_tasks: u64) {
    output(&(format!("{}, task count: {}", banner, number_of_tasks)), pi, n, elapse_time);
    println!("\tnumber of cores = {}", num_cpus::get());
}
