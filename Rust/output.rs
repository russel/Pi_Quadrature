/*
 *  Output functions for the Rust realizations of  π by quadrature.
 *
 *  Copyright © 2013  Russel Winder
 */

#[link(name = "output", vers = "0.0", author = "rw")];

pub fn output(banner:&str, pi:float, n:uint, elapseTime:float) {
    printfln!("======================== %s", banner);
    printfln!("\tπ = %.18f", pi);
    printfln!("\titeration count = %ud", n);
    printfln!("\telapse time = %f", elapseTime);
}

pub fn outputN(banner:&str, pi:float, n:uint, elapseTime:float, numberOfTasks:uint) {
    output(fmt!("%s, task count: %ud", banner, numberOfTasks), pi, n, elapseTime);
    printfln!("\tnumber of processors = %d", 8);
}
