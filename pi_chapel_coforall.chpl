/*
 *  A Chapel program to calculate Pi using quadrature as a reduce-based algorithm but with an explicit
 *  coforall.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

use Time;
use Output;

proc execute(numberOfTasks:int) {
    param n:int(64) = 1000000000;
    const delta:real = 1.0 / n;
    var timer:Timer;
    timer.start();
    param sliceSize:int(64) = n / numberOfTasks;
    const eachProcessor:domain(1) = [ 0 ..(numberOfTasks - 1) ];
    const results:[eachProcessor] real;
    proc partialSum(const id:int):real {
        const start:int(64) = 1 + id * sliceSize;
        const end:int(64) = (id + 1) * sliceSize;
        var sum:real = 0.0;
        for i in start .. end {
            sum += 1.0 / (1.0 + ((i - 0.5) * delta) ** 2);
        }
        return sum;
    }
    coforall i in eachProcessor do results[i] = partialSum(i);
    const pi = 4.0 * delta * (+ reduce results);
    timer.stop();
    output_more("Chapel Coforall", pi, n,  timer.elapsed(), numberOfTasks);
}

proc main() {
    execute(1);
    writeln();
    execute(2);
    writeln();
    execute(8);
    writeln();
    execute(32);
}
