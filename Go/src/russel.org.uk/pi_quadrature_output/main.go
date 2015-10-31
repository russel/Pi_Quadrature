// An output function for all the various versions of calculating π using quadrature.
//
//  Copyright © 2012–2013, 2015  Russel Winder

package output

import (
	"fmt"
	"runtime"
	"time"
)

func OutS(banner string, pi float64, n int32, elapseTime time.Duration) {
	fmt.Printf("======================== %s\n", banner)
	fmt.Printf("\tπ = %.18f\n", pi)
	fmt.Printf("\titeration count = %d\n", n)
	fmt.Printf("\telapse time = %v\n", elapseTime)
}

func OutP(banner string, pi float64, n int32, elapseTime time.Duration, numberOfTasks int) {
	OutS(banner+fmt.Sprintf(", number of tasks: %d", numberOfTasks), pi, n, elapseTime)
	fmt.Printf("\tnumber of processors = %d\n", runtime.NumCPU())
}
