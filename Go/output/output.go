// An output function for all the various versions of calculating π using quadrature.
//
//  Copyright © 2012–2013 Russel Winder

package output

import (
	"fmt"
	"time"
)

func OutS(banner string, pi float64, n int32, elapseTime time.Duration) {
	fmt.Printf("================ %s\n", banner)
	fmt.Printf("\tπ = %.18f\n", pi)
	fmt.Printf("\titeration count = %d\n", n)
	fmt.Printf("\telapse time = %v\n", elapseTime)
}

func OutP(banner string, pi float64, n int32, elapseTime time.Duration, numberOfTasks int) {
	fmt.Printf("================ %s\n", banner)
	fmt.Printf("\tπ = %.18f\n", pi)
	fmt.Printf("\titeration count = %d\n", n)
	fmt.Printf("\telapse time = %v\n", elapseTime)
	fmt.Printf("\tnumber of tasks = %d\n", numberOfTasks)
}
