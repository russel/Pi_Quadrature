package output

import (
	"fmt"
	"time"
)

func OutS ( banner string , pi float64 , n int32 , elapseTime time.Duration ) {
	fmt.Printf ( "================ %s\n" , banner )
	fmt.Printf ( "\tΠ = %.18f\n" , pi )
	fmt.Printf ( "\titeration count = %d\n" , n )
	fmt.Printf ( "\telapse time = %v\n" , elapseTime )
}

func OutP ( banner string , pi float64 , n int32 , elapseTime time.Duration , numberOfTasks int ) {
	fmt.Printf ( "================ %s\n" , banner )
	fmt.Printf ( "\tΠ = %.18f\n" , pi )
	fmt.Printf ( "\titeration count = %d\n" , n )
	fmt.Printf ( "\telapse time = %v\n" , elapseTime )
	fmt.Printf ( "\tnumber of tasks = %d\n" , numberOfTasks )
}
