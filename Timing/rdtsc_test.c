/*
 *  A test for the C function to extract the time stamp counter on an x86 processor.
 *
 *  Copyright © 2008 Russel Winder
 */

#include <stdio.h>

#include "rdtsc.h"

int main ( void ) {
	char ch;
	long long start , finish ;
	start = rdtsc ( ) ;
	printf ( "start cycles: %lld\n", start ) ;

	printf ( "Enter a character to continue" ) ;
	scanf ( "%c" , &ch ) ;

	finish = rdtsc ( ) ;
	printf ("finish cycles: %lld\n", finish);

	printf ( "normalised high precision time %lld\n", ( finish - start ) ) ;

	return 0 ;
}
