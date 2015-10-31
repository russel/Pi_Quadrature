#!/bin/sh

export GOPATH=`pwd`
unset GOBIN

for d in output sequential goroutines_singleChannel goroutines_multipleChannels
do
    go install -compiler gccgo russel.org.uk/pi_quadrature_$d
done
