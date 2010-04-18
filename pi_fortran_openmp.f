!  A Fortran program to calculate Pi using quadrature as a parallel algorithm using OpenMP.
!
!  Copyright © 2008-9 Russel Winder

program pi
  implicit none
  integer , parameter :: n = 1000000000
  integer , parameter :: LongReal = selected_real_kind ( p = 18 )
  real ( LongReal ) , parameter :: delta = 1.0 / n
  real ( LongReal ) :: sum = 0.0 , elapseTime , pi_
  integer :: i , startTime , startFrequency , endTime , endFrequency , omp_get_num_procs
  call system_clock ( startTime , startFrequency )
!$omp parallel do private ( i ) reduction ( + : sum )
  do i = 1 , n
     sum = sum + 1.0 / ( 1.0 +  ( ( i - 0.5 ) * delta ) ** 2 ) 
  end do
!$omp end parallel do
  pi_ = 4.0 * sum * delta
  call system_clock ( endTime , endFrequency )
  elapseTime = endTime - startTime
  elapseTime = elapseTime / startFrequency
  print * , "==== Fortran OpenMP pi =" , pi_
  print * , "==== Fortran OpenMP iteration count =" , n
  print * , "==== Fortran OpenMP elapse =" , elapseTime
  print * , "==== Fortran OpenMP processor count = " , omp_get_num_procs ( )
end program pi
