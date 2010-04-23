!  A Fortran program to calculate Pi using quadrature as a sequential algorithm.
!
!  Copyright © 2008-10 Russel Winder

program pi
  implicit none
  integer , parameter :: n = 1000000000
  integer , parameter :: LongReal = selected_real_kind ( p = 18 )
  real ( LongReal ) , parameter :: delta = 1.0 / n
  real ( LongReal ) :: sum = 0.0 , elapseTime , pi_
  integer :: i , startTime , startFrequency , endTime , endFrequency
  call system_clock ( startTime , startFrequency )
  do i = 1 , n
     sum = sum + 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) 
  end do
  pi_ = 4.0 * sum * delta
  call system_clock ( endTime , endFrequency )
  elapseTime = endTime - startTime
  elapseTime = elapseTime / startFrequency
  print * , "==== Fortran Sequential pi =" , pi_
  print * , "==== Fortran Sequential iteration count =" , n
  print * , "==== Fortran Sequential elapse =" , elapseTime
end program pi
