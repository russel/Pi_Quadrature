!  A Fortran program to calculate Pi using quadrature as a sequential algorithm.
!
!  Copyright © 2008–2012 Russel Winder

program pi
  implicit none
  integer , parameter :: n = 1000000000
! For GFortran 6, 15, 18, 33 are known precisions.  It is assumed that 6 maps to 32-bit, 15 to 64-bit, and
! 18 to 80-bit (if present). 33 presumably maps to a software 128-bit floating point library.
  integer , parameter :: DoubleKind = selected_real_kind ( p = 15 )
  real ( DoubleKind ) , parameter :: delta = 1.0 / n
  real ( DoubleKind ) :: sum = 0.0 , elapseTime , pi_
  integer :: i , startTime , startFrequency , endTime , endFrequency
  call system_clock ( startTime , startFrequency )
  do i = 1 , n
     sum = sum + 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) 
  end do
  pi_ = 4.0 * delta * sum
  call system_clock ( endTime , endFrequency )
  elapseTime = endTime - startTime
  elapseTime = elapseTime / startFrequency
  call out ( "Sequential Power" , pi_ , n , elapseTime )
end program pi
