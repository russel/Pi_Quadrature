!  A Fortran program to calculate π using quadrature.  This is an SPMD realization using OpenMPI.
!
!  Copyright © 2008–2012, 2014  Russel Winder

program pi
  use mpi
  implicit none
  integer, parameter:: n = 1000000000
  integer, parameter:: DoubleKind = selected_real_kind(p = 15)
  real (DoubleKind), parameter:: delta = 1.0 / n
  real (DoubleKind):: localSum = 0.0, sum = 0.0, elapseTime, pi_
  integer:: i, startTime, startFrequency, endTime, endFrequency
  integer:: errorState, nProcessors, myId, start, end, sliceSize
  call system_clock(startTime, startFrequency)
  call MPI_Init(errorState)
  call MPI_Comm_size(MPI_COMM_WORLD, nProcessors, errorState)
  call MPI_Comm_rank(MPI_COMM_WORLD, myId, errorState)
  sliceSize = n / nProcessors
  start = 1 + myId * sliceSize
  end = (myId + 1) * sliceSize
  do i = start, end
    localSum = localSum + 1.0 /(1.0 +(( i - 0.5) * delta) ** 2)
  enddo
  call MPI_Reduce(localSum, sum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, errorState)
  call MPI_Finalize(errorState)
  if (myId == 0) then
     pi_ = 4.0 * delta * sum
     call system_clock(endTime, endFrequency)
     elapseTime = endTime - startTime
     elapseTime = elapseTime / startFrequency
     call out("MPI", pi_, n, elapseTime, nProcessors)
  endif
endprogram
