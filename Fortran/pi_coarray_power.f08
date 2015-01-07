!  A Fortran program to calculate π using quadrature as a parallel algorithm using Fortran 2008 coarrays.
!
!  Copyright © 2015  Russel Winder

program pi
  implicit none
  integer, parameter:: n = 1000000000
  integer, parameter:: DoubleKind = selected_real_kind(p = 15)
  integer:: taskCount
  real (DoubleKind), parameter:: delta = 1.0 / n
  real (DoubleKind), allocatable:: partialSums(:)[:]
  real (DoubleKind):: partialSum = 0.0, elapseTime, pi_
  integer:: i, j, sliceSize, startTime, startFrequency, endTime, endFrequency
  taskCount = num_images()
  sliceSize = n / taskCount
  allocate(partialSums(0:taskCount-1)[0:*])
  call system_clock(startTime, startFrequency)
  do concurrent(i = 0 : taskCount - 1)
     do j = 1 + i * sliceSize, (i + 1) * sliceSize
        partialSum = partialSum + 1.0 / (1.0 + ((j - 0.5) * delta) ** 2)
     enddo
     partialSums(i)[1] = partialSum
  enddo
  sync all
  if (this_image() == 1) then
    pi_ = 4.0 * delta * sum(partialSums)
    call system_clock(endTime, endFrequency)
    elapseTime = endTime - startTime
    elapseTime = elapseTime / startFrequency
    call outn("Coarray", pi_, n, elapseTime, taskCount)
  endif
endprogram pi
