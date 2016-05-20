!  A Fortran program to calculate π using quadrature as a parallel algorithm using Fortran 2008 coarrays.
!
!  Copyright © 2015, 2016  Russel Winder

program pi
  implicit none
  integer, parameter:: n = 1000000000
  integer, parameter:: dp = kind(0.d0)
  integer:: taskCount
  real(dp), parameter:: delta = 1.0_dp / n
  real(dp), allocatable:: partialSums(:)[:]
  real(dp):: partialSum = 0.0, elapseTime, pi_
  integer:: i, j, sliceSize, startTime, startFrequency, endTime, endFrequency
  taskCount = num_images()
  sliceSize = n / taskCount
  allocate(partialSums(0:taskCount-1)[0:*])
  call system_clock(startTime, startFrequency)
  do concurrent(i = 0 : taskCount - 1)
     do j = 1 + i * sliceSize, (i + 1) * sliceSize
        partialSum = partialSum + 1.0_dp / (1.0_dp + ((j - 0.5_dp) * delta) ** 2)
     end do
     partialSums(i)[1] = partialSum
  end do
  sync all
  if (this_image() == 1) then
    pi_ = 4.0_dp * delta * sum(partialSums)
    call system_clock(endTime, endFrequency)
    elapseTime = endTime - startTime
    elapseTime = elapseTime / startFrequency
    call outn("Coarray", pi_, n, elapseTime, taskCount)
  end if
end program pi
