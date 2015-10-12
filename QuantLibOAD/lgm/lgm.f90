subroutine lgm_swaption_engine(n_times, times, res)

  implicit none

  ! interface

  integer:: n_times
  double precision:: times(0:n_times-1), res

  ! additional variables

  integer:: i

  !$openad INDEPENDENT(times)

  do i = 0, n_times-1, 1
     res = res + times(i)
  end do

  !$openad DEPENDENT(res)

end subroutine lgm_swaption_engine

