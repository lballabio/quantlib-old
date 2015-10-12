subroutine lgm_swaption_engine_ad(n_times, times, res, dres)
  use OAD_active
  use OAD_rev

  implicit none
  external lgm_swaption_engine

  ! interface

  integer:: n_times
  double precision:: times(0:n_times-1), res, dres(0:n_times-1)

  ! additional variables

  integer:: i

  ! copy inputs to active types and initialize derivatives

  type(active):: times_ad(0:n_times-1), res_ad

  do i = 0, n_times-1, 1
     times_ad(i)%v = times(i)
  end do

  res_ad%v=0.0d0
  res_ad%d=1.0d0

  ! call routine
  our_rev_mode%tape=.true.
  call lgm_swaption_engine(n_times, times_ad, res_ad)

  ! copy results
  res = res_ad%v
  do i = 0, n_times-1, 1
     dres(i) = times_ad(i)%d
  end do

end subroutine lgm_swaption_engine_ad
