subroutine lgm_swaption_engine_ad(n_times, times, modpar, n_expiries, &
     expiries, callput, n_floats, &
     float_startidxes, float_mults, index_acctimes, float_spreads, float_t1s, float_t2s, float_tps, &
     fix_startidxes, n_fixs, fix_cpn, fix_tps, integration_points, stddevs, res)

  use OAD_active
  use OAD_rev

  implicit none
  external lgm_swaption_engine

  ! interface

  integer:: n_times
  double precision, dimension(0:n_times-1):: times

  ! concatenated model parameter array: H, zeta, discounts
  double precision, dimension(0:3*n_times-1):: modpar

  integer:: n_expiries, callput
  integer, dimension(0:n_expiries-1):: expiries, fix_startidxes, float_startidxes
  integer:: n_floats, n_fixs
  double precision, dimension(0:n_floats-1):: float_mults, index_acctimes, float_spreads
  integer, dimension(0:n_floats-1):: float_t1s, float_t2s
  integer, dimension(0:n_floats-1):: float_tps
  integer, dimension(0:n_fixs-1):: fix_tps
  double precision, dimension(0:n_fixs-1):: fix_cpn

  integer:: integration_points
  double precision:: stddevs

  double precision:: res

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
