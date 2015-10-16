! to dos:
! multicurve

subroutine lgm_swaption_engine(n_times, times, modpar, n_expiries, &
     expiries, callput, n_floats, &
     float_startidxes, float_mults, index_acctimes, float_spreads, float_t1s, float_t2s, float_tps, &
     fix_startidxes, n_fixs, fix_cpn, fix_tps, integration_points, stddevs, res)

  implicit none

  ! constants

  double precision, parameter:: M_SQRT2 = 1.41421356237309504880
  double precision, parameter:: M_SQRTPI = 1.77245385090551602792981
  double precision, parameter:: M_SQRT1_2 = 0.7071067811865475244008443621048490392848359376887

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

  ! additional variables

  integer:: idx, expiry0idx, expiry1idx, yidx0, yidx1
  integer:: i, k, l, m
  double precision, dimension(0:2*integration_points,0:1):: npv
  double precision, dimension(0:2*integration_points):: val, z
  double precision:: h, sigma_t0_t1, sigma_0_t0, sigma_0_t1
  double precision:: center, yidx, c, d, e, ca, da, x0, x1, price
  double precision:: weight0, weight1, floatlegnpv, fixlegnpv
  double precision:: discount, forward, exercisevalue

  integer:: swapflag

  ! start of the calculation

  do k=0, 2*integration_points,1
     npv(k,0) = 0.0d0
     npv(k,1) = 0.0d0
  end do

  swapflag = 0

  !$openad INDEPENDENT(activevars)

  expiry0idx = expiries(n_expiries-1)
  expiry1idx = 0

  h = stddevs / dble(integration_points)
  do k = 0, 2*integration_points, 1
     z(k) = -stddevs + dble(k) * h
  end do

  ! loop over expiry dates
  do idx = n_expiries-1, -1, -1

     if (idx == -1) then
        expiry0idx = 0
     else
        expiry0idx = expiries(idx)
     endif

     sigma_0_t0 = sqrt(modpar(n_times+expiry0idx))
     if (expiry1idx /= 0) then
        sigma_0_t1 = sqrt(modpar(n_times+expiry1idx))
        sigma_t0_t1 = sqrt(modpar(n_times+expiry1idx)-modpar(n_times+expiry0idx))
     end if

     ! loop over integration points

     if(idx == -1) then
        m = 0
        center = 0
     else
        m = 2*integration_points
     endif

     do k = 0, m, 1

        ! roll back
        if (expiry1idx /= 0) then

           if (idx /= -1) then
              center = z(k)
           endif

           do i=0, 2*integration_points, 1
              yidx = ((center*sigma_0_t0+dble(i-integration_points)/dble(integration_points)* &
                   stddevs*sigma_t0_t1)/sigma_0_t1 + stddevs) / (2.0d0*stddevs) * dble(2*integration_points)
              yidx0 = floor(yidx)
              yidx1 = yidx0+1
              weight0 = yidx1 - yidx
              weight1 = yidx - yidx0
              val(i) = (weight0 * npv(min(max(yidx0,0),2*integration_points),1-swapflag) + &
                   weight1 * npv(min(max(yidx1,0),2*integration_points),1-swapflag))
           end do

           price = 0.0d0
           do i=0, 2*integration_points-1, 1 ! 1 for linear, 2 for quadratic
              ! quadratic interpolation
              ! x0 = z(i) * M_SQRT1_2
              ! x1 = z(i+2) * M_SQRT1_2
              ! c = val(i) / (2.0d0*h*h) + val(i+1) / (-h*h) + val(i+2) / (2.0d0*h*h)
              ! d = val(i) * (-z(i+2)-z(i+1))/(2.0d0*h*h) + val(i+1) * (-z(i+2)-z(i))/ (-h*h) + &
              !      val(i+2) * (-z(i+1)-z(i)) / (2.0d0*h*h)
              ! e = val(i)*z(i+1)*z(i+2)/(2.0d0*h*h) + val(i+1)*z(i)*z(i+2)/(-h*h) + &
              !      val(i+2)*z(i)*z(i+1)/(2.0d0*h*h)
              ! ca = 2.0d0 * c
              ! da = M_SQRT2 * d
              ! price = price + (0.125d0 * (2.0d0 * ca + 4.0d0 * e) * erf(x1) - &
              !      1.0d0 / (4.0d0 * M_SQRTPI) * exp(-x1 * x1) * &
              !      (2.0d0 * ca * x1 + 2.0d0 * da)) - &
              !      (0.125d0 * (2.0d0 * ca + 4.0d0 * e) * erf(x0) - &
              !      1.0d0 / (4.0d0 * M_SQRTPI) * exp(-x0 * x0) * &
              !      (2.0d0 * ca * x0 + 2.0d0 * da))
              ! linear interpolation
              x0 = z(i) * M_SQRT1_2
              x1 = z(i+1) * M_SQRT1_2
              d = (val(i+1)-val(i))/(z(i+1)-z(i))
              e = (val(i)*z(i+1)-val(i+1)*z(i)) / (z(i+1)-z(i))
              da = M_SQRT2 * d
              price = price + (0.5d0 * e * erf(x1) - &
                   1.0d0 / (4.0d0 * M_SQRTPI) * exp(-x1 * x1) * &
                   (2.0d0 * da)) - &
                   (0.5d0 * e * erf(x0) - &
                   1.0d0 / (4.0d0 * M_SQRTPI) * exp(-x0 * x0) * &
                   (2.0d0 * da))
           end do
           npv(k,swapflag) = price
        end if

        ! payoff generation

        if(idx >= 0) then
           floatlegnpv = 0.0d0
           do l = float_startidxes(idx), n_floats-1, 1
              forward = (modpar(2*n_times+float_t1s(l)) * &
                   exp(-modpar(float_t1s(l))*(z(k)*sigma_0_t0+ &
                   0.5*modpar(n_times+expiries(idx))*modpar(float_t1s(l)))) / &
                   (modpar(2*n_times+float_t2s(l)) * &
                   exp(-modpar(float_t2s(l))*(z(k)*sigma_0_t0+ &
                   0.5*modpar(n_times+expiries(idx))*modpar(float_t2s(l))))) &
                   - 1.0d0) / index_acctimes(l)
              discount =  modpar(2*n_times+float_tps(l)) * &
                   exp(-modpar(float_tps(l))*(z(k)*sigma_0_t0+ &
                   0.5*modpar(n_times+expiries(idx))*modpar(float_tps(l))))
              floatlegnpv = floatlegnpv + float_mults(l) * ( float_spreads(l) + forward) * discount
           end do
           fixlegnpv = 0.0d0
           do l = fix_startidxes(idx), n_fixs-1, 1
              discount =  modpar(2*n_times+fix_tps(l)) * &
                   exp(-modpar(fix_tps(l))*(z(k)*sigma_0_t0+ &
                   0.5*modpar(n_times+expiries(idx))*modpar(fix_tps(l))))
              fixlegnpv = fixlegnpv + fix_cpn(l) * discount
           end do
           exercisevalue = callput * (floatlegnpv - fixlegnpv)
           npv(k,swapflag) = max(npv(k,swapflag),exercisevalue)
        end if

     end do ! loop integration points

     swapflag = 1-swapflag
     expiry1idx = expiry0idx

  end do ! loop expiry dates

  res = npv(0,1-swapflag)

  !$openad DEPENDENT(res)

end subroutine lgm_swaption_engine
