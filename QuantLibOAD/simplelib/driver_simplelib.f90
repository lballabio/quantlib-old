subroutine discount_ad(zeroyield, dcf, df, ddf)
  use OAD_active

  implicit none
  external discount

  double precision:: zeroyield, dcf, df, ddf

  type(active):: zeroyield_ad, res

  zeroyield_ad%v = zeroyield
  zeroyield_ad%d = 1.0

  call discount(zeroyield_ad, dcf, res)

  df = res%v
  ddf = res%d

end subroutine discount_ad
