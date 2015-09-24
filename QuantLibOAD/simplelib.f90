subroutine discount(zeroyield, dcf, df)

  implicit none

  double precision:: zeroyield, dcf, df

  !$openad INDEPENDENT(zeroyield)

  df = exp(-zeroyield * dcf)

  !$openad DEPENDENT(df)

end subroutine discount

