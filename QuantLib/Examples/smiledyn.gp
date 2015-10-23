cd '/home/peter/quantlib/QuantLib/Examples/'

set yrange [0:*]
set xrange [0.0010:*]
set xrange [*:*]
set yrange [*:*]

# vol smile
plot 'smiledyn.dat' u 1:2 w l title 'implied log volatility'

# sticky strike
plot 'smiledyn.dat' u 1:3 w l title 'f=0.0300', '' u 1:4 w l title 'f=0.0350', '' u 1:8 w l title 'f=0.0400'

# sticky absolute moneyness
set xrange [0.0110:*]
set yrange [0:50]
plot 'smiledyn.dat' u 1:3 w l title 'f=0.0300', '' u 1:5 w l title 'f=0.0350', '' u 1:9 w l title 'f=0.0400'

# sticky bpvol
plot 'smiledyn.dat' u 1:3 w l title 'f=0.0300', '' u 1:6 w l title 'f=0.0350', '' u 1:10 w l title 'f=0.0400'

# model
plot 'smiledyn.dat' u 1:3 w l title 'f=0.0300', '' u 1:7 w l title 'f=0.0350', '' u 1:11 w l title 'f=0.0400'
