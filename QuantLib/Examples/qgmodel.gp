plot 'qgmodel.dat' u 1:2 w l title 'kappa'

# test h vs explicit expression
plot 'qgmodel.dat' u 1:3 w l title 'h(full)',
plot 'qgmodel.dat' u 1:4 w l title 'h(analytisch)'
plot 'qgmodel.dat' u 1:($3-$4) w l title 'error h'

# test G vs explicit expression
plot 'qgmodel.dat' u 1:5 w l title 'G(full)'
plot 'qgmodel.dat' u 1:6 w l title 'G(analytisch)'
plot 'qgmodel.dat' u 1:($5-$6) w l title 'error G'

# local vol surface
set xrange [*:*]
set yrange [*:*]
splot 'localvol.txt' u 1:2:3 w l