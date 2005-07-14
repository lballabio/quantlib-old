#!/usr/bin/python

import Gnuplot, time, math

gplot = Gnuplot.Gnuplot()

gplot('set nokey')
gplot('set data style boxes')
gplot('set boxwidth 1e+6')
gplot('set xdata time')
gplot('set timefmt "%Y-%m-%d"')

gplot('set xlabel "Release" 0,-2')
gplot('set format x ""')

gplot('set yrange [0:*]')

def format(date):
    tp = time.strptime(date,'%Y-%m-%d')
    return time.strftime("%b '%y",tp)
tags = []
maxLines = 0
maxClasses = 0
try:
    file = open('history.dat')
    for line in file:
    	if line.strip():
            date, lines, classes, tag = line.split()
            tags.append((tag,date))
            maxLines = max(maxLines,int(lines))
            maxClasses = max(maxClasses,int(classes))
finally:
    file.close()

def addTags(maxValue):
    off = offset(maxValue)
    for tag, date in tags:
        gplot('set label "%s" at "%s",-%d center' % (tag,date,off))
        gplot('set label "%s" at "%s",-%d center' % (format(date),date,2*off))

def offset(maxValue):
    power = math.floor(math.log10(maxValue))
    unit = math.pow(10,power)
    cap = math.ceil(maxValue/unit)*unit
    return int(cap * 0.042)
    
gplot('set terminal postscript eps enhanced 18')
gplot('set size 3,1')
gplot('set output "lines.eps"')
gplot('set ylabel "Lines of code"')
addTags(maxLines)
gplot('plot "history.dat" using 1:2')
gplot('set output "classes.eps"')
gplot('set ylabel "Number of classes"')
addTags(maxClasses)
gplot('plot "history.dat" using 1:3')
gplot('set output')


