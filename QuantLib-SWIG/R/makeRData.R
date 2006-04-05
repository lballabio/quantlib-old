dyn.load('QuantLib.so')
source('QuantLib_wrap.R')
save(list=ls(all=TRUE),file="QuantLib.RData", compress=TRUE)

