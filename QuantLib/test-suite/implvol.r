library(reshape2)
library(akima)
library("lattice")
library("grid")
library("Cairo"); 

CairoPNG("plot.png", width=1024, height=1024)

r <- read.csv('fellerext.csv')

xin <- sort(unique(r[,1]))
yin <- sort(unique(r[,2]))
#mhimpl <- as.matrix(dcast(melt(r, id=1:2, "hestonImplVol"), strike ~ time ))[,1:length(yin)+1]
mhimpl <- as.matrix(dcast(melt(r, id=1:2, "fokkerImplVol"), strike ~ time ))[,1:length(yin)+1]

mi = bicubic.grid(xin,yin,0.3+0.6*(mhimpl-0.3+0.00025),c(50,200),c(2.5,61),3,2)
xout <- mi$x
yout <- mi$y/12


g <- expand.grid(strikes=xout, maturities=yout)
g$vol = unlist(as.list(mi$z))*100

wireframe(vol ~ strikes*maturities, g, cex=1.5,
      xlab=list(label="Strike", cex=1.75, rot=-23),
      ylab=list(label="Maturity",cex=1.75, rot=48),
      zlab=list("Volatility",rot=90, cex=1.75), 
      drape=TRUE,col.regions=rainbow(100,start=0.05, end=0.775,alpha=0.65),
      screen = list(z = 325, x = -65), 
      scales=list(arrows=FALSE, cex=1.4, tck=0.5, font=1,
                  z=list(at=c(29.90,29.95,30.00,30.05),
                  lab=c('29.90%          ', '29.95%           ',
                        '30.00%          ', '30.05%         '))),
      zlim=c(29.9, 30.065),ylim=c(2.5/12,5), zoom=0.85, 
      colorkey=list(space="right", width=3, labels=list(cex=1.4)))

dev.off()
