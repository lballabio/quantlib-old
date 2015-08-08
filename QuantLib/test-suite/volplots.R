filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    #    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
    #                            col = col))
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }


filled.legend <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                         length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes, ...) 
  {
    # modification of filled.contour by Carey McGilliard and Bridget Ferris
    # designed to just plot the legend
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    #  on.exit(par(par.orig))
    #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    #  par(las = las)
    #  mar <- mar.orig
    #  mar[4L] <- mar[2L]
    #  mar[2L] <- 1
    #  par(mar = mar)
    # plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    box()
  }
#
#    if (!missing(key.title)) 
#        key.title
#    mar <- mar.orig
#    mar[4L] <- 1
#    par(mar = mar)
#    plot.new()
#    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
#    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
#        stop("no proper 'z' matrix specified")
#    if (!is.double(z)) 
#        storage.mode(z) <- "double"
#    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
#        col = col))
#    if (missing(plot.axes)) {
#        if (axes) {
#            title(main = "", xlab = "", ylab = "")
#            Axis(x, side = 1)
#            Axis(y, side = 2)
#        }
#    }
#    else plot.axes
#    if (frame.plot) 
#        box()
#    if (missing(plot.title)) 
#        title(...)
#    else plot.title
#    invisible()
#}


library(reshape2)
library(akima)

r <- read.csv('fellerext.csv')

xin <- sort(unique(r[,1]))
yin <- sort(unique(r[,2]))
mheston = as.matrix(dcast(melt(r, id=1:2, "heston"), strike ~ time ))[,1:length(yin)+1]
mbs= as.matrix(dcast(melt(r, id=1:2, "bs"), strike ~ time ))[,1:length(yin)+1]
mslv= as.matrix(dcast(melt(r, id=1:2, "fokker"), strike ~ time ))[,1:length(yin)+1]
mhimpl= as.matrix(dcast(melt(r, id=1:2, "hestonImplVol"), strike ~ time ))[,1:length(yin)+1]



mh = bicubic.grid(xin,yin,mbs-mheston,c(50,200),c(1,60),0.5,0.5)
ms = bicubic.grid(xin,yin,(mbs-mslv),c(50,200),c(1,60),0.5,0.5)
mi = bicubic.grid(xin,yin,(mhimpl),c(50,200),c(1,60),0.5,0.5)
xout <- mi$x
yout <- mi$y

plot.new()
#par(new = "TRUE",plt = c(0.1,0.75,0.25,0.85),las = 1,cex.axis = 1)
#filled.contour3(xout,yout,mh$z, color=terrain.colors, zlim = c(min(mh$z),max(mh$z)))
#par(new = "TRUE", plt = c(0.85,0.9,0.25,0.85),las = 1,cex.axis = 1)
#filled.legend(xout,yout,mh,color=terrain.colors,xlab="", ylab="", zlim = c(min(mh$z),max(mh$z)))

par(new = "TRUE",plt = c(0.1,0.75,0.25,0.85),las = 1,cex.axis = 1)
filled.contour3(xout,yout,mi$z, color=terrain.colors, zlim = c(min(mi$z),max(mi$z)))
par(new = "TRUE", plt = c(0.85,0.9,0.25,0.85),las = 1,cex.axis = 1)
filled.legend(xout,yout,mi,color=terrain.colors,xlab="", ylab="", zlim = c(min(mi$z),max(mi$z)))









