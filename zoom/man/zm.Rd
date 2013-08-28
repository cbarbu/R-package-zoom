\name{zm}
\alias{zm}
\title{Launch interaction on a plot}
\usage{
  zm(type = NULL, rp = NULL)
}
\arguments{
  \item{type}{the type of interaction with the plot.
  Possible types are: \itemize{ \item{session}{ for console
  menu} \item{navigation}{ for mouse interaction} }

  Or any short names for these. By default will try to
  launch a "navigation" session.}

  \item{rp}{plot to navigate, saved using
  \code{rp<-recordPlot()}. By default (NULL) will use the
  current device.}
}
\value{
  The recording of the final plot. Can be reploted using
  replayPlot(). The most useful may be to get the xlim and
  ylim of the final plot. That can be simply got using:
  \code{par("usr")} after \code{zm()} ends.
}
\description{
  Allow to zoom/navigate in any open plot. The controls
  should be intuitive: \itemize{ \item{zoom in:}{ scroll
  up, or right click if no scrolling weel.} \item{zoom
  out:}{ scroll down, or double click if no scrolling
  weel.} \item{move:}{ left click and move } }
}
\details{
  By default, zm() try to open a mouse interactive session.
  If the current device is not interactive, will try to
  replot the current plot in a \code{X11(type="Xlib")}
  device. If it fails it will open a console menu based
  interactive session.

  Zoom handle multiple plots on a device together. You need
  to navigate the last one plotted and all the other plots
  will be navigated according to the last one: that can be
  pretty amazing too if you want to explore multiple layers
  at the same time.
}
\note{
  This function relies on pretty low level functions in R
  that change quite often with new versions. New version of
  R can break this package but I got used to it and fix it
  quickly.

  In case you close the device before striking q, just hit
  Ctrl-C on the command line.
}
\examples{
\dontrun{
# basic example
plot(rnorm(1000),rnorm(1000)) # could be any plot
zm() # navigate the plot

# use the same xlim/ylim as ended up in the zoom session
xylim<-par("usr") # xmin,xmax,ymin,ymax of the final version of the plot
dev.off()
plot(rnorm(1000),rnorm(1000),xlim=xylim[1:2],ylim=xylim[3:4])

# navigate two layers of data at the same time
par(mfrow=c(1,2))
plot(1,type="n",xlim=c(-3,3),ylim=c(-3,3),main="First Track")
polygon(c(-1,1,1,-1)*2,c(-1,-1,1,1)*2,col="blue")
lines(rnorm(100),rnorm(100))
plot(1,type="n",xlim=c(-3,3),ylim=c(-3,3),main="Second Track")
polygon(c(-1,1,1,-1)*2,c(-1,-1,1,1)*2,col="green")
lines(rnorm(100),rnorm(100))
zm() # it flickers quite a bit as it needs to replot everything every time...

# one might want to use the older interface
# if attached to cairo under linux or MacOS
# it is also sometimes helpful to just define a square you want to zoom on
zm(type="s")
}
}
\author{
  Corentin M. Barbu
}
\keyword{navigate}
\keyword{navigation}
\keyword{plot}
\keyword{zm}
\keyword{zoom}

