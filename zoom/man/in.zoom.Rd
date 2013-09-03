\name{in.zoom}
\alias{in.zoom}
\alias{inout.zoom}
\alias{out.zoom}
\alias{set.zoom}
\alias{sq.zoom}
\title{Direct access to zoom functionalities.}
\usage{
  in.zoom(...)

  inout.zoom(...)

  out.zoom(...)

  set.zoom(...)

  sq.zoom(...)
}
\arguments{
  \item{\dots}{Extra arguments to zoomplot.zoom.}
}
\value{
  NULL
}
\description{
  Direct selection of a zoom method of the "session" type.
  Possibly of use in scripts?

  allow interactive in/out zoom in "session" mode
}
\note{
  Each function starts a different interactive sequence
  \itemize{ \item{inout.zoom(): }{left click within bounds
  zooms in, outside bounds zoom out} \item{in.zoom():
  }{each left click zooms in} \item{out.zoom(): }{each left
  click zooms out} \item{set.zoom(): }{ask for a
  magnification factor} \item{sq.zoom(): }{allow to click
  on the two corners of the desired region to zoom on} }
}
\author{
  Corentin M. Barbu
}
\seealso{
  zm(), session.zoom().
}

