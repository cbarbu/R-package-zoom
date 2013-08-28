

#' Zoom/navigate any plot directly in R.
#' 
#' The single zm() function allows to launch an interactive viewing session
#' with any plot. You can zoom in and out and move the scope (just as in Google
#' maps). Additionally, if a device contains several plots it will
#' simultanuously navigate all the plots.
#' 
#' Additionally, it allows to print on the fly the current state
#' and continue the navigation
#' 
#' It fully works under Windows and Linux.
#' 
#' On Mac it fully works if launching R from the Xquartz terminal. It will
#' default to the "session" mode if launched from Rgui. R launched from the
#' system terminal may have a bug with X11 fonts that prevents even the
#' simplest plots to be displayed, it will not get any better with this
#' package.
#' 
#' See help(zm) for more details on how to use the package.
#' 
#' \tabular{ll}{ Package: \tab zoom\cr Type: \tab Package\cr Version: \tab
#' 2.0\cr Date: \tab 2013-08-22\cr License: \tab LGPL\cr } The only function
#' end users should ever use is zm(). It orchestrate the access to other
#' functions like: \itemize{ 
#' \item{navigation.zoom(): }{launch a mouse interaction}
#' \item{session.zoom(): }{launch a console menu interaction } 
#' }
#' Themselves orchestrating lower level functions: 
#' \itemize{ 
#' \item{in.zoom(): }{zoom in}
#' \item{out.zoom(): }{zoom out}
#' \item{set.zoom(): }{zoom to a magnification factor}
#' \item{sq.zoom(): }{zoom on a user defined square}
#' \item{zoomplot.zoom(): }{the heart function reploting everything as directed by higher level functions. }
#' }
#' 
#' @name zoom-package
#' @aliases zoom-package zoom
#' @docType package
#' @note This package is maintained as part of the wider spatcontrol project on
#' github: https://github.com/cbarbu/spatcontrol
#' 
#' Bug reports/suggestions/patches can be directly submitted in this web
#' interface.
#' 
#' Known issues: \itemize{
#' \item{print to pdf:}{ in navigation mode, print of a pdf generates 
#'   a weird pdf. Exiting and printing using print.zoom() or 
#'   simply dev.print() works fine.}
#' }
#' @author Corentin M. Barbu
#' 
#' Maintainer: Corentin M. Barbu <corentin.barbu@@gmail.com>
#' @keywords zoom plot navigate navigation
#' @examples
#' \dontrun{
#' plot(rnorm(1000),rnorm(1000))
#' zm()
#' }
#' 
NULL



