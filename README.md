R-package-zoom
==============

A package allowing to zoom in and out and more generally navigate a plot or a group of plots in R.

Please see zoom.pdf for the full manual and description of the package. 

This package has been published on CRAN: http://cran.r-project.org/web/packages/zoom/
Not all releases are available on CRAN (limited rythm of updates) so check the last releases on this site (releases/)
if you have any problem. 

Installation
------------
Int most case, just copy/paste in R: 

devtools::install_git("https://github.com/cbarbu/R-package-zoom",subdir="zoom")

If issues you can try these other, plateform specific options : 

### Windows
In the releases/ folder, download the zoom\*.zip (need to click on "View raw". Then install it from withing Rgui clicking on Packages - Install a binary package from a zip file".

### Mac or linux
Download the zoom\*.tar.gz from the releases/ folder. 
Then in the console: R CMD INSTALL zoom\*.tar.gz

### Development version
You can always clone this repository, I'll assume you know what you are doing.

Organization of the package
---------------------------
zoom is the normal package folder for R

releases is the folder with official releases sources and binary for windows. 


