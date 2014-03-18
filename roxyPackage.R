#!/usr/bin/Rscript --vanilla
#===============================================================================
#
#          FILE: roxyPackage.R
# 
#         USAGE: ./roxyPackage.R 
# 
#   DESCRIPTION: regenerate DESCRIPTION, NAMESPACE and possibly much more
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Corentin M. Barbu (corentin.barbu@gmail.com), 
#  ORGANIZATION: 
#       CREATED: 09/19/2013 02:38:16 PM EDT
#      REVISION:  ---
#===============================================================================

library("roxyPackage")
authors<-"c(person(given=\"Corentin M\", family=\"Barbu\",
email=\"corentin.barbu@gmail.com\",
role=c(\"aut\", \"cre\")),
person(given=\"Sebastian\", family=\"Gibb\", role=\"ctb\",
email=\"mail@sebastiangibb.de\"))"

localRlibFile<-"localRlib.txt"
if(file.exists(localRlibFile)){
  source(localRlibFile) 
}else{
  warning(" Missing localRlib.txt file in the synlik/ directory.\n localRlib.txt should contain \n one of those:\n",paste(.libPaths(),collate="\n"))
  # cannot be specified in git archived files as it is machine/account specific

  numLocalLib<-grep(normalizePath("~"),.libPaths())
  if(!is.na(numLocalLib[1])){
    autoLocalLib<-.libPaths()[numLocalLib[1]]
    autoLocalText<-paste0("localRlib <-\"",autoLocalLib,"\"\n")
    cat(file=localRlibFile,autoLocalText)
    warning("localRlib.txt auto generated:\n",autoLocalText,"\n proceeding with caution.")
  }else{
    warning("Cannot identify your local lib folder using .libPaths() in R.\nConsider adding one and relaunching this script.\nLocal installation will not be done.")
  }


  localRlib<-"~/R"
}

roxy.package(
pck.source.dir="./zoom",
pck.version="2.0.5",
R.libs=localRlib,
repo.root="../../repo",
rm.vignette=TRUE,
pck.description=data.frame(
Package="zoom",
Type="Package",
Title="A spatial data visualization tool",
AuthorsR=authors,
Depends="R (>= 2.10.0)",
Suggests="testthat",
Description="zm(), called with any active plot allow to enter an interactive session to zoom/navigate any plot. The development version, as well as binary releases can be found at https://github.com/cbarbu/R-package-zoom",
License="GPL (>= 3)",
Encoding="UTF-8",
LazyLoad="yes",
URL="https://github.com/cbarbu/R-package-zoom",
stringsAsFactors=FALSE),
actions=c(
"roxy",
"cite",
"html",
"doc",
"package"
))
warnings()

