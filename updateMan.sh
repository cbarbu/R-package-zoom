#!/bin/bash - 
#===============================================================================
#
#          FILE: updateMan.sh
# 
#         USAGE: ./updateMan.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: YOUR NAME (), 
#  ORGANIZATION: 
#       CREATED: 09/19/2013 04:35:48 PM EDT
#      REVISION:  ---
#===============================================================================


# configuration
packageName="zoom"

# execution
set -o nounset                              # Treat unset variables as an error
Rscript -e "library(\"methods\");library(\"utils\");library(\"devtools\");document(\"$packageName\",clean=TRUE,reload=TRUE)"
R CMD Rd2pdf --force --no-preview $packageName

