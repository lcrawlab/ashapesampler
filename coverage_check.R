#Checks coverage of ashapesampler test scripts
#Restart R before running script

library(devtools)
library(testthat)
library(covr)

cover <- package_coverage("C:/Users/etwin/git_repos/ashapesampler")
report(cover)
