#!/usr/bin/env/Rscript
options(repos=structure(c(CRAN="https://ftp.osuosl.org/pub/cran/")))

## First specify the packages of interest
packages = c('stringr','data.table','ggplot2','optparse','yaml','reshape','grid','gdxtools','lubridate','RColorBrewer','forcats','gtools','sf','cowplot','tidyr','dplyr','maps')

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    is.installed <- require(x, character.only = TRUE)
    if (!is.installed){
      install.packages(x, dependencies = TRUE)
    }
    library(x, character.only = TRUE)
  }
)
