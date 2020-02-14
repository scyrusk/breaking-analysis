resource <- function() {
  source('constants.R')
  source('rebase.R')
  source('summary_stats.R')
  source('utility.R')
  source('plotter.R')
  source('rq1.R')
  source('rq2.R')
  source('rq3.R')
  source('coded_events.R')
  source('functions.R')
  import.dependencies()
}

import.dependencies <- function() {
  library(MCMCglmm)
  library(ggplot2)
  library(lme4)
  library(plyr)
  library(reshape2)
  library(apcluster)
}