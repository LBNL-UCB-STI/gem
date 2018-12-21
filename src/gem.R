#!/usr/local/bin/Rscript
#####################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This master script will execute a batch experiment of runs of the GEM Model. 
# It takes as argument a path to a yaml file as input which specifies the
# factors and factor levels of the experiment. This it does the following:
#
# Load Experiment: Read and process scenario / assumptions
# Pre-Process Inputs
# Load GAMS and Run
# Post-Process Results
# 
# Argument: experiment-definition-file
#####################################################################################

#AJ Comment: Should we try to migrate away from your personal library sooner rather than later if we decide to eventually make this project public?

suppressPackageStartupMessages(library(colinmisc,quietly=T))
load.libraries(c('stringr','data.table','ggplot2','optparse','yaml','reshape','grid','gdxtools'))
igdx(gams.executable.location)

if(!exists('gem.project.directory')){
  my.cat('Error, you need to define the variable gem.project.directory in your user-level Rprofile (i.e. ~/.Rprofile). Make the varaible be the file path of the gem project directory.')
}
setwd(gem.project.directory)
source('src/load-experiment.R')
source('src/prep-inputs-static.R')
source('src/prep-inputs-common.R')
source('src/prep-inputs-mobility.R')
source('src/prep-inputs-grid.R')
source('input/defaults.R')

#####################################################################################
# PARSE COMMAND LINE OPTIONS 
#####################################################################################
option_list <- list()
if(interactive()){
  args<-'input/experiments/test.yaml'
  args <- parse_args(OptionParser(option_list = option_list,usage = "gem.R [exp-file]"),positional_arguments=T,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "gem.R [exp-file]"),positional_arguments=T)
}

#####################################################################################
# Load Experiment
#####################################################################################
exper <- load.experiment(args$args[1])

#####################################################################################
# Pre-Process Inputs
#####################################################################################
static.inputs <- prep.inputs.static()

i <- 1
for(i in 1:nrow(exper$runs)){
  my.cat(pp('Prepping inputs for run ',i))
  inputs <- list()

  common.inputs <- c(static.inputs,prep.inputs.common(exper$run[i]))
  exper.row <- exper$run[i]
  inputs.mobility <- prep.inputs.mobility(exper$run[i],common.inputs)
  inputs.grid <- prep.inputs.grid(exper$run[i],common.inputs) 

  inputs$sets <- c(common.inputs$sets,inputs.mobility$sets,inputs.grid$sets)
  inputs$parameters <- c(common.inputs$parameters,inputs.mobility$parameters,inputs.grid$parameters)

  print(inputs)

  write.gdx(pp('src/gamsScenarioFiles/inputs',i,'.gdx'),params=lapply(inputs$parameters,as.data.frame),sets=lapply(inputs$sets,as.data.frame))
}

#####################################################################################
# Load GAMS and Run
#####################################################################################

for(i in 1:nrow(exper$runs)) {
  gem.gms <- readLines('src/gem.gms')
  gem.gms <- gsub(pattern='<<gdxName>>',replace=pp('inputs',i,'.gdx'),x=gem.gms)
  writeLines(gem.gms,con=pp('src/gamsScenarioFiles/gem_',i,'.gms'))
  
  #gams(pp('src/gamsScenarioFiles/gem_',i,'.gms')
}

#####################################################################################
# Post-Process Results
#####################################################################################

