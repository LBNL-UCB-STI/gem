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

suppressPackageStartupMessages(library(colinmisc,quietly=T))
load.libraries(c('stringr','data.table','ggplot2','optparse','yaml'))

if(!exists('gem.project.directory')){
  my.cat('Error, you need to define the variable gem.project.directory in your user-level Rprofile (i.e. ~/.Rprofile). Make the varaible be the file path of the gem project directory.')
}
setwd(gem.project.directory)
source('src/load-experiment.R')
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
inputs <- c(prep.inputs.mobility(exper),prep.inputs.grid(exper))

#####################################################################################
# Load GAMS and Run
#####################################################################################

#####################################################################################
# Post-Process Results
#####################################################################################

