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

if(!exists('gem.project.directory')){
  my.cat('Error, you need to define the variable gem.project.directory in your user-level Rprofile (i.e. ~/.Rprofile). Make the varaible be the file path of the gem project directory.')
}
setwd(gem.project.directory)
source('src/includes.R')
igdx(gams.executable.location)
source('src/load-experiment.R')
source('src/prep-inputs-static.R')
source('src/prep-inputs-common.R')
source('src/prep-inputs-mobility.R')
source('src/prep-inputs-personal-charging.R')
source('src/prep-inputs-grid.R')
source('src/plots-mobility.R')
source('input/defaults.R')

#####################################################################################
# PARSE COMMAND LINE OPTIONS 
#####################################################################################
option_list <- list()
if(interactive()){
  args<-'input/experiments/fractionSAEVs.yaml'
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

all.inputs <- list()
i <- 1
for(i in 1:nrow(exper$runs)){
  cat(pp('Prepping inputs for run ',i,'\n'))
  inputs <- list()

  common.inputs <- c(static.inputs,prep.inputs.common(exper$run[i]))
  exper.row <- exper$run[i]
  inputs.mobility <- prep.inputs.mobility(exper$run[i],common.inputs)
  inputs.personal.charging <- prep.inputs.personal.charging(exper$run[i],common.inputs,inputs.mobility)
  inputs.grid <- prep.inputs.grid(exper$run[i],common.inputs) 

  inputs$sets <- c(common.inputs$sets,inputs.mobility$sets,inputs.grid$sets,inputs.personal.charging$sets)
  inputs$parameters <- c(common.inputs$parameters,inputs.mobility$parameters,inputs.grid$parameters,inputs.personal.charging$parameters)

  #print(inputs)
  make.dir(pp(exper$input.dir,'/runs'))
  make.dir(pp(exper$input.dir,'/runs/run-',i))
  write.gdx(pp(exper$input.dir,'/runs/run-',i,'/inputs.gdx'),params=lapply(inputs$parameters,as.data.frame,stringsAsFactors=F),sets=lapply(inputs$sets,as.data.frame,stringsAsFactors=F))
  all.inputs[[length(all.inputs)+1]] <- inputs
}
save(all.inputs,file=pp(exper$input.dir,'/inputs.Rdata'))

#####################################################################################
# Load GAMS and Run
#####################################################################################

Sys.sleep(0.1) # Allow console statements to print to screen before continuing
for(i in 1:nrow(exper$runs)) {
  cat(pp('Running [',i,'] ',exper$runs[i],'\n'))
  gem.gms <- readLines('src/gem.gms')
  gem.gms <- gsub(pattern='<<gdxName>>',replace='inputs.gdx',x=gem.gms)
  writeLines(gem.gms,con=pp(exper$input.dir,'/runs/run-',i,'/gem.gms'))
  
  cat(pp(Sys.time(),'\n'))
  setwd(pp(exper$input.dir,'/runs/run-',i))
  gams('gem.gms')
  setwd(gem.project.directory)
  cat(pp(Sys.time(),'\n'))
}

#####################################################################################
# Post-Process Results
#####################################################################################
plots.dir <- pp(exper$input.dir,'/plots/')
make.dir(plots.dir)
results <- list()
for(i in 1:nrow(exper$runs)) {
  result <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/results.gdx')))
  for(key in names(result)){
    result[[key]][,run:=i]
    if(i==1)results[[key]] <- list()
    results[[key]][[length(results[[key]])+1]] <- result[[key]]
  }
  make.dir(pp(plots.dir,'/run-',i,''))
}
res <- lapply(results,function(ll){ rbindlist(ll) })

plots.mobility(exper,inputs,res,plots.dir)

