#!/usr/local/bin/Rscript
#####################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This master script will execute a batch experiment of runs of the GEM Model. 
# It takes as argument a path to a yaml file as input which specifies the
# factors and factor levels of the experiment. Then it does the following:
#
# Load Experiment: Read and process scenario / assumptions
# Pre-Process Inputs
# Load GAMS and Run
# Post-Process Results
# 
# Argument: experiment-definition-file
#####################################################################################


#####################################################################################
# INITIALIZING DIRECTORIES AND PACKAGES
#####################################################################################

if(!exists('gem.project.directory')){
  my.cat('Error, you need to define the variable gem.project.directory in your user-level Rprofile (i.e. ~/.Rprofile). Make the variable be the file path of the gem project directory.')
}
if(!exists('gams.executable.location')){
  my.cat('Error, you need to define the variable gams.executable.location in your user-level Rprofile (i.e. ~/.Rprofile). Make the variable be the file path to the "sysdir" sub-directory of the GAMS application.')
}
if(!exists('gem.raw.inputs')){
  my.cat('Error, you need to define the variable gem.raw.inputs in your user-level Rprofile (i.e. ~/.Rprofile). Make the variable be the file path of the gem-inputs-public directory.')
}
setwd(gem.project.directory)
source('src/includes.R')

# igdx is a function that points to the location of the GAMS executable
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

# The "option_list" provides a list of options when running the model externally in a console environment (e.g. CMD in Windows or Terminal in MacOS)

option_list <- list(make_option(c("-p", "--plots"), action="store_true", default=F,help="Only run code that produces plots, requires results to be already present in outputs [default %default]"),
                    make_option(c("-t", "--notimestamp"), action="store_true", default=F,help="Don't add timestamp to outputs directory [default %default]"),
                    make_option(c("-d", "--trimdays"), action="store_true", default=F,help="Trim a day off beginning and end of simulation results to avoid edge effects [default %default]"),
                    make_option(c("-e", "--experiment"), type="character", default='input/experiments/base.yaml',help="Path to experiment file [default %default]",metavar="exp"),
                    make_option(c("-r", "--runsubset"), type="character", default='',help="Comma separate list of runs to execute [default %default]"),
                    make_option(c("-o", "--overwrite"), action="store_true", default=F,help="Overwrite an existing solution from GAMS [default %default]"))

# Set the scenario run as a yaml file in the "input/experiments" folder (the first argument within the if statement).
# Each experiment sets a sensitivity sweep on either one or two parameters, multi-dimensional sensitivity sweeps will run all combinations of parameter values

if(interactive()){
  args <- 'input/experiments/base.yaml'
  args <- pp('--experiment=',args)
  args <- c(args,'-t') # don't add timestamp
  args <- c(args,'-p') # only plots
  # args <- c(args,'-d') # trim one day off beginning and end of results
  #args <- c(args,'-o') # overwrite existing
  #args <- c(args,'--runsubset=16,17,18,19')
  #args <- c(args,'--runsubset=4') 
  args <- parse_args(OptionParser(option_list = option_list,usage = "gem.R [exp-file]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "gem.R [exp-file]"),positional_arguments=F)
}

#####################################################################################
# Load Experiment
#####################################################################################
exper <- load.experiment(args$experiment,!args$notimestamp)
if(args$runsubset==''){
  runs.to.run <- 1:nrow(exper$runs)
}else{
  runs.to.run <- unlist(lapply(str_split(args$runsubset,","),as.numeric))
}
#####################################################################################
# Pre-Process Inputs
#####################################################################################
if(!args$plots){ # only prep and run model if *not* in plot-only mode
  
  # Within this if statement and for loop below, the inputs are prepared for the model
  # Inputs are built from functions among several categories: common inputs, mobility, charging, and grid.  These functions can be found in their respective "prep-inputs-XYZ.R" files
  # Inputs are written to a .gdx file via the "write.gdx" function.  The gdx file serves as the input format for the GAMS optimization model

  static.inputs <- prep.inputs.static()
  
  all.inputs <- list()
  i <- runs.to.run[1]
  for(i in runs.to.run){
    cat(pp('Prepping inputs for run ',i,'\n'))
    inputs <- list()
  
    common.inputs <- c(static.inputs,prep.inputs.common(exper$run[i],exper$param.names))
    exper.row <- exper$run[i]
    inputs.mobility <- prep.inputs.mobility(exper$run[i],exper$param.names,common.inputs)
    inputs.personal.charging <- prep.inputs.personal.charging(exper$run[i],exper$param.names,common.inputs,inputs.mobility)
    inputs.grid <- prep.inputs.grid(exper$run[i],exper$param.names,common.inputs) 
  
    inputs$sets <- c(common.inputs$sets,inputs.mobility$sets,inputs.grid$sets,inputs.personal.charging$sets)
    inputs$parameters <- c(common.inputs$parameters,inputs.mobility$parameters,inputs.grid$parameters,inputs.personal.charging$parameters)
  
    make.dir(pp(exper$input.dir,'/runs'))
    make.dir(pp(exper$input.dir,'/runs/run-',i))
    write.gdx(pp(exper$input.dir,'/runs/run-',i,'/inputs.gdx'),params=lapply(inputs$parameters,as.data.frame,stringsAsFactors=F),sets=lapply(inputs$sets,as.data.frame,stringsAsFactors=F))
    save(inputs,file=pp(exper$input.dir,'/runs/run-',i,'/inputs.Rdata'))
    all.inputs[[length(all.inputs)+1]] <- inputs
  }

  # Below, the inputs are saved as a .Rdata file so when a scenario is run again, the input preparation process is not needed

  if(args$runsubset=='')save(all.inputs,file=pp(exper$input.dir,'/inputs.Rdata'))
  # Save the defaults.R file for future reference
  file.copy('input/defaults.R',exper$input.dir)
  
  #####################################################################################
  # Load GAMS and Run
  #####################################################################################
  
  # Below, the code initializes the optimization model and runs GAMS via the "gams" function

  for(i in runs.to.run) {
    Sys.sleep(0.1) # Allow console statements to print to screen before continuing
    if(args$overwrite | !file.exists(pp(exper$input.dir,'/runs/run-',i,'/results.gdx'))){
      cat(pp('Running run-',i,' ',pp(names(exper$runs),': ',exper$runs[i],collapse=', '),'\n'))
      gem.gms <- readLines('src/gem.gms')
      gem.gms <- gsub(pattern='<<gdxName>>',replace='inputs.gdx',x=gem.gms)
      gem.baseGeneration.gms <- readLines('src/gem-baseGeneration.gms')
      gem.baseGeneration.gms <- gsub(pattern='<<gdxName>>',replace='inputs.gdx',x=gem.baseGeneration.gms)
      writeLines(gem.gms,con=pp(exper$input.dir,'/runs/run-',i,'/gem.gms'))
      writeLines(gem.baseGeneration.gms,con=pp(exper$input.dir,'/runs/run-',i,'/gem-baseGeneration.gms'))
    
      cat(pp(Sys.time(),'\n'))
      setwd(pp(exper$input.dir,'/runs/run-',i))
      gams('gem.gms solvelink=0')
      print('Full GEM results:')
      print.lst.status('gem.lst')
      gams('gem-baseGeneration.gms solvelink=0')
      print('Base generation results:')
      print.lst.status('gem-baseGeneration.lst')
      setwd(gem.project.directory)
      cat(pp(Sys.time(),'\n'))
    }
  }

# If the model has already been run, then the inputs can simply be loaded from the .Rdata file

}else{
  load(file=pp(exper$input.dir,'/inputs.Rdata'))
}

#####################################################################################
# Post-Process Results
#####################################################################################
plots.dir <- pp(exper$input.dir,'/plots/')
make.dir(plots.dir)
write.csv(exper$runs,pp(plots.dir,'runs.csv'),row.names=T)

# Below the results are parsed from gdx outputs in the GAM run into the res object

results <- list(); i<-1
for(i in 1:nrow(exper$runs)) {
  result <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/results.gdx')))
  result.baseGen <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/results-baseGeneration.gdx')))
  result <- merge.baseGen(result,result.baseGen); key<-names(result)[1]
  for(key in names(result)){
    result[[key]][,run:=i]
    if('t'%in%names(result[[key]]) & args$trimdays){
      max.t <- max(result[[key]]$t)
      result[[key]] <- result[[key]][t>24 & t<=max.t-24]
      result[[key]][,t:=t-24]
    }
    if(i==1)results[[key]] <- list()
    results[[key]][[length(results[[key]])+1]] <- result[[key]]
  }
  if(args$trimdays){
    all.inputs[[i]]$sets$t <- all.inputs[[i]]$set$t[1:(length(all.inputs[[i]]$set$t)-48)]
  }
  make.dir(pp(plots.dir,'/run-',i,''))
}
res <- lapply(results,function(ll){ rbindlist(ll,fill=T) })

# plots.mobility will take the results and provide a series of plotted outputs

plots.mobility(exper,all.inputs,res,plots.dir)
