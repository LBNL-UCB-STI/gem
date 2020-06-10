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
option_list <- list(make_option(c("-p", "--plots"), action="store_true", default=F,help="Only run code that produces plots, requires results to be already present in outputs [default %default]"),
                    make_option(c("-t", "--notimestamp"), action="store_true", default=F,help="Don't add timestamp to outputs directory [default %default]"),
                    make_option(c("-d", "--trimdays"), action="store_true", default=F,help="Trim a day off beginning and end of simulation results to avoid edge effects [default %default]"),
                    make_option(c("-e", "--experiment"), type="character", default='input/experiments/base.yaml',help="Path to experiment file [default %default]",metavar="exp"),
                    make_option(c("-r", "--runsubset"), type="character", default='',help="Comma separate list of runs to execute [default %default]"),
                    make_option(c("-o", "--overwrite"), action="store_true", default=F,help="Overwrite an existing solution from GAMS [default %default]"))
if(interactive()){
#  args<-'input/experiments/fractionSAEVsAndSmartCharging.yaml'
 # args<-'input/experiments/base.yaml'
#  args<-'input/experiments/smartMobility.yaml'
   # args<-'input/experiments/batteryLifetime.yaml'
   # args<-'input/experiments/sharingFactor.yaml'
   args<-'input/experiments/vehicleCapitalCost.yaml'
  # args<-'input/experiments/b150ConversionEfficiency.yaml'
  # args<-'input/experiments/conversionEfficiency.yaml'
  # args<-'input/experiments/electrificationPenetration.yaml'
  args <- pp('--experiment=',args)
 args <- c(args,'-t') # don't add timestamp
 args <- c(args,'-p') # only plots
 args <- c(args,'-d') # trim one day off beginning and end of results
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
  
    #print(inputs)
    make.dir(pp(exper$input.dir,'/runs'))
    make.dir(pp(exper$input.dir,'/runs/run-',i))
    write.gdx(pp(exper$input.dir,'/runs/run-',i,'/inputs.gdx'),params=lapply(inputs$parameters,as.data.frame,stringsAsFactors=F),sets=lapply(inputs$sets,as.data.frame,stringsAsFactors=F))
    save(inputs,file=pp(exper$input.dir,'/runs/run-',i,'/inputs.Rdata'))
    all.inputs[[length(all.inputs)+1]] <- inputs
  }
  if(args$runsubset=='')save(all.inputs,file=pp(exper$input.dir,'/inputs.Rdata'))
  
  #####################################################################################
  # Load GAMS and Run
  #####################################################################################
  
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
}else{
  load(file=pp(exper$input.dir,'/inputs.Rdata'))
}

#####################################################################################
# Post-Process Results
#####################################################################################
plots.dir <- pp(exper$input.dir,'/plots/')
make.dir(plots.dir)
write.csv(exper$runs,pp(plots.dir,'runs.csv'),row.names=T)
results <- list(); i<-1
for(i in 1:nrow(exper$runs)) {
  result <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/results.gdx')))
  result.baseGen <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/results-baseGeneration.gdx')))
  result <- merge.baseGen(result,result.baseGen)
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

plots.mobility(exper,all.inputs,res,plots.dir)
