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
                    make_option(c("-e", "--experiment"), type="character", default='input/experiments/base.yaml',help="Path to experiment file [default %default]",metavar="exp"))
if(interactive()){
  args<-'input/experiments/congestion.yaml'
  args<-'input/experiments/l10ChargerCost.yaml'
  args<-'input/experiments/chargerCostSuperlinear.yaml'
  args<-'input/experiments/discountRate.yaml'
  args<-'input/experiments/fractionSmartCharging.yaml'
  args<-'input/experiments/carbonTax.yaml'
  args<-'input/experiments/renewableScalingFactor.yaml'
  args<-'input/experiments/fractionSAEVs.yaml'
  args<-'input/experiments/base.yaml'
  args <- pp('--experiment=',args)
  args <- c(args,'-t') # don't add timestamp
#  args <- c(args,'-p') # only plots
  args <- parse_args(OptionParser(option_list = option_list,usage = "gem.R [exp-file]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "gem.R [exp-file]"),positional_arguments=F)
}

#####################################################################################
# Load Experiment
#####################################################################################
exper <- load.experiment(args$experiment,!args$notimestamp)

#####################################################################################
# Pre-Process Inputs
#####################################################################################
if(!args$plots){ # only prep and run model if *not* in plot-only mode
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
    if(group.days==0){
      make.dir(pp(exper$input.dir,'/runs'))
      make.dir(pp(exper$input.dir,'/runs/run-',i))
      write.gdx(pp(exper$input.dir,'/runs/run-',i,'/inputs.gdx'),params=lapply(inputs$parameters,as.data.frame,stringsAsFactors=F),sets=lapply(inputs$sets,as.data.frame,stringsAsFactors=F))
    }else{
      days.in.groups <- lapply(seq(1,length(days),by=group.days-1),function(idx){ idx:min(length(days),(idx+group.days-1)) })
      all.t.set <- inputs$sets$t
      for(day.i in 1:length(days.in.groups)){
        hours.to.run <- days.in.groups[[day.i]]
        make.dir(pp(exper$input.dir,'/runs'))
        make.dir(pp(exper$input.dir,'/runs/run-',i))
        make.dir(pp(exper$input.dir,'/runs/run-',i,'/day-group-',day.i))
        inputs$sets$t <- all.t.set[as.vector(sapply(days.in.groups[[day.i]],function(x){ seq((x-1)*24+1,x*24)}))]
        write.gdx(pp(exper$input.dir,'/runs/run-',i,'/day-group-',day.i,'/inputs.gdx'),params=lapply(inputs$parameters,as.data.frame,stringsAsFactors=F),sets=lapply(inputs$sets,as.data.frame,stringsAsFactors=F))
      }
      inputs$sets$t <- all.t.set
    }
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
    gem.baseGeneration.gms <- readLines('src/gem-baseGeneration.gms')
    gem.baseGeneration.gms <- gsub(pattern='<<gdxName>>',replace='inputs.gdx',x=gem.baseGeneration.gms)
    if(group.days==0){
      writeLines(gem.gms,con=pp(exper$input.dir,'/runs/run-',i,'/gem.gms'))
      writeLines(gem.baseGeneration.gms,con=pp(exper$input.dir,'/runs/run-',i,'/gem-baseGeneration.gms'))
    
      cat(pp(Sys.time(),'\n'))
      setwd(pp(exper$input.dir,'/runs/run-',i))
      gams('gem.gms')
      gams('gem-baseGeneration.gms')
      setwd(gem.project.directory)
      cat(pp(Sys.time(),'\n'))
    }else{
      days.in.groups <- lapply(seq(1,length(days),by=group.days-1),function(idx){ idx:min(length(days),(idx+group.days-1)) })
      for(day.i in 1:length(days.in.groups)){
        writeLines(gem.gms,con=pp(exper$input.dir,'/runs/run-',i,'/day-group-',day.i,'/gem.gms'))
        writeLines(gem.baseGeneration.gms,con=pp(exper$input.dir,'/runs/run-',i,'/day-group-',day.i,'/gem-baseGeneration.gms'))
        cat(pp('day group ',day.i,'\n'))
        cat(pp(Sys.time(),'\n'))
        setwd(pp(exper$input.dir,'/runs/run-',i,'/day-group-',day.i))
        gams('gem.gms')
        gams('gem-baseGeneration.gms')
        setwd(gem.project.directory)
        cat(pp(Sys.time(),'\n'))
      }
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
results <- list()
for(i in 1:nrow(exper$runs)) {
  if(group.days==0){
    result <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/results.gdx')))
    result.baseGen <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/results-baseGeneration.gdx')))
    result <- merge.baseGen(result,result.baseGen)
    for(key in names(result)){
      result[[key]][,run:=i]
      if(i==1)results[[key]] <- list()
      results[[key]][[length(results[[key]])+1]] <- result[[key]]
    }
  }else{
    days.in.groups <- lapply(seq(1,length(days),by=group.days-1),function(idx){ idx:min(length(days),(idx+group.days-1)) })
    day.group.result <- list()
    for(day.i in 1:length(days.in.groups)){
      result <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/day-group-',day.i,'/results.gdx')))
      result.baseGen <- gdx.to.data.tables(gdx(pp(exper$input.dir,'/runs/run-',i,'/day-group-',day.i,'/results-baseGeneration.gdx')))
      result <- merge.baseGen(result,result.baseGen)
      for(key in names(result)){
        result[[key]][,run:=i]
      }
      day.group.result[[length(day.group.result)+1]] <- lapply(result,function(ll){
        if('t' %in% names(ll)){
          ll[t > min(t) + 11 & t <= max(t)-12]
        }else{
          ll
        }
      })
    }
    day.group.result <- sapply(names(day.group.result[[1]]),function(the.name){ rbindlist(lapply(day.group.result,function(ll){ ll[[the.name]] })) })
    for(key in names(day.group.result)){
      if(key == 'scalar'){
        day.group.result[[key]] <- day.group.result[[key]][ ,lapply(.SD, max)]
      }else if(!'t'%in%names(day.group.result[[key]])){
        keys <- str_split(key,'-')[[1]]
        not.keys <- names(day.group.result[[key]])[!names(day.group.result[[key]])%in%keys]
        day.group.result[[key]] <- streval(pp('day.group.result[[key]][,.(',pp(not.keys,'=max(',not.keys,')',collapse = ","),'),by=c("',pp(keys,collapse='","'),'")]'))
      }
      day.group.result[[key]][,run:=i]
      if(i==1)results[[key]] <- list()
      results[[key]][[length(results[[key]])+1]] <- day.group.result[[key]]
    }
  }
  make.dir(pp(plots.dir,'/run-',i,''))
}
res <- lapply(results,function(ll){ rbindlist(ll,fill=T) })

plots.mobility(exper,all.inputs,res,plots.dir)

