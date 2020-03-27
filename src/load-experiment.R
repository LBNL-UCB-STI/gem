##############################################################################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to load an experiment from a yaml file and 
# create the appropriate "exper" object containin all combinations of 
# factor levels and all meta data needed to run an experiment.
#
# Argument: experiment-definition-file
# Returns: "exper" list containing the following sub-objects:
# ----------yaml: the original yaml data
# ----------num.factors: the number of factors in the experiment
# ----------runs: the data.table of experimental runs, each row represents one model run
##############################################################################################################################################

# experiment.file <- args$experiment
# add.timestamp <- !args$notimestamp
load.experiment <- function(experiment.file,add.timestamp=T){
  if(file.exists(experiment.file)){
    exper <- list()
    exper$yaml <- yaml.load(readChar(experiment.file,file.info(args$experiment)$size))
    exper$num.factors <- length(exper$yaml$Factors)
    if(exper$yaml$Mode=='combinatorial'){
      exper$runs <- expand.grid(lapply(exper$yaml$Factors,function(fac){ fac$Levels }),stringsAsFactors=F)
      names(exper$runs) <- unlist(lapply(exper$yaml$Factors,function(fac){ fac$Name }))
      exper$runs <- data.table(exper$runs)
      exper$param.names <- names(exper$runs)
    }else if(exper$yaml$Mode=='scenario'){
      exper$runs <- rbindlist(lapply(exper$yaml$Scenarios,function(ll){ streval(pp('data.table(Scenario="',ll$Name,'",',pp(names(ll$Parameters),'=',ll$Parameters,collapse = ','),')')) }))
      exper$yaml$Factors <- list(list(Name='Scenario',Levels=exper$runs$Scenario))
      exper$num.factors <- 1
      exper$param.names <- 'Scenario'
    }else{
      stop('only combinatorial or scenario experiments enabled for now, please set "Mode: combinatorial" or "Mode: scenario" in your yaml file')
    }
    # Setup the input directory
    exper$input.dir <- pp('experiments/',exper$yaml$Name)
    if(add.timestamp)exper$input.dir <- pp(exper$input.dir,'_',format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    make.dir('experiments')
    make.dir(exper$input.dir)
    exper
  }else{
    stop(pp("Experiment file not found: ",experiment.file))
  }
}


