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

load.libraries(c('yaml'))

load.experiment <- function(experiment.file){
  if(file.exists(experiment.file)){
    exper <- list()
    exper$yaml <- yaml.load(readChar(experiment.file,file.info(args$args[1])$size))
    exper$num.factors <- length(exper$yaml$Factors)
    if(exper$yaml$Mode=='combinatorial'){
      exper$runs <- expand.grid(lapply(exper$yaml$Factors,function(fac){ fac$Levels }),stringsAsFactors=F)
      names(exper$runs) <- unlist(lapply(exper$yaml$Factors,function(fac){ fac$Name }))
      exper$runs <- data.table(exper$runs)
    }else{
      stop('only combinatorial experiments enabled for now, please set "Mode: combinatorial" in your yaml file')
    }
    exper
  }else{
    stop(pp("Experiment file not found: ",experiment.file))
  }
}

