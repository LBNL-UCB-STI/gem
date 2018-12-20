#############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs required by the grid model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
#############################################################################################

prep.inputs.grid <- function(exper.row,common.inputs){
  param.names <- names(exper.row)

  inputs <- list()
  inputs$sets <- list()
  inputs$parameters <- list()

  ##### GENERATION CAPACITIES #####
  generators.Cap <- generators 
  if('renewableScalingFactor'%in%param.names) {
  	generators.Cap$generationCapacities[generators.Cap$FuelType%in%c('Solar','Wind')] <- generators.Cap$generationCapacities[generators.Cap$FuelType%in%c('Solar','Wind')]*exper.row$renewableScalingFactor
  	generators.Cap <- generators.Cap[,list(g,generationCapacities)]
  }else{
  	generators.Cap <- generators.Cap[,list(g,generationCapacities)]
  }
  names(generators.Cap)[names(generators.Cap)=='generationCapacities'] <- 'value'
  inputs$parameters$maxGen <- generators.Cap

  ##### MAX RENEWABLES #####
  total.Renewables <- generators[generators$FuelType%in%c('Hydro','Wind','Solar'),list(generationCapacities=sum(generationCapacities)),by=list(r,FuelType)]
  maxSolar <- merge(x=renewableCF,y=total.Renewables[total.Renewables$FuelType=='Solar',],by='r')
  maxSolar$value <- maxSolar$generationCapacities*maxSolar$solarCF
  maxSolar <- maxSolar[,list(t,r,value)]
  maxWind <- merge(x=renewableCF,y=total.Renewables[total.Renewables$FuelType=='Wind',],by='r')
  maxWind$value <- maxWind$generationCapacities*maxWind$windCF
  maxWind <- maxWind[,list(t,r,value)]

  inputs$parameters$maxSolar <- maxSolar
  inputs$parameters$maxWind <- maxWind

  ##### GENERATION COSTS #####
  generators.Cost <- generators
  if('carbonTax'%in%param.names) {
  	generators.Cost$generationCosts <- generators.Cost$generationCosts+generators.Cost$generationCO2*carbonTax
  	generators.Cost <- generators.Cost[,list(g,generationCosts)]
  }else{
  	generators.Cost <- generators.Cost[,list(g,generationCosts)]
  }
  names(generators.Cost)[names(generators.Cost)=='generationCosts'] <- 'value'
  inputs$parameters$genCost <- generators.Cost



  inputs
}

