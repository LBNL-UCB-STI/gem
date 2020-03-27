#############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs required by the grid model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
#############################################################################################

prep.inputs.grid <- function(exper.row,param.names,common.inputs){
  inputs <- list()
  inputs$sets <- list()
  inputs$parameters <- list()
  
  ##### TRANSMISSION CAPACITIES / COSTS #####
  trans <- copy(transmission)
  names(trans) <- c('r1','r2','transCap','transCost')
  if('transmissionScalingFactor'%in%param.names){
    transmissionScalingFactor <- exper.row$transmissionScalingFactor
  }
  trans[,transCap:=transCap*transmissionScalingFactor]
  transCap <- trans[,list(r1,r2,transCap)]
  names(transCap) <- c('r','o','value')
  transCost <- trans[,list(r1,r2,transCost)]
  names(transCost) <- c('r','o','value')
  inputs$parameters$transCap <- transCap
  inputs$parameters$transCost <- transCost

  ##### GENERATION CAPACITIES #####
  generators.Cap <- generators 
  if('renewableScalingFactor'%in%param.names) {
  	generators.Cap$generationCapacities[generators.Cap$FuelType%in%c('Solar','Wind')] <- generators.Cap$generationCapacities[generators.Cap$FuelType%in%c('Solar','Wind')]*as.numeric(exper.row$renewableScalingFactor)
  	generators.Cap <- generators.Cap[,list(g,generationCapacities)]
  }else{
  	generators.Cap <- generators.Cap[,list(g,generationCapacities)]
  }
  names(generators.Cap)[names(generators.Cap)=='generationCapacities'] <- 'value'
  inputs$parameters$maxGen <- generators.Cap
  
  ##### MAX RENEWABLES #####
  renewableCF[,t:=pp('t',sprintf('%04d',as.numeric(substr(as.character(t),2,nchar(as.character(t))))))]
  setkey(renewableCF,r,t)
  total.Renewables <- generators[generators$FuelType%in%c('Hydro','Wind','Solar'),list(generationCapacities=sum(generationCapacities)),by=list(r,FuelType)]
  if('renewableScalingFactor'%in%param.names) {
    total.Renewables$generationCapacities <- total.Renewables$generationCapacities*as.numeric(exper.row$renewableScalingFactor)
  }
  maxSolar <- merge(x=renewableCF,y=total.Renewables[total.Renewables$FuelType=='Solar',],by='r')
  maxSolar$value <- maxSolar$generationCapacities*maxSolar$solarCF
  maxSolar <- maxSolar[,list(r,t,value)]
  maxSolar <- maxSolar[maxSolar$t%in%common.inputs$sets$t,]

  maxWind <- merge(x=renewableCF,y=total.Renewables[total.Renewables$FuelType=='Wind',],by='r')
  maxWind$value <- maxWind$generationCapacities*maxWind$windCF
  maxWind <- maxWind[,list(r,t,value)]
  maxWind <- maxWind[maxWind$t%in%common.inputs$sets$t,]

  inputs$parameters$maxSolar <- maxSolar
  inputs$parameters$maxWind <- maxWind

  ##### GENERATION COSTS #####
  generators.Cost <- generators
  if('carbonTax'%in%param.names) {
  	generators.Cost$generationCosts <- generators.Cost$generationCosts+generators.Cost$generationCO2*exper.row$carbonTax
  	generators.Cost <- generators.Cost[,list(g,generationCosts)]
  }else{
  	generators.Cost <- generators.Cost[,list(g,generationCosts)]
  }
  names(generators.Cost)[names(generators.Cost)=='generationCosts'] <- 'value'
  inputs$parameters$genCost <- generators.Cost

  # Helpful plot to see capacities
  #ggplot(generators[,.(Capacity=generationCapacities/1e3),by=c('FuelType','r')],aes(x=r,y=Capacity,fill=fct_rev(FuelType)))+geom_bar(stat='identity')+labs(x="Region",y="Capacity (GW)",fill="Fuel Type")


  inputs
}

