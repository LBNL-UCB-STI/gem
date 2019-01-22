###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs required by the mobility model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
###############################################################################################

prep.inputs.mobility <- function(exper.row,common.inputs){
  param.names <- names(exper.row)

  inputs <- list()
  inputs$sets <- list()
  inputs$parameters <- list()

  ##### GENERIC PROCESSING OF SIMPLE PARAMS #####

  generic.params <- c('batteryCapitalCost','discountRate','chargerLifetime')

  for(generic.param in generic.params){
    if(generic.param %in% param.names){
      streval(pp('inputs$parameters$',generic.param,' <- data.table(value=exper.row$',generic.param,')'))
    }else{
      streval(pp('inputs$parameters$',generic.param,' <- data.table(value=',generic.param,')'))
    }
  }

  if('fractionSAEVs' %in% names(exper.row)){
    fractionSAEVs <- exper.row$fractionSAEVs
  }

  ##### SHARING #####
  if('sharingFactor'%in%param.names){
    inputs$parameters$sharingFactor <- data.table(d=common.inputs$sets$d,value=exper.row$sharing)
  }else{
    inputs$parameters$sharingFactor <- data.table(value=sharingFactor)
  }

  ##### URBAN FORM - STRAIGHT SCALING ALL REGIONS #####
  if('scale.urban.form.factor' %in% param.names){
    inputs$parameters$urbanFormFactor <- data.table(rmob=common.inputs$sets$rmob,value=exper.row$scale.urban.form.factor * 1.3)
  }else{
    inputs$parameters$urbanFormFactor <- data.table(rmob=common.inputs$sets$rmob,value=1.3)
  }

  #### DEMAND ####
  if(F){
    # The following is only needed to reshape the data from Brian Gerke's python script into a single data frame, so only used when that source data is updated
    dfs <- list()
    for(file in grep('Rdata',grep('dist_hour_hists',list.files(gem.raw.inputs),value=T),invert=T,value=T)){
      df <- data.table(read.csv(pp(gem.raw.inputs,file),stringsAsFactors=F))
      df[,season:=str_split(file,"_")[[1]][4]]
      df[,transit:=str_split(file,"_")[[1]][5]]
      dfs[[length(dfs)+1]] <- df
    }
    dem <- rbindlist(dfs)
    save(dem,file=pp(gem.raw.inputs,'dist_hour_hists.Rdata'))
  }
  load(pp(gem.raw.inputs,'dist_hour_hists.Rdata'))
  dem[,dist:=AVGDIST]
  dem[,d:=MILEBIN]
  dem[,r:=pp(CDIVLS,'-',URBRURS)]
  dem[,':='(MILEBIN=NULL,CDIVLS=NULL,URBRURS=NULL,AVGDIST=NULL,NRAW=NULL,NWTD=NULL)]
  dem[,':='(day.type=WKTIME,WKTIME=NULL)]
  dem <- melt(dem,id.vars=c('d','dist','season','transit','r','day.type'),variable.name='t',value.name='trips')
  dem[,t:=as.numeric(unlist(lapply(str_split(t,"X"),function(x){x[2]})))]
  dem[,use.transit:=transit=='with']
  #ggplot(dem[,.(trips=sum(trips)),by=c('t','d','season','transit')],aes(x=t,y=trips,colour=d))+geom_line()+facet_grid(season~transit)


  #for a truly weighted average distance by bin, weight the day.type
  dem[,weighted.trips:=trips*ifelse(day.type=="TU/WE/TH",3,2)]

  # Day of the week for the days in the simulated year 
  dates <- date.info(days,year)
  all.dem <- list()
  for(i in 1:length(days)){
    the.dem <- copy(dem[day.type==dates$day.types[i] & use.transit == include.transit.demand & season == dates$seasons[i]])
    the.dem[,t:=pp('t',sprintf('%04d',1 + t + 24*(i-1)))]
    all.dem[[length(all.dem)+1]] <- the.dem[,.(r,t,d,trips)]
  }
  all.dem <- rbindlist(all.dem)
  all.dem[,':='(value=trips,trips=NULL,rmob=r,r=NULL)]
  all.dem[t==tail(common.inputs$sets$t,1),value:=0]
  all.dem.unscaled <- all.dem[,list(t,d=pp('d',d),rmob,value=value)]
  all.dem <- all.dem[,list(t,d=pp('d',d),rmob,value=value*fractionSAEVs)]
  inputs$parameters$demand <- all.dem
  inputs$parameters$demandUnscaled <- all.dem.unscaled

  ##### DISTANCE BINS #####
  inputs$sets$d <- pp('d',sort(u(dem$d)))
  inputs$parameters$travelDistance <- dem[,.(d=pp('d',d),value=weighted.mean(dist,weighted.trips)),by=c('d','r')]
  inputs$parameters$travelDistance[,':='(d=NULL,rmob=r,r=NULL)]
  inputs$parameters$travelDistance <- inputs$parameters$travelDistance[,list(d,rmob,value)]

  #### SPEED ####
  if('congestion'%in%param.names){
    congestion <- exper.row$congestion
  }
  speed.by.dist <- data.table(d=c("d0-2","d2-5","d5-10","d10-20","d20-30","d30-50","d50-100","d100-300"),value=c(18,22,32,38,40,45,48,48))
  speeds <- data.table(expand.grid(list(common.inputs$sets$rmob,common.inputs$sets$t,inputs$sets$d)))
  names(speeds) <- c('rmob','t','d')
  speeds <- join.on(speeds,speed.by.dist,'d','d')
  setkey(speeds,rmob,d,t)
    speed.scale.base <- c(
      1,1,1,1,1,0.99,
      0.9,0.8,0.7,0.8,0.85,0.85,
      0.85,0.85,0.8,0.7,0.6,0.55,
      0.7,0.75,0.9,0.95,0.99,1)
  if(congestion=='Freeflow'){
    speed.scale <- 1
  }else if(congestion=='Light'){
    speed.scale <- 1-(1-speed.scale.base)*.5
  }else if(congestion=='Medium'){
    speed.scale <- speed.scale.base
  }else if(congestion=='Heavy'){
    speed.scale <- 1-(1-speed.scale.base)*1.5
  }
  speeds[,value:=value*speed.scale]
  inputs$parameters$speed <- speeds[,.(t=t,d,rmob,value)]

  ##### CHARGING INFRASTRUCTURE #####
  charger.levels.str <- pp('L',str_pad(charger.levels,3,'left','0'))
  inputs$sets$l <- charger.levels.str
  #### CHARGER CAPITAL ####
  if('l10ChargerCost'%in%param.names){
    l10ChargerCost <- exper.row$l10ChargerCost
  }
  if('chargerCostSuperlinear'%in%param.names){
    chargerCostSuperlinear <- exper.row$chargerCostSuperlinear
  }
  chargerCapitalCost <- l10ChargerCost + c(charger.levels-min(charger.levels))*chargerCostSuperlinear
  inputs$parameters$chargerCapitalCost <- data.table(l=charger.levels.str,value=chargerCapitalCost)
  #### CHARGER POWER ####
  inputs$parameters$chargerPower <- data.table(l=charger.levels.str,value=charger.levels)
  #### CHARGER DISTRBITUION FACTOR ####
  inputs$parameters$chargerDistributionFactor <- data.table(l=charger.levels.str,value=1)

  #### DEMAND CHARGES ####
  if('demandCharge'%in%param.names){
    inputs$parameters$demandCharge <- data.table(rmob=common.inputs$sets$rmob,value=exper.row$demandCharge)
  }else{
    inputs$parameters$demandCharge <- data.table(rmob=common.inputs$sets$rmob,value=7.7)
  }
  
  ### ECON AND ADJUSTMENT FACTORS ###
  cached.raw.factors <- pp(gem.raw.inputs,'/econ-and-scaling-factors/cached-factors.Rdata')
  the.files <- grep(".csv",list.files(pp(gem.raw.inputs,'/econ-and-scaling-factors')),value=T)
  the.params <- lapply(str_split(the.files,".csv"),function(ll){ ll[1] })
  if(file.exists(cached.raw.factors)){
    load(cached.raw.factors)
  }else{
    for(i in 1:length(the.files)){
      the.file <- the.files[i]
      the.param <- the.params[i]
      streval(pp(the.param," <- data.table(read.csv('",pp(gem.raw.inputs,'/econ-and-scaling-factors/',the.file),"',stringsAsFactors=F))"))
    }
    streval(pp('save(',pp(the.params,collapse=","),',file="',cached.raw.factors,'")'))
  }
  for(the.param in the.params){
    streval(pp('inputs$parameters$',the.param,' <- ',the.param))
  }
  
  inputs
}

