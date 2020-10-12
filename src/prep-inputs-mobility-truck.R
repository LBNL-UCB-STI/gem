###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs required by the mobility model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
###############################################################################################

prep.inputs.mobility.truck <- function(exper.row,param.names,common.inputs){
  param.names <- names(exper.row)
  

  inputs <- list()
  inputs$sets <- list()
  inputs$parameters <- list()

  ##### GENERIC PROCESSING OF SIMPLE PARAMS #####

  # generic.params <- c('batteryCapitalCost','discountRate','chargerLifetime','vehicleCapitalCost','sharingFactor')
  # 
  # for(generic.param in generic.params){
  #   if(generic.param %in% param.names){
  #     streval(pp('inputs$parameters$',generic.param,' <- data.table(value=exper.row$',generic.param,')'))
  #   }else{
  #     streval(pp('inputs$parameters$',generic.param,' <- data.table(value=',generic.param,')'))
  #   }
  # }

  if('electrificationPenetration' %in% param.names){
    electrificationPenetration <- exper.row$electrificationPenetration
  }
  if('fractionSAEVs' %in% param.names){
    fractionSAEVs <- exper.row$fractionSAEVs
  }
  if('vmtReboundFactor' %in% param.names){
    vmtReboundFactor <- exper.row$vmtReboundFactor
  }

  ##### URBAN FORM - STRAIGHT SCALING ALL REGIONS #####
  # if('scale.urban.form.factor' %in% param.names){
  #   inputs$parameters$urbanFormFactor <- data.table(rmob=common.inputs$sets$rmob,value=exper.row$scale.urban.form.factor * 1.3)
  # }else{
  #   inputs$parameters$urbanFormFactor <- data.table(rmob=common.inputs$sets$rmob,value=1.3)
  # }
  
  ##### VEHICLE CONVERSION EFFICIENCY - kwh/mile #####
  # We original varied from 0.262-0.310 per ES&T paper, but changed to center around 0.325 to match EVI-Pro assumptions
  #	kwh per mile /	b075  b150  b225  b300	b400
  #	TRB Paper 2019	0.262 0.274 0.286 0.298	0.310 /									   
  # 2nd Paper 2020	0.31  0.324 0.338 0.351 0.353
  conversion.efficiency.by.range <- c(.360,.373,.379,.391,.399)
  if('b150ConversionEfficiency' %in% param.names){
    inputs$parameters$truckconversionEfficiency <- data.table(tb=c('tb0500','tb0800','tb1000','tb1500','tb2000'),value=conversion.efficiency.by.range*exper.row$b150ConversionEfficiency/.324)
  }else{
    inputs$parameters$truckconversionEfficiency <- data.table(tb=c('tb0500','tb0800','tb1000','tb1500','tb2000'),value=conversion.efficiency.by.range)
  }
  # / b075 0.31
  # b150 0.324
  # b225 0.338
  # b300 0.351
  # b400 0.353 /

  #### DEMAND ####
  if(F){
    # The following is only needed to reshape the data from Brian Gerke's python script into a single data frame, so only used when that source data is updated
    dfs <- list()
    for(file in grep('Rdata',grep('dist_hour_hists',list.files(pp(gem.raw.inputs,'trucks/nhts/')),value=T),invert=T,value=T)){
      df <- data.table(read.csv(pp(gem.raw.inputs,'trucks/nhts/',file),stringsAsFactors=F))
      df[,season:=str_split(file,"_")[[1]][4]]
      df[,transit:=str_split(file,"_")[[1]][5]]
      dfs[[length(dfs)+1]] <- df
    }
    dem <- rbindlist(dfs)
    save(dem,file=pp(gem.raw.inputs,'trucks/nhts/','dist_hour_hists_trucks.Rdata'))
  }
  load(pp(gem.raw.inputs,'trucks/nhts/dist_hour_hists_trucks.Rdata'))
  dem[,dist:=AVGDIST]
  dem[,td:=MILEBIN]
  dem[,r:=pp(CDIVLS,'-',URBRURS)]
  dem[,':='(MILEBIN=NULL,CDIVLS=NULL,URBRURS=NULL,AVGDIST=NULL,NRAW=NULL,NWTD=NULL)]
  dem[,':='(day.type=WKTIME,WKTIME=NULL)]
  dem <- data.table(melt(dem,id.vars=c('td','dist','season','transit','r','day.type'),variable.name='t',value.name='trips'))
  names(dem) <- c('td','dist','season','transit','r','day.type','t','trips')
  dem[,t:=as.numeric(unlist(lapply(str_split(t,"X"),function(x){x[2]})))]
  dem[,use.transit:=transit=='with']
  #ggplot(dem[,.(trips=sum(trips)),by=c('t','d','season','transit')],aes(x=t,y=trips,colour=d))+geom_line()+facet_grid(season~transit)


  #for a truly weighted average distance by bin, weight the day.type
  dem[,weighted.trips:=trips*ifelse(day.type=="TU/WE/TH",3,2)]

  # Day of the week for the days in the simulated year 
  dates <- date.info(days,year)
  all.dem <- list()
  all.dem1 <- list()
  for(i in 1:length(days)){
    the.dem <- copy(dem[day.type==dates$day.types[i] & use.transit == includeTransitDemand & season == dates$seasons[i]])
    the.dem[,t:=pp('t',sprintf('%04d',1 + t + 24*(i-1)))]
    all.dem[[length(all.dem)+1]] <- the.dem[,.(r,t,td,trips)]
  }
  all.dem <- rbindlist(all.dem)
  all.dem[,':='(value=trips,trips=NULL,rmob=r,r=NULL)]
  all.dem[t==tail(common.inputs$sets$t,1),value:=0]
  all.dem.unscaled <- all.dem[,list(t,td=pp('td',td),rmob,value=value)]
  all.dem <- all.dem[,list(t,td=pp('td',td),rmob,value=value*fractionSAEVs*electrificationPenetration*vmtReboundFactor)]
  inputs$parameters$truckdemand <- all.dem
  inputs$parameters$truckdemandUnscaled <- all.dem.unscaled
  newdemand <- data.table(read.csv(pp(gem.raw.inputs,'trucks/truckdemand.csv')))
  names(newdemand)[1] <- 't'
  newdemand$t <- as.numeric(newdemand$t)
  for(i in 1:length(days)){
    demtmp <- copy(newdemand)
    demtmp[,t:=pp('t',sprintf('%04d', t + 24*(i-1)))]
    all.dem1[[length(all.dem1)+1]] <- demtmp[,.(t,td,rmob,value)]
  }
  all.dem1 <- rbindlist(all.dem1)
  inputs$parameters$truckdemandnew <- all.dem1

  ##### DISTANCE BINS #####
  inputs$sets$td <- pp('td',sort(u(dem$td)))
  inputs$parameters$trucktravelDistance <- dem[,.(td=pp('td',td),value=weighted.mean(dist,weighted.trips)),by=c('td','r')]
  inputs$parameters$trucktravelDistance[,':='(td=NULL,rmob=r,r=NULL)]
  inputs$parameters$trucktravelDistance <- inputs$parameters$trucktravelDistance[,list(td,rmob,value)]
  inputs$parameters$trucktravelDistancenew <- read.csv(pp(gem.raw.inputs,'trucks/parameter_selection_traveldistance.csv'))
  

  #### SPEED ####
  if('congestion'%in%param.names){
    congestion <- exper.row$congestion
  }
  tspeed.by.dist <- data.table(td=c("td0-2","td2-5","td5-10","td10-20","td20-30","td30-50","td50-100","td100-300"),value=c(18,22,32,38,40,45,48,48))
  tspeeds <- data.table(expand.grid(list(common.inputs$sets$rmob,common.inputs$sets$t,inputs$sets$td)))
  names(tspeeds) <- c('rmob','t','td')
  tspeeds <- join.on(tspeeds,tspeed.by.dist,'td','td')
  setkey(tspeeds,rmob,td,t)
    tspeed.scale.base <- c(
      1,1,1,1,1,0.99,
      0.9,0.8,0.7,0.8,0.85,0.85,
      0.85,0.85,0.8,0.7,0.6,0.55,
      0.7,0.75,0.9,0.95,0.99,1)
  if(congestion=='Freeflow'){
    tspeed.scale <- 1
  }else if(congestion=='Light'){
    tspeed.scale <- 1-(1-tspeed.scale.base)*.5
  }else if(congestion=='Medium'){
    tspeed.scale <- tspeed.scale.base
  }else if(congestion=='Heavy'){
    tspeed.scale <- 1-(1-tspeed.scale.base)*1.5
  }
  tspeeds[,value:=value*tspeed.scale]
  inputs$parameters$tspeed <- tspeeds[,.(t=t,td,rmob,value)]

  ##### CHARGING INFRASTRUCTURE #####
  truckcharger.levels.str <- pp('tL',str_pad(truckcharger.levels,4,'left','0'))
  inputs$sets$tl <- truckcharger.levels.str
  #### CHARGER CAPITAL ####
  if('l10ChargerCost'%in%param.names){
    l10ChargerCost <- exper.row$l10ChargerCost
  }
  if('chargerCostSuperlinear'%in%param.names){
    chargerCostSuperlinear <- exper.row$chargerCostSuperlinear
  }
  truckchargerCapitalCost <- l10ChargerCost + c(truckcharger.levels-min(truckcharger.levels))*chargerCostSuperlinear
  inputs$parameters$truckchargerCapitalCost <- data.table(tl=truckcharger.levels.str,value=truckchargerCapitalCost)
  #### CHARGER POWER ####
  inputs$parameters$truckchargerPower <- data.table(tl=truckcharger.levels.str,value=truckcharger.levels)
  #### CHARGER DISTRBITUION FACTOR ####
  inputs$parameters$truckchargerDistributionFactor <- data.table(tl=truckcharger.levels.str,value=1)

  #### DEMAND CHARGES ####
  if('demandCharge'%in%param.names){
    inputs$parameters$truckdemandCharge <- data.table(rmob=common.inputs$sets$rmob,value=exper.row$demandCharge)
  }else{
    inputs$parameters$truckdemandCharge <- data.table(rmob=common.inputs$sets$rmob,value=7.7)
  }
  
  ##### Scaling Factors from RISE #####
  rise <- data.table(read.csv(pp(gem.raw.inputs,'trucks/rise-scaling-factors.csv')))
  closest.mode.share <- u(rise$mode_share)[which.min(abs(u(rise$mode_share)-fractionSAEVs*electrificationPenetration))]
  rise[,chargeRelocationRatio:=d_chgempty+1]
  inputs$parameters$truckchargeRelocationRatio <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=chargeRelocationRatio)]
  inputs$parameters$truckfleetRatio <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=ntx_rat)]
  inputs$parameters$truckbatteryRatio <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=bat_rat)]
  inputs$parameters$truckdistCorrection <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=1+d_trpempty)]
  inputs$parameters$trucktimeCorrection <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=1+t_trpempty)]
  ### OTHER ADJUSTMENT FACTORS ###
  cached.raw.factors <- pp(gem.raw.inputs,'trucks/rise-scaling-factors/cached-factors.Rdata')
  the.files <- grep(".csv",list.files(pp(gem.raw.inputs,'trucks/rise-scaling-factors')),value=T)
  the.params <- lapply(str_split(the.files,".csv"),function(ll){ ll[1] })
#  if(file.exists(cached.raw.factors)){
#    load(cached.raw.factors)
#  }else{
    for(i in 1:length(the.files)){
      the.file <- the.files[i]
      the.param <- the.params[i]
      streval(pp(the.param," <- data.table(read.csv('",pp(gem.raw.inputs,'trucks/rise-scaling-factors/',the.file),"',stringsAsFactors=F))"))
    }
#    streval(pp('save(',pp(the.params,collapse=","),',file="',cached.raw.factors,'")'))
#  }
  for(the.param in the.params){
    streval(pp('inputs$parameters$',the.param,' <- ',the.param))
  }
  if('vehicleLifetime'%in%param.names){
    inputs$parameters$truckvehicleLifetime[,value:=exper.row$truckvehicleLifetime]
  }
  if('batteryLifetime'%in%param.names){
    inputs$parameters$truckbatteryLifetime[,value:=exper.row$truckbatteryLifetime]
  }
  
  inputs
}

