###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs required by the mobility model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
###############################################################################################

prep.inputs.mobility.bike <- function(exper.row,param.names,common.inputs){
  param.names <- names(exper.row)
  

  inputs <- list()
  inputs$sets <- list()
  inputs$parameters <- list()

  ##### GENERIC PROCESSING OF SIMPLE PARAMS #####

  generic.params <- c('bikebatteryCapitalCost','bikechargerLifetime','bikevehicleCapitalCost','bikesharingFactor','bikechargercost')

  for(generic.param in generic.params){
    if(generic.param %in% param.names){
      streval(pp('inputs$parameters$',generic.param,' <- data.table(value=exper.row$',generic.param,')'))
    }else{
      streval(pp('inputs$parameters$',generic.param,' <- data.table(value=',generic.param,')'))
    }
  }

  if('bikeelectrificationPenetration' %in% param.names){
    bikeelectrificationPenetration <- exper.row$bikeelectrificationPenetration
  }
  if('bikefractionSAEVs' %in% param.names){
    bikefractionSAEVs <- exper.row$bikefractionSAEVs
  }
  if('bikevmtReboundFactor' %in% param.names){
    bikevmtReboundFactor <- exper.row$bikevmtReboundFactor
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
  conversion.efficiency.by.range <- c(.31)
  if('bikeConversionEfficiency' %in% param.names){
    inputs$parameters$bikeconversionEfficiency <- data.table(bb=c('bb40'),value=conversion.efficiency.by.range*exper.row$bikeConversionEfficiency/.324)
  }else{
    inputs$parameters$bikeconversionEfficiency <- data.table(bb=c('bb40'),value=conversion.efficiency.by.range)
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
    for(file in grep('Rdata',grep('dist_hour_hists',list.files(pp(gem.raw.inputs,'bikes/nhts/')),value=T),invert=T,value=T)){
      df <- data.table(read.csv(pp(gem.raw.inputs,'bikes/nhts/',file),stringsAsFactors=F))
      df[,season:=str_split(file,"_")[[1]][4]]
      df[,transit:=str_split(file,"_")[[1]][5]]
      dfs[[length(dfs)+1]] <- df
    }
    dem <- rbindlist(dfs)
    save(dem,file=pp(gem.raw.inputs,'bikes/nhts/','dist_hour_hists.Rdata'))
  }
  load(pp(gem.raw.inputs,'bikes/nhts/dist_hour_hists.Rdata'))
  dem[,dist:=AVGDIST]
  dem[,bd:=MILEBIN]
  dem[,r:=pp(CDIVLS,'-',URBRURS)]
  dem[,':='(MILEBIN=NULL,CDIVLS=NULL,URBRURS=NULL,AVGDIST=NULL,NRAW=NULL,NWTD=NULL)]
  dem[,':='(day.type=WKTIME,WKTIME=NULL)]
  dem <- data.table(melt(dem,id.vars=c('bd','dist','season','transit','r','day.type'),variable.name='t',value.name='trips'))
  names(dem) <- c('bd','dist','season','transit','r','day.type','t','trips')
  dem[,t:=as.numeric(unlist(lapply(str_split(t,"X"),function(x){x[2]})))]
  dem[,use.transit:=transit=='with']
  #ggplot(dem[,.(trips=sum(trips)),by=c('t','d','season','transit')],aes(x=t,y=trips,colour=d))+geom_line()+facet_grid(season~transit)
  
  
  #for a truly weighted average distance by bin, weight the day.type
  dem[,weighted.trips:=trips*ifelse(day.type=="TU/WE/TH",3,2)]
  
  if('biketocarfactor'%in%param.names){
    biketocarfactor <- exper.row$biketocarfactor
  }
  
  #map car to bike
  car02=48928
  k02=c(380,622,415,322)
  dist02=c(0.25,0.75,1.25,1.75)
  n02=c('0-0.5','0.5-1','1-1.5','1.5-2')
  car25=43673
  k25=c(179,140,141,92)
  dist25=c(2.25/3.5,2.75/3.5,1,4.5/3.5)
  n25=c('2-2.5','2.5-3','3-4','4-5')
  car510=26420
  k510=168
  car10=34965
  k10=131
  all02 <- list()
  for (i in 1:4){
    dem02 <- copy(dem[bd=='0-2'])
    dem02[,bd:=n02[i]]
    dem02$dist=dem02$dist*dist02[i]
    dem02$trips = dem02$trips*k02[i]/car02*biketocarfactor
    dem02$weighted.trips=dem02$weighted.trips*k02[i]/car02*biketocarfactor
    all02[[length(all02)+1]]<-dem02
  }
  all02<-rbindlist(all02) 
  all25<-list()
  for (i in 1:4){
    dem25 <- copy(dem[bd=='2-5'])
    dem25[,bd:=n25[i]]    
    dem25$trips = dem25$trips*k25[i]/car25*biketocarfactor
    dem25$dist=dem25$dist*dist25[i]
    dem25$weighted.trips=dem25$weighted.trips*k25[i]/car25*biketocarfactor
    all25[[length(all25)+1]]<-dem25
  }
  all25<-rbindlist(all25)  
  all510 <- copy(dem[bd=='5-10'])
  all510$trips = all510$trips*510/car510*biketocarfactor
  all510$weighted.trips=all510$weighted.trips*k510/car510*biketocarfactor
  all10 <-copy(dem[bd=='10-20'])
  all10[,bd:='10+']
  all10$trips = all10$trips*10/car10*biketocarfactor
  all10$weighted.trips=all10$weighted.trips*k10/car10*biketocarfactor
  dem1<-list(all02,all25,all510,all10)
  dem1<-rbindlist(dem1)
  
  dates <- date.info(days,year)
  all.dem<-list()
  for(i in 1:length(days)){
    the.dem <- copy(dem1[day.type==dates$day.types[i] & use.transit == includeTransitDemand & season == dates$seasons[i]])
    the.dem[,t:=pp('t',sprintf('%04d',1 + t + 24*(i-1)))]
    all.dem[[length(all.dem)+1]] <- the.dem[,.(r,t,bd,trips)]
  }
  all.dem <- rbindlist(all.dem)
  all.dem[,':='(value=trips,trips=NULL,rmob=r,r=NULL)]
  all.dem[t==tail(common.inputs$sets$t,1),value:=0]
  all.dem.unscaled <- all.dem[,list(t,bd=pp('bd',bd),rmob,value=value)]
  all.dem <- all.dem[,list(t,bd=pp('bd',bd),rmob,value=value*fractionSAEVs*electrificationPenetration*vmtReboundFactor)]
  inputs$parameters$bikedemand <- all.dem
  inputs$parameters$bikedemandUnscaled <- all.dem.unscaled

  ##### DISTANCE BINS #####
  inputs$sets$bd <- pp('bd',sort(u(dem1$bd)))
  inputs$parameters$biketravelDistance <- dem[,.(bd=pp('bd',bd),value=weighted.mean(dist,weighted.trips)),by=c('bd','r')]
  inputs$parameters$biketravelDistance[,':='(bd=NULL,rmob=r,r=NULL)]
  inputs$parameters$biketravelDistance <- inputs$parameters$biketravelDistance[,list(bd,rmob,value)]
  #inputs$parameters$biketravelDistance <- read.csv(pp(gem.raw.inputs,'bikes/parameter_selection_traveldistance.csv'))
  

  #### SPEED ####
  if('congestion'%in%param.names){
    congestion <- exper.row$congestion
  }
  bspeed.by.dist <- data.table(bd=c("bd0-0.5","bd0.5-1","bd1-1.5","bd1.5-2","bd2-2.5","bd2.5-3","bd3-4","bd4-5","bd5-10","bd10+"),value=c(6.5,7,7.5,7.8,7.8,7.8,7.8,7.8,7.8,7.8))
  bspeeds <- data.table(expand.grid(list(common.inputs$sets$rmob,common.inputs$sets$t,inputs$sets$bd)))
  names(bspeeds) <- c('rmob','t','bd')
  bspeeds <- join.on(bspeeds,bspeed.by.dist,'bd','bd')
  setkey(bspeeds,rmob,bd,t)
    bspeed.scale.base <- c(
      1,1,1,1,1,0.99,
      0.9,0.8,0.7,0.8,0.85,0.85,
      0.85,0.85,0.8,0.7,0.6,0.55,
      0.7,0.75,0.9,0.95,0.99,1)
  if(congestion=='Freeflow'){
    bspeed.scale <- 1
  }else if(congestion=='Light'){
    bspeed.scale <- 1-(1-bspeed.scale.base)*.5
  }else if(congestion=='Medium'){
    bspeed.scale <- bspeed.scale.base
  }else if(congestion=='Heavy'){
    bspeed.scale <- 1-(1-bspeed.scale.base)*1.5
  }
  bspeeds[,value:=value*bspeed.scale]
  inputs$parameters$bspeed <- bspeeds[,.(t=t,bd,rmob,value)]

  #### bike battery cost ####
  bikebattery.levels.str <- pp('bb',str_pad(bikebattery.levels,2,'left','0'))
  inputs$sets$bb <- bikebattery.levels.str
  if('bikebatteryCapitalCost'%in%param.names){
    bikebatteryCapitalCost <- exper.row$bikebatteryCapitalCost
  }
  inputs$parameters$bikebatteryCapitalCost <- data.table(bb=bikebattery.levels.str,value=bikebatteryCapitalCost)
  
  
  ##### CHARGING INFRASTRUCTURE #####
  bikecharger.levels.str <- pp('bL',str_pad(bikecharger.levels,1,'left','0'))
  inputs$sets$bl <- bikecharger.levels.str
  #### CHARGER CAPITAL ####
  if('bikechargerCapitalCost'%in%param.names){
    bikechargerCapitalCost <- exper.row$bikechargerCapitalCost
  }
  inputs$parameters$bikechargerCapitalCost <- data.table(bl=bikecharger.levels.str,value=bikechargerCapitalCost)
  #### CHARGER POWER ####
  inputs$parameters$bikechargerPower <- data.table(bl=bikecharger.levels.str,value=bikecharger.levels)
  #### CHARGER DISTRBITUION FACTOR ####
  inputs$parameters$bikechargerDistributionFactor <- data.table(bl=bikecharger.levels.str,value=1)

  #### DEMAND CHARGES ####
  if('demandCharge'%in%param.names){
    inputs$parameters$bikedemandCharge <- data.table(rmob=common.inputs$sets$rmob,value=exper.row$bikedemandCharge)
  }else{
    inputs$parameters$bikedemandCharge <- data.table(rmob=common.inputs$sets$rmob,value=7.7)
  }
  
  ##### Scaling Factors from RISE #####
  rise <- data.table(read.csv(pp(gem.raw.inputs,'bikes/rise-scaling-factors.csv')))
  closest.mode.share <- u(rise$mode_share)[which.min(abs(u(rise$mode_share)-bikefractionSAEVs*bikeelectrificationPenetration))]
  rise[,chargeRelocationRatio:=d_chgempty+1]
  inputs$parameters$bikechargeRelocationRatio <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=chargeRelocationRatio)]
  inputs$parameters$bikefleetRatio <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=ntx_rat)]
  inputs$parameters$bikebatteryRatio <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=bat_rat)]
  inputs$parameters$bikedistCorrection <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=1+d_trpempty)]
  inputs$parameters$biketimeCorrection <- rise[mode_share==closest.mode.share,.(rmob=abbrev,value=1+t_trpempty)]
  ### OTHER ADJUSTMENT FACTORS ###
  cached.raw.factors <- pp(gem.raw.inputs,'bikes/rise-scaling-factors/cached-factors.Rdata')
  the.files <- grep(".csv",list.files(pp(gem.raw.inputs,'bikes/rise-scaling-factors')),value=T)
  the.params <- lapply(str_split(the.files,".csv"),function(ll){ ll[1] })
#  if(file.exists(cached.raw.factors)){
#    load(cached.raw.factors)
#  }else{
    for(i in 1:length(the.files)){
      the.file <- the.files[i]
      the.param <- the.params[i]
      streval(pp(the.param," <- data.table(read.csv('",pp(gem.raw.inputs,'bikes/rise-scaling-factors/',the.file),"',stringsAsFactors=F))"))
    }
#    streval(pp('save(',pp(the.params,collapse=","),',file="',cached.raw.factors,'")'))
#  }
  for(the.param in the.params){
    streval(pp('inputs$parameters$',the.param,' <- ',the.param))
  }
  if('vehicleLifetime'%in%param.names){
    inputs$parameters$bikevehicleLifetime[,value:=exper.row$bikevehicleLifetime]
  }
  if('batteryLifetime'%in%param.names){
    inputs$parameters$bikebatteryLifetime[,value:=exper.row$bikebatteryLifetime]
  }
  
  inputs
}

