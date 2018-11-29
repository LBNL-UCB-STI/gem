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

  ##### GENERIC PROCESSING OF SIMPLE PARAMS #####
  generic.params <- c('battery.cost','dist.bin.labels','discount.rate')

  for(generic.param in generic.params){
    if(generic.param %in% param.names){
      streval(pp('inputs$',generic.param,' <- exper.row$',generic.param))
    }else{
      streval(pp('inputs$',generic.param,' <- ',generic.param))
    }
  }


  ##### SHARING #####
  if('sharing.factor'%in%param.names){
    inputs$sharing.factor <- data.table(d=common.inputs$d,sharingFactor=exper.row$sharing)
  }else{
    inputs$sharing.factor <- sharing.factor
  }

  ##### DISTANCE BINS #####
  inputs$travel.distance <- dist.bins

  ##### URBAN FORM - STRAIGHT SCALING ALL REGIONS #####
  if('scale.urban.form.factor' %in% param.names){
    inputs$urban.form.factor <- data.table(r=common.inputs$r,urban.form.factor=exper.row$scale.urban.form.factor * 1.3)
  }else{
    inputs$urban.form.factor <- data.table(r=common.inputs$r,urban.form.factor=1.3)
  }
  #uff <- data.table(read.csv('../StreetLightOutputs/extrapolation/urban-rural predictions.csv'))
  #uff <- melt(uff,id.vars='div')[variable%in%c('urban_deadhead','rural_deadhead')]
  #uff[,':='(urban.type=ifelse(grepl("rural",variable),"RUR","URB"),variable=NULL)]
  #uff[,urban.form.factor:=value+1]
  #div.to.lab <- c('New York'='NY','Mountain'='MTN','East North Central'='ENC','East South Central'='ESC','Pacific'='PAC','Middle Atlantic'='MAT','New England'='NENG','Texas'='TX','Florida'='FL','West North Central'='WNC','South Atlantic'='SAT','West South Central'='WSC','California'='CA')
  #uff[,div:=div.to.lab[as.character(uff$div)]]

  #urban.form.factor <- data.table(regions)
  #urban.form.factor[,div:=unlist(lapply(str_split(regions,'-'),function(ll){ 
                                          #second.item <- head(tail(ll,2),1)
                                          #ifelse(second.item=='NL',ll[1],second.item)}))]
  #urban.form.factor[,urban.type:=unlist(lapply(str_split(regions,'-'),function(ll){ tail(ll,1) }))]
  #urban.form.factor <- join.on(urban.form.factor,uff,c('div','urban.type'),c('div','urban.type'),'urban.form.factor')
  #urban.form.factor[,':='(r=regions,regions=NULL,div=NULL,urban.type=NULL)]
  #if('urban.form'%in%param.names){
    #urban.form.factor[,urban.form.factor:=exp.pars$urban.form[exp.i]]
  #}

  #### DEMAND ####
  if(F){
    if('day.type' %in% param.names){
      day.type <- exp.pars$day.type[exp.i]
    }else{
      day.type <- 'WEEKDAY'
    }

    # Create some scenarios, generate inputs, run and collect results
    load('input/mobility/nhts/dist_hour_hists.Rdata')
    dem <- df
    dem[,d:=AVGDIST]
    dem[,r:=pp(CDIVLS,'-',URBRURS)]
    dem[,':='(MILEBIN=NULL,CDIVLS=NULL,URBRURS=NULL,AVGDIST=NULL,NRAW=NULL,NWTD=NULL)]
    dem[,':='(day.type=WKTIME,WKTIME=NULL)]
    dem <- melt(dem,id.vars=c('d','r','day.type'),variable.name='t',value.name='trips')
    dem[,t:=as.numeric(unlist(lapply(str_split(t,"X"),function(x){x[2]})))]
    #ggplot(dem,aes(x=t,y=trips,colour=factor(d)))+geom_line()+facet_wrap(~r)
    #ggplot(dem,aes(x=t,y=trips,colour=factor(r)))+geom_line()+facet_wrap(~d)
    #ggplot(dem,aes(x=t,y=trips.scaled,colour=factor(r)))+geom_line()+facet_wrap(~d,scales='free_y')

    dist.bins <- quantile(u(dem$d),seq(0,1,length.out=num.dist.bins+1))
    dist.bin.num <- as.numeric(factor(dist.bins))
    dem[,new.bins:=dist.bin.num[findInterval(d,dist.bins)]]
    dem[new.bins==tail(dist.bin.num,1),new.bins:=head(tail(dist.bin.num,2),1)]
    dem[,new.d:=weighted.mean(d,trips),by='new.bins']
    dem[,':='(new.bins=new.d,new.d=NULL)]
    dem <- dem[,.(trips=sum(trips)),by=c('r','t','new.bins')]
    dem[,trips:=trips / (365 * ifelse(day.type=='WEEKDAY',5,2) / 7)] # the data come in total annual trips for weekdays or weekends, so we need to scale them back
    dem[,':='(d=new.bins,new.bins=NULL)]
    dem[,trips.scaled:=trips/sum(trips),by=c('d','r')]
    dist.bins <- sort(u(dem$d))
    dist.bins.str <- pp('d',roundC(dist.bins,1))
    dem[,d.str:=pp('d',roundC(d,1))]
    dem[,':='(d=NULL)]
    dem[,':='(d=d.str,d.str=NULL)]
    dem[t<4,t:=t+24]

    num.time.steps <- length(u(dem$t))+1
    delta.t <- diff(sort(u(dem$t)))[1]
    times <- c(sort(u(dem$t)),max(dem$t)+delta.t)

    setkey(dem,r,d)
    zero.dem <- u(dem)
    zero.dem[,':='(trips=0,trips.scaled=0,t=max(times))]
    dem <- rbindlist(list(dem,zero.dem))
    #dem[,r:=str_replace(r,"-","")]
    if(regions[1]=='ALL'){
      regions <- u(dem$r)
    }else{
      dem <- dem[r%in%regions]
    }
    inputs$demand <- dem[,.(t=t,d,r,trips)] 
  }else{
    inputs$demand <- 0
  }

  #### SPEED ####
  velocity.shape <- c(1,0.9,0.8,0.6,1) # not currently used
  velocity.by.dist <- data.table(d=c(-1,5,seq(10,100,by=10),9999),v=c(18,18,32,38,40,42,44,45,45,45,45,48,48))
  velocity.per <- velocity.shape[round(((1:length(common.inputs$t))*(length(velocity.shape)-1))/length(common.inputs$t))+1]
  velocities <- data.table(expand.grid(list(regions,common.inputs$t,dist.bins)))
  names(velocities) <- c('r','t','d')
  setkey(velocities,r,d)
  velocities[,velocity:=velocity.by.dist$v[findInterval(d, velocity.by.dist$d)]]
  velocities[,d:=pp('d',roundC(d,1))]
  inputs$velocity <- velocities[,.(t=t,d,r,velocity)]

  ##### CHARGING INFRASTRUCTURE #####
  charger.levels.str <- pp('L',str_pad(charger.levels,3,'left','0'))
  inputs$l <- charger.levels.str
  #### CHARGER CAPITAL ####
  if('l10.charger.cost'%in%param.names){
    l10.charger.cost <- exp.pars$l10.charger.cost[exp.i]
  }else{
    l10.charger.cost <- 500
  }
  if('charger.cap.superlinear'%in%param.names){
    charger.cap.superlinear <- exp.pars$charger.cap.superlinear[exp.i]
  }else{
    charger.cap.superlinear <- 5
  }
  charger.cap.costs <- l10.charger.cost + c(charger.levels-min(charger.levels))*charger.cap.superlinear
  inputs$charger.cap.costs <- data.table(l=charger.levels.str,cap.costs=charger.cap.costs)
  #### CHARGER POWER ####
  inputs$charger.power <- data.table(l=charger.levels.str,charger.power=charger.levels)
  #### CHARGER DISTRBITUION FACTOR ####
  inputs$charger.distrib.factor <- data.table(l=charger.levels.str,charger.distrib.factor=1)
  
  inputs
}

