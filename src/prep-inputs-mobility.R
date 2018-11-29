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

  my.cat(pp('Creating mobility inputs'))

  inputs <- list()

  generic.params <- c('battery.cost')

  ##### Battery Cost ####
  if('battery.cost' %in% param.names){
    inputs$battery.cost <- exper.row$battery.cost
  }else{
    inputs$battery.cost <- battery.cost
  }
  
  inputs
}

  ##### CHARGING INFRASTRUCTURE #####
  charger.levels.str <- pp('L',str_pad(charger.levels,3,'left','0'))
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
  if('l10.factor'%in%param.names){
    l10.factor <- exp.pars$l10.factor[exp.i]
  }else{
    l10.factor <- 1.1
  }
  # From Gordon, at 22kW ~5% overbuild -> ~2% charger dead heading
  # 97% for 50kW ~3% overbuild ->1-2% dead head
  # Proposal, go from ~0-8% overbuild L250->L10 instead of 0-50%
  #charger.distrib.factors <- l10.factor ^ (1 / (charger.levels/min(charger.levels)))
  # EDIT: By 2018-07-23 Gordon's conclusion, set to 1.0, or 0% overbuild
  charger.distrib.factors <- 1.0
  if('price.shape'%in%param.names){
    if(exp.pars[exp.i]$price.shape=='Flat'){
      price.shape <- c(90,90,90)
    }else if(exp.pars[exp.i]$price.shape == 'Base'){
      price.shape <- base.price$price
    }else if(exp.pars[exp.i]$price.shape == 'CAISO-Duck'){
      price.shape <- caiso.duck$price
    }else if(exp.pars[exp.i]$price.shape == 'NYISO-Winter'){
      price.shape <- nyiso.winter$price
    }else if(exp.pars[exp.i]$price.shape == 'ERCOT-Summer'){
      price.shape <- ercot.summer$price
    }
  }else{
    price.shape <- base.price$price
  }
  if('price.energy'%in%param.names){
    price.energy <- exp.pars$price.energy[exp.i]
  }else{
    price.energy <- 1
  }
  if('day.type' %in% param.names){
    day.type <- exp.pars$day.type[exp.i]
  }else{
    day.type <- 'WEEKDAY'
  }
  if('demand.charge' %in% param.names){
    demand.charge <- exp.pars$demand.charge[exp.i]
  }else{
    # based on https://openei.org/apps/USURDB/
    # avg 7.7 $/kw/month
    # IQR 3.020000  10.700000
    # 10/90p 1.150000 16.230000
    demand.charge <- 7.7
  }
  if('discount.rate' %in% param.names){
    discount.rate <- exp.pars$discount.rate[exp.i]
  }else{
    discount.rate <- 0.050
  }

  velocity.shape <- c(1,0.9,0.8,0.6,1) # not currently used
  velocity.by.dist <- data.table(d=c(-1,5,seq(10,100,by=10),9999),v=c(18,18,32,38,40,42,44,45,45,45,45,48,48))
  if('sharing'%in%param.names){
    sharing <- exp.pars$sharing[exp.i]
  }else{
    sharing <- 1.5
  }

  # Create some scenarios, generate inputs, run and collect results


  load('input/mobility/nhts/dist_hour_hists.Rdata')
  dem <- data.table(read.csv('../NHTSAnalysis/binned_dists/dist_hour_hists.csv'))
  dem[,d:=AVGDIST]
  dem[,r:=pp(CDIVLS,'-',URBRURS)]
  dem[,':='(MILEBIN=NULL,CDIVLS=NULL,URBRURS=NULL,AVGDIST=NULL,NRAW=NULL,NWTD=NULL)]
  dem <- dem[WKTIME==day.type]
  dem[,':='(WKTIME=NULL)]
  dem <- melt(dem,id.vars=c('d','r'),variable.name='t',value.name='trips')
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

  uff <- data.table(read.csv('../StreetLightOutputs/extrapolation/urban-rural predictions.csv'))
  uff <- melt(uff,id.vars='div')[variable%in%c('urban_deadhead','rural_deadhead')]
  uff[,':='(urban.type=ifelse(grepl("rural",variable),"RUR","URB"),variable=NULL)]
  uff[,urban.form.factor:=value+1]
  div.to.lab <- c('New York'='NY','Mountain'='MTN','East North Central'='ENC','East South Central'='ESC','Pacific'='PAC','Middle Atlantic'='MAT','New England'='NENG','Texas'='TX','Florida'='FL','West North Central'='WNC','South Atlantic'='SAT','West South Central'='WSC','California'='CA')
  uff[,div:=div.to.lab[as.character(uff$div)]]

  urban.form.factor <- data.table(regions)
  urban.form.factor[,div:=unlist(lapply(str_split(regions,'-'),function(ll){ 
                                          second.item <- head(tail(ll,2),1)
                                          ifelse(second.item=='NL',ll[1],second.item)}))]
  urban.form.factor[,urban.type:=unlist(lapply(str_split(regions,'-'),function(ll){ tail(ll,1) }))]
  urban.form.factor <- join.on(urban.form.factor,uff,c('div','urban.type'),c('div','urban.type'),'urban.form.factor')
  urban.form.factor[,':='(r=regions,regions=NULL,div=NULL,urban.type=NULL)]
  if('urban.form'%in%param.names){
    urban.form.factor[,urban.form.factor:=exp.pars$urban.form[exp.i]]
  }

  price.per <- data.table(mod.t=0:23,price.shape)
  prices <- data.table(expand.grid(list(regions,times)))
  names(prices) <- c('r','t')
  prices[,mod.t:=t%%24]
  prices <- join.on(prices,price.per,'mod.t','mod.t','price.shape')
  prices[,mod.t:=NULL]
  prices[,price:=(price.shape/1000 + .09 - mean(price.shape/1000))*price.energy,by='r'] # translates so when price.energy==1, then average is 9 cents, the avg, commercial retail rate
  setkey(prices,r,t)

  velocity.per <- velocity.shape[round(((1:length(times))*(length(velocity.shape)-1))/length(times))+1]
  velocities <- data.table(expand.grid(list(regions,times,dist.bins)))
  names(velocities) <- c('r','t','d')
  setkey(velocities,r,d)
  velocities[,velocity:=velocity.by.dist$v[findInterval(d, velocity.by.dist$d)]]
  velocities[,d:=pp('d',roundC(d,1))]

  write.table(data.table(r=regions),file=pp(scenario.dir,'par_r.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(d=dist.bins.str),file=pp(scenario.dir,'par_d.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(t=times),file=pp(scenario.dir,'par_t.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(l=charger.levels.str),file=pp(scenario.dir,'par_l.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(d=dist.bins.str,s=sharing),file=pp(scenario.dir,'sharing.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(d=dist.bins.str,s=dist.bins),file=pp(scenario.dir,'travelDistance.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(r=regions,demCh=demand.charge),file=pp(scenario.dir,'demandCharge.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(batt=battery.cost),file=pp(scenario.dir,'batteryCost.csv'),sep=',',col.names=F,row.names=T,quote=F) # for scalars, write row.names
  write.table(data.table(rate=discount.rate),file=pp(scenario.dir,'discountRate.csv'),sep=',',col.names=F,row.names=T,quote=F) # for scalars, write row.names
  write.table(urban.form.factor[,.(r,urban.form.factor)],file=pp(scenario.dir,'urbanForm.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(prices[,.(t=t,r,price)],file=pp(scenario.dir,'price.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(dem[,.(t=t,d,r,trips)],file=pp(scenario.dir,'tripDemand.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(velocities[,.(t=t,d,r,velocity)],file=pp(scenario.dir,'velocity.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(l=charger.levels.str,cap.costs=charger.cap.costs),file=pp(scenario.dir,'chargerCapitalCost.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(l=charger.levels.str,cap.costs=charger.levels),file=pp(scenario.dir,'chargerPower.csv'),sep=',',col.names=F,row.names=F,quote=F)
  write.table(data.table(l=charger.levels.str,the.factors=charger.distrib.factors),file=pp(scenario.dir,'chargerDistributionFactor.csv'),sep=',',col.names=F,row.names=F,quote=F)
}
write.csv(exp.pars,file=pp(scenario.dir,'/../exp-params.csv'),row.names=F)

