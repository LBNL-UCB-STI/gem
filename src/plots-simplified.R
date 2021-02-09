### Issues to be fixed: ###
# Per-passenger mile metrics don't include HDVs

getPalette = function(vals){
	ncol <- length(u(vals))
	if(any(u(vals)=='b075',na.rm=TRUE)|any(u(vals)=='75 kWh',na.rm=TRUE)){
		nprivate <- sum(substr(u(vals),1,6)=='Privat')
		nsaevs <- ncol - nprivate
		c(rev(colorRampPalette(brewer.pal(nsaevs, "Reds"))(nsaevs)),
		rev(colorRampPalette(brewer.pal(nprivate, "Blues"))(nprivate)))
	} else if(any(u(vals)=='L010',na.rm=TRUE)|any(u(vals)=='10 kW',na.rm=TRUE)){
		nsaevs <- sum(substr(u(vals),1,1)=='L')
		nprivate <- ncol-nsaevs
		c(rev(colorRampPalette(brewer.pal(nsaevs, "Purples"))(nsaevs)),
		rev(colorRampPalette(brewer.pal(nprivate, "Greens"))(nprivate)))
	} else if(any(vals%in%c("Solar","Wind","Other","Hydro","Natural Gas","Coal","Nuclear"),na.rm=T)){  
	  #cs <- rev(colorRampPalette(brewer.pal(10, "Set3"))(10)
	  #names(cs) <- c("Solar","Wind","Geothermal","Other","Hydro","NaturalGas","Pumps","Biomass","Coal","Nuclear")
	  #cs[vals]
		rev(colorRampPalette(brewer.pal(ncol, "Greys"))(ncol))
	} else if(any(substr(u(vals),1,4)=='Idle',na.rm=TRUE)|any(substr(u(vals),1,6)=='Moving',na.rm=TRUE)|any(substr(u(vals),1,8)=='Charging',na.rm=TRUE)){
		nidle <- sum(substr(u(vals),1,3)=='Idl')
		nmoving <- sum(substr(u(vals),1,6)=='Moving')
		ncharging <- sum(substr(u(vals),1,8)=='Charging')
		c(rev(colorRampPalette(brewer.pal(nidle, "Greys"))(nidle)),
		rev(colorRampPalette(brewer.pal(nmoving, "Blues"))(nmoving)),
		rev(colorRampPalette(brewer.pal(ncharging, "Reds"))(ncharging))
	  )
	} else if(any(substr(u(vals),1,4)=='Chgr',na.rm=TRUE)){
		ncharger <- sum(substr(u(vals),1,4)=='Chgr')
		nbat <- ncol-ncharger
		c(
		rev(colorRampPalette(brewer.pal(nbat, "Blues"))(nbat)),
		rev(colorRampPalette(brewer.pal(ncharger, "Reds"))(ncharger))
		)
	} else{
		rev(colorRampPalette(brewer.pal(ncol, "Reds"))(ncol))
	}
}

run.all <- function(exper,all.inputs,res,plots.dir) {
	inputs <- all.inputs[[1]]
	source('src/colors.R')

	# Day-to-Year ratio based on travel demand
	n.days.in.run <- length(inputs$set$t)/24

	# Prepping data inputs for plotting
	lightduty.prepped <- prepData.all.lightduty(exper,inputs,res,plots.dir)
	heavyduty.prepped <- prepData.all.heavyduty(exper,inputs,res,plots.dir)
	generators.prepped <- prepData.generators(res)
	maps.prepped <- prepData.maps()
	costs.prepped <- prepData.costs(res,generators.prepped$gen.cost,inputs)

	day.axis.breaks <- seq(0,max(lightduty.prepped$veh.ch$t),by=24)

	# Plotting all single run plots
	for(run.i in u(lightduty.prepped$vehs$run)) {
		plot.grid.all(run.i,plots.dir,res,generators.prepped$geners)
		plot.lightduty.all(run.i,plots.dir,lightduty.prepped$vehs,exper,all.inputs,lightduty.prepped$veh.ch,lightduty.prepped$personal.ev.ch,lightduty.prepped$en,lightduty.prepped$by.r,day.axis.breaks)
		plot.heavyduty.all(run.i,plots.dir,heavyduty.prepped$trcks,exper,all.inputs,heavyduty.prepped$trck.ch,heavyduty.prepped$en.trck,heavyduty.prepped$by.r.trck,day.axis.breaks)
	}

	all.factors.prepped <- prepData.factor.all(all.inputs,inputs,res,lightduty.prepped$by.r,heavyduty.prepped$by.r.trck,lightduty.prepped$veh.ch,heavyduty.prepped$trck.ch,lightduty.prepped$personal.ev.ch,costs.prepped,lightduty.prepped$vmt.by.region,heavyduty.prepped$vmt.by.region.trck,generators.prepped$geners,lightduty.prepped$vehs)

	make.1d.plots(plots.dir,all.factors.prepped$all,res,lightduty.prepped$by.r,all.factors.prepped$the.cols,all.factors.prepped$factor.labels,all.factors.prepped$metric.units,all.factors.prepped$to.plot.ch.agg,all.factors.prepped$the.ch.cols)
	make.2d.plots(all.factors.prepped$all,all.factors.prepped$to.plot.ch.agg,all.factors.prepped$the.cols,all.factors.prepped$the.ch.cols)
}

prepData.all.lightduty <- function(exper,inputs,res,plots.dir) {
	# Vehicle Distribution
	veh.mv <- data.table(cast(melt(res[['b-d-rmob-t']],id.vars=c('t','b','d','rmob','run'),measure.vars=c('vehiclesMoving')),b + rmob + t + run ~ d))
	d.dot <- str_replace(inputs$sets$d,"-",".")
	streval(pp('veh.mv[,":="(',pp(pp('vehiclesMoving.',d.dot,'=`',inputs$sets$d,'`,`',inputs$sets$d,'`=NULL'),collapse=','),')]'))
	l.dot <- str_replace(inputs$sets$l,"-",".")
	veh.ch <- data.table(cast(melt(res[['b-l-rmob-t']],id.vars=c('t','b','l','rmob','run'),measure.vars=c('vehiclesCharging')),b + rmob + t + run ~ l))
	streval(pp('veh.ch[,":="(',pp(pp('vehiclesCharging.',l.dot,'=`',inputs$sets$l,'`,`',inputs$sets$l,'`=NULL'),collapse=','),')]'))
	vehs <- join.on(res[['b-rmob-t']],join.on(veh.mv,veh.ch,c('b','rmob','t','run')),c('b','rmob','t','run'))
	veh.ch <- data.table(melt(res[['b-l-rmob-t']],id.vars=c('t','b','l','rmob','run'),measure.vars=c('vehiclesCharging')))
	veh.ch[,kw:=unlist(lapply(str_split(l,'L'),function(ll){ as.numeric(ll[2])}))]
	veh.ch[,gw.charging:=kw*value/1e6]
	setkey(veh.ch,run,l,rmob,t)
	veh.ch[,charger.level:=gsub('L|L0','',l)]
	veh.ch[,charger.level:=paste(charger.level,'kW')]
	veh.ch$charger.level <- factor(veh.ch$charger.level,levels=unique(veh.ch$charger.level)[mixedorder(unique(veh.ch$charger.level))])

	# Private Vehicle Charging
	personal.ev.ch <- res[['rmob-t']][,.(t,rmob,gw.charging=personalEVPower/1e6,run)]
	personal.ev.ch[,l:='Private EVs']
	personal.ev.ch[,charger.level:='Private EVs']

	# Energy balance
	en <- join.on(join.on(res[['b-l-rmob-t']],res[['b-l-rmob']],c('l','rmob','b','run'),c('l','rmob','b','run'))[,.(en.ch=sum(energyCharged)),by=c('t','rmob','b','run')],res[['b-d-rmob-t']][,.(en.mob=sum(energyConsumed)),by=c('t','rmob','b','run')],c('b','rmob','t','run'),c('b','rmob','t','run'))
	batt <- join.on(res[['b-rmob']],res[['b']],c('b','run'),c('b','run'))
	batt[,soc:=fleetSize*batteryCapacity]
	en <- join.on(en,batt,c('b','rmob','run'),c('b','rmob','run'),'soc')
	#en[t>0,soc:=0]
	setkey(en,b,rmob,t)
	en[,soc:=soc+cumsum(en.ch-en.mob),by=c('b','rmob','run')]
	en[,battery.level:=gsub('b|b0','',b)]
	en[,battery.level:=paste(battery.level,'mi')]
	en$battery.level <- factor(en$battery.level,levels=unique(en$battery.level)[mixedorder(unique(en$battery.level))])

	# Fleet size and num chargers
	by.r <- rbindlist(list(res[['l-rmob']][,':='(run=run,variable=l,value=numChargers,group='Chargers')],res[['b-rmob']][,':='(run=run,variable=b,value=fleetSize,group='Fleet')]),fill=T)
	setkey(by.r,run,variable)
	by.r[,var.clean:=ifelse(grepl('L',variable),paste('Chgr:',variable,'kW'),paste('Bat:',variable,'mi'))]
	by.r[,var.clean:=gsub(' L| L0| b| b0',' ',var.clean)]
	by.r$var.clean <- factor(by.r$var.clean,levels=unique(by.r$var.clean)[mixedorder(unique(by.r$var.clean))])

	setkey(vehs,run,b,rmob,t)

	#VMT
	vmt <- join.on(res[['b-d-rmob-t']],res[['d-rmob']],c('d','rmob','run'),c('d','rmob','run'),'travelDistance')
	vmt <- join.on(vmt,res[['d-rmob-t']],c('d','t','rmob','run'),c('d','t','rmob','run'),'speed')
	vmt[,vmt:=vehiclesMoving*travelDistance]
	vmt[,pmt:=demandAllocated*travelDistance]
	fleet <- res[['b-rmob']]
	vmt.by.region <- join.on(join.on(fleet,vmt[,.(vmt=sum(vmt),pmt=sum(pmt)),by=c('rmob','b','run')],c('rmob','b','run'),c('rmob','b','run')),inputs$parameters$vehicleLifetime[,.(value=mean(value)),by=c('b','rmob')],c('rmob','b'),c('rmob','b'),'value','assumed.lifetime.')
	n.days <- max(vmt$t)/24
	vmt.by.region[,daily.vmt.per.vehicle:=vmt/fleetSize/n.days]
	vmt.by.region[,daily.pmt:=pmt/n.days]
	vmt.by.region[,battery.level:=gsub('b|b0','',b)]
	vmt.by.region[,battery.level:=paste(battery.level,'mi')]
	vmt.by.region$battery.level <- factor(vmt.by.region$battery.level,levels=unique(vmt.by.region$battery.level)[mixedorder(unique(vmt.by.region$battery.level))])

	output <- list('veh.mv'=veh.mv,'d.dot'=d.dot,'l.dot'=l.dot,'veh.ch'=veh.ch,'vehs'=vehs,'veh.ch'=veh.ch,'personal.ev.ch'=personal.ev.ch,'en'=en,'batt'=batt,'by.r'=by.r,'vmt'=vmt,'vmt.by.region'=vmt.by.region)
	return(output)
}

prepData.all.heavyduty <- function(exper,inputs,res,plots.dir) {
	# Vehicle Distribution
	trck.mv <- data.table(cast(melt(res[['rmob-t-tb-td']],id.vars=c('t','tb','td','rmob','run'),measure.vars=c('truckvehiclesMoving')),tb + rmob + t + run ~ td))
	td.dot <- str_replace(inputs$sets$td,"-",".")
	streval(pp('trck.mv[,":="(',pp(pp('truckvehiclesMoving.',td.dot,'=`',inputs$sets$td,'`,`',inputs$sets$td,'`=NULL'),collapse=','),')]'))
	tl.dot <- str_replace(inputs$sets$tl,"-",".")
	trck.ch <- data.table(cast(melt(res[['rmob-t-tb-tl']],id.vars=c('t','tb','tl','rmob','run'),measure.vars=c('truckvehiclesCharging')),tb + rmob + t + run ~ tl))
	streval(pp('trck.ch[,":="(',pp(pp('truckvehiclesCharging.',tl.dot,'=`',inputs$sets$tl,'`,`',inputs$sets$tl,'`=NULL'),collapse=','),')]'))
	trcks <- join.on(res[['rmob-t-tb']],join.on(trck.mv,trck.ch,c('tb','rmob','t','run')),c('tb','rmob','t','run'))
	trck.ch <- data.table(melt(res[['rmob-t-tb-tl']],id.vars=c('t','tb','tl','rmob','run'),measure.vars=c('truckvehiclesCharging')))
	trck.ch[,kw:=unlist(lapply(str_split(tl,'tL'),function(ll){ as.numeric(ll[2])}))]
	trck.ch[,gw.charging:=kw*value/1e6]
	setkey(trck.ch,run,tl,rmob,t)
	trck.ch[,charger.level:=gsub('tL|tL0|tL00','',tl)]
	trck.ch[,charger.level:=paste(charger.level,'kW')]
	trck.ch$charger.level <- factor(trck.ch$charger.level,levels=unique(trck.ch$charger.level)[mixedorder(unique(trck.ch$charger.level))])

	# Energy balance
	en.trck <- join.on(join.on(res[['rmob-t-tb-tl']],res[['rmob-tb-tl']],c('tl','rmob','tb','run'),c('tl','rmob','tb','run'))[,.(en.ch=sum(truckenergyCharged)),by=c('t','rmob','tb','run')],res[['rmob-t-tb-td']][,.(en.mob=sum(truckenergyConsumed)),by=c('t','rmob','tb','run')],c('tb','rmob','t','run'),c('tb','rmob','t','run'))
	batt.trck <- join.on(res[['rmob-tb']],res[['tb']],c('tb','run'),c('tb','run'))
	batt.trck[,soc:=truckfleetSize*truckbatteryCapacity]
	en.trck <- join.on(en.trck,batt.trck,c('tb','rmob','run'),c('tb','rmob','run'),'soc')
	#en[t>0,soc:=0]
	setkey(en.trck,tb,rmob,t)
	en.trck[,soc:=soc+cumsum(en.ch-en.mob),by=c('tb','rmob','run')]
	en.trck[,battery.level:=gsub('tb|tb0|tb00','',tb)]
	en.trck[,battery.level:=paste(battery.level,'mi')]
	en.trck$battery.level <- factor(en.trck$battery.level,levels=unique(en.trck$battery.level)[mixedorder(unique(en.trck$battery.level))])

	# Fleet size and num chargers
	by.r.trck <- rbindlist(list(res[['rmob-tl']][,':='(run=run,variable=tl,value=trucknumChargers,group='Chargers')],res[['rmob-tb']][,':='(run=run,variable=tb,value=truckfleetSize,group='Fleet')]),fill=T)
	setkey(by.r.trck,run,variable)
	by.r.trck[,var.clean:=ifelse(grepl('tL',variable),paste('Chgr:',variable,'kW'),paste('Bat:',variable,'mi'))]
	by.r.trck[,var.clean:=gsub(' tL| tL0| tL00|tb00| tb| tb0',' ',var.clean)]
	by.r.trck$var.clean <- factor(by.r.trck$var.clean,levels=unique(by.r.trck$var.clean)[mixedorder(unique(by.r.trck$var.clean))])

	#VMT
	vmt.trck <- join.on(res[['rmob-t-tb-td']],res[['rmob-td']],c('td','rmob','run'),c('td','rmob','run'),'trucktravelDistance')
	vmt.trck <- join.on(vmt.trck,res[['rmob-t-td']],c('td','t','rmob','run'),c('td','t','rmob','run'),'tspeed')
	vmt.trck[,vmt.trck:=truckvehiclesMoving*trucktravelDistance]
	vmt.trck[,pmt:=truckdemandAllocated*trucktravelDistance]
	fleet.trck <- res[['rmob-tb']]
	vmt.by.region.trck <- join.on(join.on(fleet.trck,vmt.trck[,.(vmt.trck=sum(vmt.trck),pmt=sum(pmt)),by=c('rmob','tb','run')],c('rmob','tb','run'),c('rmob','tb','run')),inputs$parameters$truckvehicleLifetime[,.(value=mean(value)),by=c('tb','rmob')],c('rmob','tb'),c('rmob','tb'),'value','assumed.lifetime.')
	n.days <- max(vmt.trck$t)/24
	vmt.by.region.trck[,daily.vmt.per.vehicle:=vmt.trck/truckfleetSize/n.days]
	vmt.by.region.trck[,daily.pmt:=pmt/n.days]
	vmt.by.region.trck[,battery.level:=gsub('tb|tb0','',tb)]
	vmt.by.region.trck[,battery.level:=paste(battery.level,'mi')]
	vmt.by.region.trck$battery.level <- factor(vmt.by.region.trck$battery.level,levels=unique(vmt.by.region.trck$battery.level)[mixedorder(unique(vmt.by.region.trck$battery.level))])

	output <- list('trck.mv'=trck.mv,'td.dot'=td.dot,'tl.dot'=tl.dot,'trck.ch'=trck.ch,'trcks'=trcks,'trck.ch'=trck.ch,'en.trck'=en.trck,'batt.trck'=batt.trck,'by.r.trck'=by.r.trck,'vmt.trck'=vmt.trck,'vmt.by.region.trck'=vmt.by.region.trck)
	return(output)
}

prepData.generators <- function(res) {
	# Merit order
	geners <- copy(generators)
	geners$g <- as.character(geners$g)
	geners$FuelType <- as.character(geners$FuelType)
	geners <- merge(x=geners,y=fuels,by='FuelType',all.x=TRUE)
	geners$Simplified <- factor(geners$Simplified,levels=meritOrder)

	geners[,g:=as.numeric(g)]
	res[['g-t']][,g:=as.numeric(g)]
	generation <- merge(x=res[['g-t']],y=geners,by='g',all.x=TRUE)
	generation <- generation[,list(generation=sum(generation),base.generation=sum(base.generation)),by=list(run,r,Simplified,t)]
	generation[,consq.generation:=generation-base.generation]
	generation <- generation[complete.cases(generation),]

	gen.cost <- merge(x=res[['g-t']],y=geners,by='g',all.x=TRUE)
	gen.cost <- gen.cost[,list(energyCost=sum(generationCosts*(generation-base.generation))),by=list(r,run)]

	output <- list('geners'=geners,'generation'=generation,'gen.cost'=gen.cost)
	return(output)
}

prepData.costs <- function(res,gen.cost,inputs) {
	# Cost
	costs <- join.on(join.on(res[['rmob']],res[['rmob-t']][,.(demandChargeCost=sum(demandChargeCost),vehicleMaintCost=sum(vehicleMaintCost),truckvehicleMaintCost=sum(truckvehicleMaintCost)),by=c('run','rmob')],c('run','rmob'),c('run','rmob'),c('demandChargeCost','vehicleMaintCost','truckvehicleMaintCost')),inputs$sets$rmobtor,'rmob','rmob')
	costs <- merge(x=costs,y=gen.cost,by=c('r','run'),all.x=TRUE)
  
	# JANKY FIX JUST TO MAKE PLOT (since the merge duplicates the values)
	costs$energyCost <- costs$energyCost/2

	return(costs)
}

prepData.factor.all <- function(all.inputs,inputs,res,by.r,by.r.trck,veh.ch,trck.ch,personal.ev.ch,costs,vmt.by.region,vmt.by.region.trck,geners,vehs) {
	param.names <- exper$param.names
	run.params <- copy(exper$runs)[,run:=1:.N]
	n.days.in.run <- length(inputs$set$t)/24

	factor.fleets.prepped <- prepData.factor.fleets(param.names,run.params,all.inputs,by.r,by.r.trck,res)
	factor.utilization.prepped <- prepData.factor.utilization(param.names,run.params,inputs,all.inputs,factor.fleets.prepped$to.plot.fleet,vehs,n.days.in.run)
	factor.chargers.prepped <- prepData.factor.chargers(param.names,run.params,all.inputs,by.r,by.r.trck)
	factor.charging.prepped <- prepData.factor.charging(param.names,run.params,veh.ch,trck.ch,personal.ev.ch,inputs)
	factor.costs.prepped <- prepData.factor.costs(param.names,run.params,inputs,factor.chargers.prepped$personal.chargers,costs,factor.fleets.prepped$personal.evs,vmt.by.region,n.days.in.run)
	factor.emissions.prepped <- prepData.factor.emissions(param.names,run.params,vmt.by.region,vmt.by.region.trck,res,geners,factor.fleets.prepped$to.plot.fleet,n.days.in.run)

	metric.units <- data.table(metric=c('Fleet Size','# Chargers','Peak Load','Emissions','Emissions_per_Passenger_Mile','Emissions_per_Capita','Cost','Cost_per_Passenger_Mile','Cost_per_Capita','SAEV_Utilization','HDV_Utilization','Person_Trips_per_SAEV'),
		scale.factor=c(1e6,1e6,1,1e6,1e-6,1,1e9,1,1,1,1,1),
		label=c('Millions of Vehicles','Millions of Chargers','Peak Load (GW)','Emissions (Million Tonnes CO2eq)','Emissions (grams CO2eq per Passenger-Mile)','Emissions (Tonnes CO2eq per Capita)','Cost (Billions of $)','Cost ($ per Passenger-Mile)','Cost ($ per Capita)','Vehicle-Hours per Day','Vehicle-Hours per Day','Person-trips per SAEV per Day'))
	factor.labels <- c('fractionSAEVs'='Fraction SAEVs in Fleet','fractionSmartCharging'='Fraction Smart Charging in Private Fleet','renewableScalingFactor'='Solar & Wind Capacity Scaling Factor')
	for(param.name in param.names){
		if(!param.name %in% names(factor.labels)){
			factor.labels <- c(factor.labels,streval(pp("c('",param.name,"'='",param.name,"')")))
		}
	}

	all <- rbindlist(list(factor.fleets.prepped$to.plot.fleet,factor.chargers.prepped$to.plot.chargers,factor.charging.prepped$to.plot.peak.ch,factor.emissions.prepped$to.plot.em,factor.emissions.prepped$to.plot.em.per.pass.mile,factor.emissions.prepped$to.plot.em.per.capita,factor.costs.prepped$to.plot.cost,factor.costs.prepped$to.plot.cost.per.pass.mile,factor.costs.prepped$to.plot.cost.per.capita,factor.utilization.prepped$to.plot.saev.hours,factor.utilization.prepped$to.plot.trips.per.saev),fill=T)
	all[,metric:=factor(metric,levels = c('Fleet Size','# Chargers','Peak Load','Cost','Emissions','Emissions_per_Passenger_Mile','Emissions_per_Capita','Cost_per_Passenger_Mile','Cost_per_Capita','SAEV_Utilization','Person_Trips_per_SAEV'))]
	the.cols <- all$col
	names(the.cols) <- all$variable

	return(list('metric.units'=metric.units,'factor.labels'=factor.labels,'all'=all,'the.cols'=the.cols,'to.plot.ch.agg'=factor.charging.prepped$to.plot.ch.agg,'the.ch.cols'=factor.charging.prepped$the.ch.cols))
}

prepData.factor.fleets <- function(param.names,run.params,all.inputs,by.r,by.r.trck,res) {
	personal.ev.conversion.eff <- 0.325
	personal.evs <- rbindlist(lapply(1:length(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVFleetSize[,.(value=sum(value),run=i),by='type'] }))[,.(run,b=pp("Private_",type),value)]
	personal.evs <- join.on(personal.evs,run.params,'run','run')
	personal.evs[,batt.kwh:=unlist(lapply(str_split(b,"EV"),function(ll){ as.numeric(ll[2])}))*personal.ev.conversion.eff]

	to.plot.fleet <- merge(x=by.r,y=run.params,by='run')
	to.plot.fleet <- rbindlist(list(to.plot.fleet[group=='Fleet',.(value=sum(fleetSize),bplus=pp('SAEV_BEV',substr(b,2,nchar(b)))),by=c('run',param.names,'b')],personal.evs),fill=T)
	not.needed <- to.plot.fleet[,sum(value),by='b'][V1<=1]$b
	to.plot.fleet <- to.plot.fleet[!b%in%not.needed]
	to.plot.fleet[,variable:=ifelse(is.na(bplus),b,bplus)]
	to.plot.fleet[,metric:='Fleet Size']

	to.plot.truckfleet <- merge(x=by.r.trck,y=run.params,by='run')
	to.plot.truckfleet <- to.plot.truckfleet[group=='Fleet',.(value=sum(truckfleetSize),bplus=pp('HDV_BEV',substr(tb,3,nchar(tb)))),by=c('run',param.names,'tb')]
	names(to.plot.truckfleet)[names(to.plot.truckfleet)=='tb'] <- 'b'
	not.needed <- to.plot.truckfleet[,sum(value),by='b'][V1<=1]$b
	to.plot.truckfleet <- to.plot.truckfleet[!b%in%not.needed]
	to.plot.truckfleet[,variable:=ifelse(is.na(bplus),b,bplus)]
	to.plot.truckfleet[,metric:='Fleet Size']

	to.plot.fleet <- rbindlist(list(to.plot.fleet,to.plot.truckfleet),fill=T)
	to.plot.fleet[,col:=getPalette(b)[match(to.plot.fleet$b,u(to.plot.fleet$b))]]
	to.plot.fleet <- join.on(to.plot.fleet,res[['b']],c('run','b'),c('run','b'))
	to.plot.fleet <- join.on(to.plot.fleet,res[['tb']],c('run','b'),c('run','tb'))
	to.plot.fleet[is.na(batteryCapacity)&grepl('Private',b,fixed=TRUE),':='(batteryCapacity=batt.kwh,conversionEfficiency=personal.ev.conversion.eff)]
	to.plot.fleet[is.na(batteryCapacity)&grepl('tb',b,fixed=TRUE),':='(batteryCapacity=truckbatteryCapacity,conversionEfficiency=truckconversionEfficiency)]

	return(list('personal.evs'=personal.evs,'to.plot.fleet'=to.plot.fleet))
}

prepData.factor.utilization <- function(param.names,run.params,inputs,all.inputs,to.plot.fleet,vehs,n.days.in.run) {
	to.plot.saev.hours <- streval(pp('vehs[,.(Charging=sum(',pp('vehiclesCharging.',all.inputs[[1]]$sets$l,collapse='+'),')/n.days.in.run,Moving=sum(',pp('vehiclesMoving.',str_replace_all(all.inputs[[1]]$sets$d,"-","."),collapse='+'),')/n.days.in.run,Idle=sum(vehiclesIdle)/n.days.in.run),by="run"]'))
	to.plot.saev.hours <- data.table(melt(join.on(to.plot.saev.hours,to.plot.fleet[grepl('SAEV',bplus,fixed=TRUE),.(n.veh=sum(value)),by='run'],'run','run'),id.vars=c('run','n.veh')))
	to.plot.saev.hours[,':='(metric='SAEV_Utilization',value=value/n.veh)]
	to.plot.saev.hours <- join.on(to.plot.saev.hours,run.params,'run','run')
	to.plot.saev.hours[,col:=getPalette(variable)[match(variable,u(variable))]]
	person.trips <- res[['b-d-rmob-t']][,.(personTripsPerDay=sum(demandAllocated)/n.days.in.run),by=c('run','b')]
	to.plot.trips.per.saev <- join.on(to.plot.fleet[!is.na(bplus)],person.trips,c('run','b'),c('run','b'))
	to.plot.trips.per.saev[,':='(metric='Person_Trips_per_SAEV',value=personTripsPerDay/value)]

	return(list('to.plot.saev.hours'=to.plot.saev.hours,'to.plot.trips.per.saev'=to.plot.trips.per.saev))
}

prepData.factor.chargers <- function(param.names,run.params,all.inputs,by.r,by.r.trck) {	
	personal.chargers <- rbindlist(lapply(1:length(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargers[,.(value=sum(value),run=i),by=c('type','level')] }))[,.(run,type,level,value)]
	personal.chargers[,variable:=pp(type,'_Chgr')]
	personal.chargers[,level.kw:=ifelse(level=='L1',1.5,ifelse(level=='L2',6.7,50))]
	personal.chargers <- join.on(personal.chargers,run.params,'run','run')

	to.plot.chargers <- merge(x=by.r,y=run.params,by='run')
	to.plot.chargers <- to.plot.chargers[group=='Chargers',.(value=sum(numChargers)),by=c('run',param.names,'l')]
	to.plot.chargers[,level.kw:=as.numeric(substr(l,2,nchar(l)))]
	to.plot.chargers[,variable:=pp('SAEV_Chgr_',ifelse(level.kw<=20,'AC','DC'))]

	to.plot.chargers.trcks <- merge(x=by.r.trck,y=run.params,by='run')
	to.plot.chargers.trcks <- to.plot.chargers.trcks[group=='Chargers',.(value=sum(trucknumChargers)),by=c('run',param.names,'tl')]
	to.plot.chargers.trcks[,level.kw:=as.numeric(substr(tl,3,nchar(tl)))]
	to.plot.chargers.trcks[,variable:=pp('HDV_Chgr_',ifelse(level.kw<=20,'AC','DC'))]

	to.plot.chargers <- rbindlist(list(to.plot.chargers,to.plot.chargers.trcks,personal.chargers),fill=T)
	to.plot.chargers[,l:=ifelse(grepl('HDV',variable,fixed=TRUE),paste0('HDV',str_pad(trunc(level.kw),4,pad='0')),l)]
	to.plot.chargers <- to.plot.chargers[,.(value=sum(value),l=l[1]),by=c('run','variable',param.names)]
	to.plot.chargers[,metric:='# Chargers']
	to.plot.chargers[is.na(l),l:=variable]
	to.plot.chargers[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),ifelse(substr(l,1,1)=='H',pp('b',l),l))]
	to.plot.chargers[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]

	return(list('personal.chargers'=personal.chargers,'to.plot.chargers'=to.plot.chargers))
}

prepData.factor.charging <- function(param.names,run.params,veh.ch,trck.ch,personal.ev.ch,inputs) {
	to.plot.ch <- rbindlist(list(veh.ch,trck.ch,personal.ev.ch),fill=T,use.names=T)
	to.plot.ch <- merge(x=to.plot.ch,y=run.params,by='run')
	to.plot.ch <- join.on(to.plot.ch,to.plot.ch[,.(gw=sum(gw.charging,na.rm=T)),by=c('run','t')][,.(peak.t=t[which.max(gw)]),by='run'],'run','run')
	to.plot.ch[is.na(l),l:=tl]

	to.plot.peak.ch <- to.plot.ch[t==peak.t,.(value=sum(gw.charging,na.rm=T)),by=c('run','l',param.names)]
	to.plot.peak.ch[,level.kw:=ifelse(grepl('tL',l,fixed=TRUE),as.numeric(substr(l,3,nchar(l))),as.numeric(substr(l,2,nchar(l))))]
	to.plot.peak.ch[,variable:=ifelse(l=='Private EVs','Private_EV_Load',ifelse(grepl('tL',l,fixed=TRUE),'HDV_Load_DC',pp('SAEV_Load_',ifelse(level.kw<=20,'AC','DC'))))]
	to.plot.peak.ch <- to.plot.peak.ch[,.(value=sum(value),l=l[1],level.kw=level.kw[1]),by=c('run',param.names,'variable')]
	to.plot.peak.ch[,metric:='Peak Load']
	to.plot.peak.ch[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),ifelse(substr(l,1,1)=='t',pp('b',l),l))]
	to.plot.peak.ch[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]

	to.plot.ch.agg <- to.plot.ch[,.(gwh=sum(gw.charging,na.rm=T)),by=c('run',param.names,'charger.level','l','t')]
	to.plot.ch.agg <- disag.the.private.load(to.plot.ch.agg,inputs$parameters$personalEVUnmanagedLoads)
	to.plot.ch.agg[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
	to.plot.ch.agg[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
	the.ch.cols <- to.plot.ch.agg$col
	names(the.ch.cols) <- to.plot.ch.agg$charger.level

	return(list('to.plot.ch'=to.plot.ch,'to.plot.peak.ch'=to.plot.peak.ch,'to.plot.ch.agg'=to.plot.ch.agg,'the.ch.cols'=the.ch.cols))
}

prepData.factor.costs <- function(param.names,run.params,inputs,personal.chargers,costs,personal.evs,vmt.by.region,n.days.in.run) {
	privateChargerLifetime <- inputs$parameters$chargerLifetime$value[1]*365
	privateVehicleLifetime <- 11*365
	privateBatteryLifetime <- privateVehicleLifetime
	dailyDiscount <- discountRate/365
	vehCapCost <- 30e3
	vehPerDayCost <- 600 / 365
	vehPerMileCosts <- 0.09
	private.charger.levels <- u(personal.chargers$level.kw)
	infra.cost <- join.on(personal.chargers,data.table(level.kw=private.charger.levels,cost.per.kw=l10ChargerCost + c(private.charger.levels-10)*chargerCostSuperlinear),'level.kw','level.kw')
	infra.cost[level=='L1',cost.per.kw:=50]
	infra.cost[,cost:=cost.per.kw*level.kw*value]
	infra.cost <- infra.cost[,.(privateInfrastructureCost=(sum(cost)*dailyDiscount*(1+dailyDiscount)^privateChargerLifetime)/((1+dailyDiscount)^privateChargerLifetime-1)),by='run']
	veh.amort.ratio <- dailyDiscount*(1+dailyDiscount)^privateVehicleLifetime / ((1+dailyDiscount)^privateVehicleLifetime - 1)
	batt.amort.ratio <- dailyDiscount*(1+dailyDiscount)^privateBatteryLifetime / ((1+dailyDiscount)^privateBatteryLifetime - 1)
	veh.cost <- personal.evs[,.(n=sum(value)),by=c('run','batt.kwh')][,.(privateFleetCost=sum(n*(vehPerDayCost + vehCapCost*veh.amort.ratio + batt.kwh*inputs$parameters$batteryCapitalCost$value*batt.amort.ratio + vehPerMileCosts*12e3/365))),by='run']

	# ICE Cost comparison
	iceCapCost <- 27.66e3
	iceNumVehicles <- 255e6
	iceAnnualFleetCost <- iceCapCost * iceNumVehicles * discountRate * (1 + discountRate)^11 / ((1+discountRate)^11 - 1)
	icePerMileCost <- 0.12
	iceAnnualVMT <- 2883e9
	iceTotalFuelCost <- 370.5e9
	iceAnnualCost <- iceAnnualFleetCost + iceTotalFuelCost + icePerMileCost*iceAnnualVMT
	iceTCOPerMile <- iceAnnualCost/iceAnnualVMT

	costs[,totalFleetCost:=vehicleMaintCost/n.days.in.run+fleetCost]
	costs[,totalTruckFleetCost:=truckvehicleMaintCost/n.days.in.run+truckfleetCost]
	costs[,totalEnergyCost:=demandChargeCost/n.days.in.run+energyCost/n.days.in.run]
	to.plot.cost <- merge(x=costs,y=run.params,by='run')
	to.plot.cost <- data.table(melt(to.plot.cost,measure.vars=c('totalEnergyCost','totalFleetCost','totalTruckFleetCost','infrastructureCost','truckinfrastructureCost'),id.vars=c('run',param.names)))[,.(value=sum(value)),by=c('variable','run',param.names)]
	to.plot.cost <- rbindlist(list(to.plot.cost,melt(join.on(join.on(infra.cost,veh.cost,'run','run'),run.params,'run','run'),measure.vars=c('privateInfrastructureCost','privateFleetCost'),id.vars=c('run',param.names))),fill=T)
	to.plot.cost[,max.value:=max(to.plot.cost[,.(val=sum(value)),by='run']$val)]
	to.plot.cost[,metric:='Cost']
	to.plot.cost[,variable.short:=variable]
	to.plot.cost[,value:=value*weekday.to.year.factor]
	cost.key <- data.table(variable=c('infrastructureCost','truckinfrastructureCost','totalFleetCost','totalTruckFleetCost','totalEnergyCost','privateInfrastructureCost','privateFleetCost'),cost.key=c('SAEV Infrastructure','HDV Infrastructure','SAEV Fleet','HDV Fleet','Energy','Private Infrastructure','Private Fleet'))
	to.plot.cost <- join.on(to.plot.cost,cost.key,'variable.short','variable')
	to.plot.cost[,variable:=pp('Cost: ',cost.key)]
	to.plot.cost[,col:=getPalette(variable.short)[match(variable.short,u(variable.short))]]
	to.plot.cost.per.pass.mile <- join.on(copy(to.plot.cost),vmt.by.region[,.(daily.pmt=sum(daily.pmt)),by='run'],'run','run')
	to.plot.cost.per.pass.mile[,metric:='Cost_per_Passenger_Mile']
	to.plot.cost.per.pass.mile[,value:=value/(daily.pmt*weekday.to.year.factor)]
	to.plot.cost.per.capita <- copy(to.plot.cost)
	to.plot.cost.per.capita[,metric:='Cost_per_Capita']
	to.plot.cost.per.capita[,value:=value/330e6] # 2020 US Population

	return(list('to.plot.cost'=to.plot.cost,'to.plot.cost.per.pass.mile'=to.plot.cost.per.pass.mile,'to.plot.cost.per.capita'=to.plot.cost.per.capita))
}

prepData.factor.emissions <- function(param.names,run.params,vmt.by.region,vmt.by.region.trck,res,geners,to.plot.fleet,n.days.in.run) {
	dailyDiscount <- discountRate/365
	saev.lifetimes <- vmt.by.region[,.(vehicleLifetime=vehicleLifetime[1],batteryLifetime=batteryLifetime[1]),by=c('run','b')]
	saev.lifetimes[,veh.amort.ratio:=dailyDiscount*(1+dailyDiscount)^(vehicleLifetime*365) / ((1+dailyDiscount)^(vehicleLifetime*365) - 1)]
	saev.lifetimes[,batt.amort.ratio:=dailyDiscount*(1+dailyDiscount)^(batteryLifetime*365) / ((1+dailyDiscount)^(batteryLifetime*365) - 1)]
	if(is.character(res[['g-t']]$g[1]))res[['g-t']][,g:=as.numeric(g)]
	if(is.character(geners$g[1]))geners[,g:=as.numeric(g)]

	hdv.lifetimes <- vmt.by.region.trck[,.(vehicleLifetime=truckvehicleLifetime[1],batteryLifetime=truckbatteryLifetime[1]),by=c('run','tb')]
	hdv.lifetimes[,veh.amort.ratio:=dailyDiscount*(1+dailyDiscount)^(vehicleLifetime*365) / ((1+dailyDiscount)^(vehicleLifetime*365) - 1)]
	hdv.lifetimes[,batt.amort.ratio:=dailyDiscount*(1+dailyDiscount)^(batteryLifetime*365) / ((1+dailyDiscount)^(batteryLifetime*365) - 1)]
	names(hdv.lifetimes)[names(hdv.lifetimes)=='tb'] <- 'b'

	all.lifetimes <- rbind(saev.lifetimes,hdv.lifetimes)

	to.plot.em <- join.on(res[['g-t']],geners,'g','g')[!Simplified%in%c('Solar','Wind','Hydro','Pumps','Nuclear','Geothermal')]
	to.plot.em[is.na(Simplified) | Simplified=='Biomass',Simplified:='Other']
	to.plot.em <- to.plot.em[,list(emissions=sum(generationCO2*generation),base.emissions=sum(generationCO2*base.generation)),by=c('run','Simplified')]
	to.plot.em[,value:=(emissions-base.emissions)/n.days.in.run]
	
	privateVehicleLifetime <- 11*365
	privateBatteryLifetime <- privateVehicleLifetime
	veh.manuf.em <- to.plot.fleet[,.(n=sum(value),batteryCapacity=batteryCapacity[1]),by=c('run','b')]
	veh.manuf.em <- join.on(veh.manuf.em,all.lifetimes,c('run','b'),c('run','b'))
	veh.manuf.em[is.na(vehicleLifetime),':='(vehicleLifetime=privateVehicleLifetime/365,batteryLifetime=privateBatteryLifetime/365,veh.amort.ratio=dailyDiscount*(1+dailyDiscount)^(privateVehicleLifetime) / ((1+dailyDiscount)^(privateVehicleLifetime) - 1),batt.amort.ratio=dailyDiscount*(1+dailyDiscount)^(privateBatteryLifetime) / ((1+dailyDiscount)^(privateBatteryLifetime) - 1))]
	veh.manuf.em[,value:=n*7.1*veh.amort.ratio+n*0.11*batteryCapacity*batt.amort.ratio] # 7.1 and 0.11 are tons per vehicle
	veh.manuf.em[,Simplified:='Vehicle Manufacture']
	to.plot.em <- rbindlist(list(to.plot.em,veh.manuf.em[,.(value=sum(value)),by=c('Simplified','run')]),use.names=T,fill=T)
	to.plot.em[,value:=value*weekday.to.year.factor]
	#to.plot.em<- to.plot.em[complete.cases(to.plot.em),]
	to.plot.em[,metric:='Emissions']
	to.plot.em[,variable:=factor(as.character(Simplified))]
	to.plot.em <- join.on(to.plot.em,run.params,'run','run')
	to.plot.em[,col:=getPalette(variable)[match(variable,u(variable))]]
	to.plot.em.per.pass.mile <- join.on(copy(to.plot.em),vmt.by.region[,.(daily.pmt=sum(daily.pmt)),by='run'],'run','run')
	to.plot.em.per.pass.mile[,metric:='Emissions_per_Passenger_Mile']
	to.plot.em.per.pass.mile[,value:=value/(daily.pmt*weekday.to.year.factor)]
	to.plot.em.per.capita <- copy(to.plot.em)
	to.plot.em.per.capita[,metric:='Emissions_per_Capita']
	to.plot.em.per.capita[,value:=value/330e6] # 2020 US Population

	return(list('to.plot.em'=to.plot.em,'to.plot.em.per.pass.mi'=to.plot.em.per.pass.mile,'to.plot.em.per.capita'=to.plot.em.per.capita))
}

prepData.maps <- function() {
	# Key for matching regions to shapefile region names
	region.key <- data.table(r=c('ENC','MAT-NL','MAT-NY','MTN','NENG','PAC-CA','PAC-NL','SAT-FL','SAT-NL','WNC','WSC-NL','WSC-TX','ESC'),NAME=c('East North Central','Middle Atlantic','New York','Mountain','New England','California','Pacific','Florida','South Atlantic','West North Central','West South Central','Texas','East South Central'))

	# Map data
	eia.regions <- st_read(file.path(pp(gem.raw.inputs,'census-division-plus-big-4/','census-division-plus-big-four.shp')))
	centroids <- matrix(unlist(do.call(rbind,st_centroid(eia.regions))['geometry',]),nrow=2)
	centroids <- data.table(NAME=eia.regions$NAME,long=centroids[1,],lat=centroids[2,])
	centroids <- merge(x=centroids,y=region.key,by='NAME')

	states <- map_data('state')

	outputs <- list('region.key'=region.key,'eia.regions'=eia.regions,'centroids'=centroids,'states'=states)
	return(outputs)
}

disag.the.private.load <- function(df,personalEVUnmanagedLoads){
	df[l!='Private EVs'&substr(l,1,1)=='L',charger.level:=pp('SAEV: ',charger.level)]
	df[l!='Private EVs'&substr(l,1,1)=='t',charger.level:=pp('HDV: ',charger.level)]
	# Split private EV out by charger level
	proportional.split <- personalEVUnmanagedLoads[,.(frac=sum(value)/sum(personalEVUnmanagedLoads$value)),by='l']
	private.disag <- join.on(rbindlist(list(df[l=='Private EVs'][,ll:='L1'],df[l=='Private EVs'][,ll:='L2'],df[l=='Private EVs'][,ll:='L3'])),proportional.split,'ll','l')
	private.disag[,gwh:=frac*gwh]
	private.disag[,charger.level:=ifelse(ll=='L1','Private: 1.5kW',ifelse(ll=='L2','Private: 7kW','Private: 50kW'))]
	private.disag[,l:=ifelse(ll=='L1','Private: 1.5kW',ifelse(ll=='L2','Private: 7kW','Private: 50kW'))]
	rbindlist(list(df[l!='Private EVs'],private.disag),fill=T)
}

plot.grid.all <- function(run.i,plots.dir,res,geners) {
	plot.grid.generation(run.i,plots.dir,res,geners)
	plot.grid.emissions(run.i,plots.dir,res,geners)
}

plot.grid.generation <- function(run.i,plots.dir,res,geners) {
	to.plot <- merge(x=res[['g-t']][run==run.i],y=geners,by='g',all.x=TRUE)
	to.plot <- to.plot[,list(generation=sum(generation),base.generation=sum(base.generation)),by=list(r,Simplified,t)]
	to.plot[,consq.generation:=generation-base.generation]
	to.plot[,consq.generation.pos:=ifelse(consq.generation>=0,consq.generation,0)]
	to.plot[,consq.generation.neg:=ifelse(consq.generation<0,consq.generation,-1)]
	to.plot <- to.plot[complete.cases(to.plot),]
	cols <- c('Other'='#7fc97f','Coal'='gray27','Hydro'='#386cb0','Natural Gas'='gray73','Nuclear'='#fb9a99','Solar'='#ffff99','Wind'='lightskyblue')
	pdf.scale <- 1
	p <- ggplot(to.plot,aes(x=t,y=generation/1000,fill=Simplified))+
		geom_area()+
		xlab('Hour')+
		ylab('Generation (GW)')+
		scale_fill_manual(name='Fuel Type',values=cols)+
		facet_wrap(~r,scale='free_y')+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_generation-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

	p <- ggplot(to.plot,aes(x=t,fill=Simplified))+
		geom_area(aes(y=consq.generation.pos/1000))+
		geom_area(aes(y=consq.generation.neg/1000))+
		xlab('Hour')+
		ylab('Generation (GW)')+
		scale_fill_manual(name='Fuel Type',values=cols)+
		facet_wrap(~r,scale='free_y')+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_generation-cnsq-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
}

plot.grid.emissions <- function(run.i,plots.dir,res,geners) {
	to.plot <- merge(x=res[['g-t']][run==run.i],y=geners,by='g',all.x=TRUE)
	to.plot <- to.plot[,list(emissions=sum(generationCO2*generation),base.emissions=sum(generationCO2*base.generation)),by=list(r,Simplified,t)]
	to.plot[,consq.emissions:=emissions-base.emissions]
	to.plot <- to.plot[complete.cases(to.plot),]
	pdf.scale <- 1
	p <- ggplot(to.plot,aes(x=t,y=emissions/1000,fill=Simplified))+
		geom_area()+
		xlab('Hour')+
		ylab('Hourly CO2 Emissions (tons)')+
		scale_fill_discrete(name='Fuel Type')+
		facet_wrap(~r,scale='free_y')+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_emissions-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

	p <- ggplot(to.plot,aes(x=t,y=consq.emissions/1000,fill=Simplified))+
		geom_area()+
		xlab('Hour')+
		ylab('Hourly CO2 Emissions (tons)')+
		scale_fill_discrete(name='Fuel Type')+
		facet_wrap(~r,scale='free_y')+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_emissions-cnsq-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

	p <- ggplot(to.plot,aes(x=t,y=consq.emissions/1000,colour=Simplified))+
		geom_line()+
		xlab('Hour')+
		ylab('Hourly CO2 Emissions (tons)')+
		scale_colour_discrete(name='Fuel Type')+
		facet_wrap(~r,scale='free_y')+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_emissions-cnsq-total-by-region-fuel-line.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
}

plot.lightduty.all <- function(run.i,plots.dir,vehs,exper,all.inputs,veh.ch,personal.ev.ch,en,by.r,day.axis.breaks) {
	plot.lightduty.numberOfVehicles(run.i,plots.dir,vehs,day.axis.breaks)
	plot.lightduty.charging(run.i,plots.dir,exper,all.inputs,veh.ch,personal.ev.ch,day.axis.breaks)
	plot.lightduty.personalVehs(run.i,plots.dir,all.inputs,personal.ev.ch,veh.ch,day.axis.breaks)
	plot.lightduty.energyBalance(run.i,plots.dir,en)
	plot.lightduty.fleetDetails(run.i,plots.dir,by.r)
}

plot.lightduty.numberOfVehicles <- function(run.i,plots.dir,vehs,day.axis.breaks) {
	to.plot <- data.table(melt(vehs[run==run.i],id.vars=c('b','t','rmob','run')))
	to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),pp('Charging: SAEV ',substr(b,2,5),'mi'),ifelse(variable=='vehiclesIdle',pp('Idle: SAEV ',substr(b,2,5),'mi'),pp('Moving: SAEV ',substr(b,2,5),'mi')))]
	to.remove <- to.plot[,sum(value),by='variable.simp'][V1<1e-4]$variable.simp
	to.plot <- to.plot[!variable.simp%in%to.remove]
	to.plot[,variable.simp:=factor(variable.simp,c(rev(u(to.plot$variable.simp))))]
	if(nrow(to.plot)>0){
		setkey(to.plot,variable.simp)
		p<-ggplot(to.plot[,.(value=sum(value)),by=c('t','variable.simp')],aes(x=t,y=value/1000,fill=variable.simp))+
			geom_area()+
			xlab('Hour')+
			ylab('Number of vehicles (thousands)')+
			scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
			scale_x_continuous(breaks=day.axis.breaks)+
			theme_bw()
		pdf.scale <- 1
		ggsave(pp(plots.dir,'/run-',run.i,'/_ld_num-vehs-simple-2.pdf'),p,width=10*pdf.scale,height=5*pdf.scale,units='in')
	  
	to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
	to.plot[,variable.simp:=factor(variable.simp,c('Idle','Moving','Charging'))]
	setkey(to.plot,variable.simp)
	p <- ggplot(to.plot,aes(x=t,y=value/1000,fill=variable.simp))+
		geom_area()+
		xlab('Hour')+
		ylab('Number of vehicles (thousands)')+
		facet_wrap(~rmob,scales='free_y')+
		scale_x_continuous(breaks=day.axis.breaks)+
		scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
	}
}

plot.lightduty.charging <- function(run.i,plots.dir,exper,all.inputs,veh.ch,personal.ev.ch,day.axis.breaks) {
	p <- ggplot(veh.ch[run==run.i,.(gw.charging=sum(gw.charging)),by=.(t,charger.level,rmob)],aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
		geom_area()+
		facet_wrap(~rmob,scales='free_y')+
		scale_fill_manual(values = getPalette(veh.ch$charger.level),guide=guide_legend(reverse=F))+
		labs(x='Hour',y='Load (GW)',fill='Charger Level')+
		theme_bw()
	pdf.scale <- 1
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_charging-saevs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
	
	to.plot <- data.table(rbindlist(list(veh.ch,personal.ev.ch),fill=T,use.names=T)[run==run.i])
	to.plot[,gwh:=gw.charging]
	to.plot[,ll:='']
	to.plot <- disag.the.private.load(to.plot,all.inputs[[run.i]]$parameters$personalEVUnmanagedLoads)
	to.plot[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
	to.plot[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
	the.ch.cols <- to.plot$col
	names(the.ch.cols) <- to.plot$charger.level
	
	p <- ggplot(to.plot[,.(gwh=sum(gwh)),by=c('t','charger.level','rmob')],aes(x=t,y=gwh,fill=charger.level))+
		geom_area()+
		facet_wrap(~rmob,scales='free_y')+
		scale_fill_manual(values = the.ch.cols)+
		labs(x='Hour',y='Load (GW)',fill='Charger Level')+
		theme_bw()
	pdf.scale <- 1
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_charging-all.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
	
	p <- ggplot(to.plot[,.(gwh=sum(gwh)),by=c('t','charger.level')],aes(x=t,y=gwh,fill=charger.level))+
		scale_x_continuous(breaks=day.axis.breaks)+
		geom_area()+
		scale_fill_manual(values = the.ch.cols)+
		labs(x='Hour',y='Load (GW)',fill='Charger Level')+
		theme_bw()
	pdf.scale <- 1
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_charging-all-agg.pdf'),p,width=10*pdf.scale,height=5*pdf.scale,units='in')
}

plot.lightduty.personalVehs <- function(run.i,plots.dir,all.inputs,personal.ev.ch,veh.ch,day.axis.breaks) {
	to.plot <- rbindlist(list(veh.ch,personal.ev.ch),fill=T,use.names=T)[run==run.i]
	to.plot[,gwh:=gw.charging]
	to.plot[,ll:='']
	to.plot <- disag.the.private.load(to.plot,all.inputs[[run.i]]$parameters$personalEVUnmanagedLoads)
	to.plot[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
	to.plot[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
	the.ch.cols <- to.plot$col
	names(the.ch.cols) <- to.plot$charger.level

	personal.evs.total <- all.inputs[[run.i]]$parameters$personalEVFleetSize
	personal.evs.total <- personal.evs.total[rep(1:nrow(personal.evs.total),length(u(personal.ev.ch$t)))]
	personal.evs.total[,t:=rep(1:length(u(personal.ev.ch$t)),each=nrow(personal.evs.total)/length(u(personal.ev.ch$t)))]
	personal.evs.total <- personal.evs.total[,.(Total=sum(value)),by=.(t,rmob)]

	chts.trips$n <- 1:nrow(chts.trips)
	chts.hours <- unlist(sapply(chts.trips$n,function(x) seq(chts.trips$start_hour[x],chts.trips$end_hour[x])))
	chts.hours <- data.table(table(chts.hours))
	names(chts.hours) <- c('Hour','Count')
	chts.hours[,Probability:=Count/sum(Count)]
	chts.hours[,Hour:=as.numeric(Hour)+1]
	chts.hours <- chts.hours[rep(1:nrow(chts.hours),length(u(personal.ev.ch$t))/24),]
	chts.hours[,t:=(1:length(u(personal.ev.ch$t)))]
	chts.hours <- chts.hours[,.(t,Probability)]

	personal.evs.total <- merge(x=personal.evs.total,y=chts.hours,by='t',all.x=TRUE)
	personal.evs.total[,Moving:=Total*Probability]

	personal.evs.ch.total <- to.plot[ll%in%c('L1','L2','L3'),.(gw.charging=sum(gw.charging)),by=.(ll,t,rmob)]
	personal.evs.ch.total[,Charging:=ifelse(ll=='L1',gw.charging*10^6/1.5,ifelse(ll=='L2',gw.charging*10^6/6.7,gw.charging*10^6/50))]
	personal.evs.ch.total <- personal.evs.ch.total[,.(Charging=sum(Charging)),by=.(t,rmob)]

	personal.evs.total <- merge(x=personal.evs.total,y=personal.evs.ch.total,by=c('t','rmob'))
	personal.evs.total[,Charging:=ifelse(Charging>Total,Total,Charging)]
	personal.evs.total[,Moving:=ifelse(Moving+Charging>Total,Total-Charging,Moving)]
	personal.evs.total[,Idle:=Total-Moving-Charging]
	personal.evs.total <- personal.evs.total[,.(Moving=sum(Moving),Charging=sum(Charging),Idle=sum(Idle)),by=.(t)]
	personal.evs.total[Idle<0,Idle:=0]
	personal.evs.total <- data.table(melt(personal.evs.total,id='t',variable.name='Activity',value.name='Cars'))
	names(personal.evs.total) <- c('t','Activity','Cars')
	personal.evs.total$Activity <- factor(personal.evs.total$Activity,levels=c('Idle','Moving','Charging'))

	p<-ggplot(personal.evs.total,aes(x=t,y=Cars/1000,fill=Activity))+
		geom_area()+
		xlab('Hour')+
		ylab('Number of vehicles (thousands)')+
		scale_fill_manual(name='Personal Vehicle activity',values=c('Idle'='#bdbdbd','Moving'='#3182bd','Charging'='#ee3a3a'))+
		scale_x_continuous(breaks=day.axis.breaks)+
		theme_bw()
	pdf.scale <- 1
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_num-vehs-simple-personal.pdf'),p,width=10*pdf.scale,height=5*pdf.scale,units='in')
}

plot.lightduty.energyBalance <- function(run.i,plots.dir,en) {
	pdf.scale <- 1
	p <- ggplot(en[run==run.i],aes(x=t,y=soc/10^6,colour=fct_rev(battery.level)))+
		geom_line()+
		xlab('Fleet Energy State (GWh)')+
		ylab('Hour')+
		facet_wrap(~rmob,scales='free_y')+
		scale_colour_manual(name='Vehicle battery size',values = rev(getPalette(en$battery.level)),guide=guide_legend(reverse=F))+
		theme_bw() 
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_soc.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
	p <- ggplot(en,aes(x=t,y=soc/10^6,fill=fct_rev(battery.level)))+
		geom_area()+
		xlab('Hour')+
		ylab('Fleet Energy State (GWh)')+
		facet_wrap(~rmob,scales='free_y')+
		scale_fill_manual(name='Vehicle battery size',values=rev(getPalette(en$battery.level)),guide=guide_legend(reverse=F))+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
}

plot.lightduty.fleetDetails <- function(run.i,plots.dir,by.r) {
	pdf.scale <- 1
	p <- ggplot(by.r[run==run.i],aes(x=rmob,y=value/1000,fill=fct_rev(var.clean)))+
		geom_bar(stat='identity')+
		xlab('Region')+
		ylab('Count (thousands)')+
		facet_wrap(~group,scales='free_y')+
		theme_bw()+
		theme(axis.text.x = element_text(angle = 50, hjust = 1))+
		scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(by.r$var.clean)),guide=guide_legend(reverse=F))
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

	to.plot <- by.r[run==run.i,.(value=value,percent=value/sum(value)*100,variable=var.clean),by=c('rmob','group')]
	to.plot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
	geo.ordered <- cbind(c(sapply(c('-RUR','-URB'),function(x){ pp(c('PAC-NL','PAC-CA','MTN','WNC','WSC-NL','WSC-TX','ENC','ESC','NENG','MAT-NL','MAT-NY','SAT-NL','SAT-FL'),x) })))[,1]
	to.plot[,rmob:=factor(rmob,geo.ordered)]
	setkey(to.plot,rmob,variable)

	p <- ggplot(to.plot,aes(x=factor(rmob),y=percent,fill=fct_rev(variable)))+
		geom_bar(stat='identity')+
		xlab('Region')+
		ylab('Percent')+
		facet_grid(group~urb,scales='free_x', space ='free_x')+
		theme_bw()+
		theme(axis.text.x = element_text(angle = 50, hjust = 1))+
		scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(to.plot$variable)),guide=guide_legend(reverse=F))
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_fleet-size-and-type-percent.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
	
	to.remove <- to.plot[,sum(value),by='variable'][V1==0]$variable
	to.plot <- to.plot[!variable%in%to.remove,.(value=sum(value)/1e6,percent=value/sum(value)*100),by=c('group','variable','urb')]

	if(nrow(to.plot)>0){
		p <- ggplot(to.plot,aes(x=urb,y=value,fill=variable))+
			geom_bar(stat='identity')+
			xlab('Regional Type')+
			ylab('Count in Millions')+
			facet_wrap(~group,scales='free_y')+ 
			theme_bw()+
			scale_fill_manual(name='Charger/Battery Level',values = getPalette(to.plot$variable),guide=guide_legend(reverse=F))
		ggsave(pp(plots.dir,'/run-',run.i,'/_ld_fleet-size-and-type-agg.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
	}
}

plot.heavyduty.all <- function(run.i,plots.dir,vehs,exper,all.inputs,veh.ch,en,by.r,day.axis.breaks) {
	plot.heavyduty.numberOfVehicles(run.i,plots.dir,vehs,day.axis.breaks)
	plot.heavyduty.charging(run.i,plots.dir,exper,all.inputs,veh.ch)
	plot.heavyduty.energyBalance(run.i,plots.dir,en)
	plot.heavyduty.fleetDetails(run.i,plots.dir,by.r)
}

plot.heavyduty.numberOfVehicles <- function(run.i,plots.dir,vehs,day.axis.breaks) {
	to.plot <- data.table(melt(vehs[run==run.i],id.vars=c('tb','t','rmob','run')))
	to.plot[,variable.simp:=ifelse(grepl('truckvehiclesCharging',variable),pp('Charging: SAEV ',substr(tb,3,7),'mi'),ifelse(variable=='truckvehiclesIdle',pp('Idle: SAEV ',substr(tb,3,7),'mi'),pp('Moving: SAEV ',substr(tb,3,7),'mi')))]
	to.remove <- to.plot[,sum(value),by='variable.simp'][V1<1e-4]$variable.simp
	to.plot <- to.plot[!variable.simp%in%to.remove]
	to.plot[,variable.simp:=factor(variable.simp,c(rev(u(to.plot$variable.simp))))]
	if(nrow(to.plot)>0){
		setkey(to.plot,variable.simp)
		p<-ggplot(to.plot[,.(value=sum(value)),by=c('t','variable.simp')],aes(x=t,y=value/1000,fill=variable.simp))+
			geom_area()+
			xlab('Hour')+
			ylab('Number of vehicles (thousands)')+
			scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
			scale_x_continuous(breaks=day.axis.breaks)+
			theme_bw()
		pdf.scale <- 1
		ggsave(pp(plots.dir,'/run-',run.i,'/_hd_num-vehs-simple-2.pdf'),p,width=10*pdf.scale,height=5*pdf.scale,units='in')
	  
	to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
	to.plot[,variable.simp:=factor(variable.simp,c('Idle','Moving','Charging'))]
	setkey(to.plot,variable.simp)
	p <- ggplot(to.plot,aes(x=t,y=value/1000,fill=variable.simp))+
		geom_area()+
		xlab('Hour')+
		ylab('Number of vehicles (thousands)')+
		facet_wrap(~rmob,scales='free_y')+
		scale_x_continuous(breaks=day.axis.breaks)+
		scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_hd_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
	}
}

plot.heavyduty.charging <- function(run.i,plots.dir,exper,all.inputs,veh.ch) {
	p <- ggplot(veh.ch[run==run.i,.(gw.charging=sum(gw.charging)),by=.(t,charger.level,rmob)],aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
		geom_area()+
		facet_wrap(~rmob,scales='free_y')+
		scale_fill_manual(values = getPalette(veh.ch$charger.level),guide=guide_legend(reverse=F))+
		labs(x='Hour',y='Load (GW)',fill='Charger Level')+
		theme_bw()
	pdf.scale <- 1
	ggsave(pp(plots.dir,'/run-',run.i,'/_hd_charging.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
}

plot.heavyduty.energyBalance <- function(run.i,plots.dir,en) {
	pdf.scale <- 1
	p <- ggplot(en[run==run.i],aes(x=t,y=soc/10^6,colour=fct_rev(battery.level)))+
		geom_line()+
		xlab('Fleet Energy State (GWh)')+
		ylab('Hour')+
		facet_wrap(~rmob,scales='free_y')+
		scale_colour_manual(name='Vehicle battery size',values = rev(getPalette(en$battery.level)),guide=guide_legend(reverse=F))+
		theme_bw() 
	ggsave(pp(plots.dir,'/run-',run.i,'/_hd_soc.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
	p <- ggplot(en,aes(x=t,y=soc/10^6,fill=fct_rev(battery.level)))+
		geom_area()+
		xlab('Hour')+
		ylab('Fleet Energy State (GWh)')+
		facet_wrap(~rmob,scales='free_y')+
		scale_fill_manual(name='Vehicle battery size',values=rev(getPalette(en$battery.level)),guide=guide_legend(reverse=F))+
		theme_bw()
	ggsave(pp(plots.dir,'/run-',run.i,'/_hd_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
}

plot.heavyduty.fleetDetails <- function(run.i,plots.dir,by.r) {
	pdf.scale <- 1
	p <- ggplot(by.r[run==run.i],aes(x=rmob,y=value/1000,fill=fct_rev(var.clean)))+
		geom_bar(stat='identity')+
		xlab('Region')+
		ylab('Count (thousands)')+
		facet_wrap(~group,scales='free_y')+
		theme_bw()+
		theme(axis.text.x = element_text(angle = 50, hjust = 1))+
		scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(by.r$var.clean)),guide=guide_legend(reverse=F))
	ggsave(pp(plots.dir,'/run-',run.i,'/_hd_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

	to.plot <- by.r[run==run.i,.(value=value,percent=value/sum(value)*100,variable=var.clean),by=c('rmob','group')]
	to.plot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
	geo.ordered <- cbind(c(sapply(c('-RUR','-URB'),function(x){ pp(c('PAC-NL','PAC-CA','MTN','WNC','WSC-NL','WSC-TX','ENC','ESC','NENG','MAT-NL','MAT-NY','SAT-NL','SAT-FL'),x) })))[,1]
	to.plot[,rmob:=factor(rmob,geo.ordered)]
	setkey(to.plot,rmob,variable)

	p <- ggplot(to.plot,aes(x=factor(rmob),y=percent,fill=fct_rev(variable)))+
		geom_bar(stat='identity')+
		xlab('Region')+
		ylab('Percent')+
		facet_grid(group~urb,scales='free_x', space ='free_x')+
		theme_bw()+
		theme(axis.text.x = element_text(angle = 50, hjust = 1))+
		scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(to.plot$variable)),guide=guide_legend(reverse=F))
	ggsave(pp(plots.dir,'/run-',run.i,'/_hd_fleet-size-and-type-percent.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
	
	to.remove <- to.plot[,sum(value),by='variable'][V1==0]$variable
	to.plot <- to.plot[!variable%in%to.remove,.(value=sum(value)/1e6,percent=value/sum(value)*100),by=c('group','variable','urb')]

	if(nrow(to.plot)>0){
		p <- ggplot(to.plot,aes(x=urb,y=value,fill=variable))+
			geom_bar(stat='identity')+
			xlab('Regional Type')+
			ylab('Count in Millions')+
			facet_wrap(~group,scales='free_y')+ 
			theme_bw()+
			scale_fill_manual(name='Charger/Battery Level',values = getPalette(to.plot$variable),guide=guide_legend(reverse=F))
		ggsave(pp(plots.dir,'/run-',run.i,'/_hd_fleet-size-and-type-agg.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
	}
}

make.1d.fleet.and.chargers.plot <- function(sub,code,freeCol,sub.dir,energy.by.r.l){
	print(sub)
	print(freeCol)
	to.plot <- sub[,.(value=value,percent=value/sum(value)*100,l=l,variable=var.clean,trips=sum(totalSAEVTrips)/n.days.in.run),by=c('run','rmob','group',freeCol)]
	to.plot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
	to.remove <- to.plot[,sum(value),by=c('variable')][V1==0]$variable
	to.plot <- to.plot[!variable%in%to.remove,.(value=sum(value)/1e6,trips=trips[1]/1e6,l=l[1]),by=c('run','group','variable','urb',freeCol)]
	energy.by.r.l[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
	energy.by.r.l.agg <- energy.by.r.l[,.(energyCharged=sum(energyCharged)),by=c('urb','l','run')]
	to.plot <- join.on(to.plot,energy.by.r.l.agg,c('urb','l','run'),c('urb','l','run'))
	fleet.tot <- to.plot[group=='Fleet',.(fleet.tot=sum(value)),by=c('urb',freeCol)]
	trips.tot <- to.plot[group=='Chargers',.(trips.tot=sum(trips)),by=c('urb',freeCol)]
	to.plot <- join.on(join.on(to.plot,fleet.tot,c('urb',freeCol),c('urb',freeCol)),trips.tot,c('urb',freeCol),c('urb',freeCol))
	to.plot[group=='Chargers',value.per:=ifelse(fleet.tot==0,0,value/fleet.tot)]
	to.plot[group=='Fleet',value.per:=ifelse(trips.tot==0,0,value/trips.tot)]
	p <- ggplot(to.plot,aes(x=urb,y=value.per,fill=variable))+
	  geom_bar(stat='identity')+
	  xlab('Regional Type')+
	  ylab('Value (Chargers per Vehicle or Vehicles per Trip)')+
	  facet_grid(group~streval(freeCol),scales='free_y')+ 
	  theme_bw()+
	  scale_fill_manual(name='Charger/Battery Level',values = getPalette(to.plot$variable),guide=guide_legend(reverse=F))
	ggsave(pp(sub.dir,'/fleet_and_chargers_',code,'.pdf'),p,width=8,height=4,units='in')
	write.csv(to.plot,pp(sub.dir,'/fleet_and_chargers_',code,'.csv'))
}

make.1d.charging.plot <- function(ch.sub,code,freeCol,sub.dir,factor.labels,the.ch.cols) {
	day.axis.breaks <- seq(0,72,by=12)
	p <- ggplot(ch.sub[t>min(day.axis.breaks)&t<=max(day.axis.breaks)],aes(x=t,y=gwh,fill=charger.level))+geom_area(stat='identity')+facet_grid(.~streval(freeCol))+scale_x_continuous(breaks=day.axis.breaks)+scale_fill_manual(values = the.ch.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x='Hour',y='Charging Power (GW)',title=pp('Charging Power by ',factor.labels[freeCol],ifelse(code=='','',' when '),code),fill='')+theme_bw()
	pdf.scale <- 1
	ggsave(pp(sub.dir,'/charging_',code,'.pdf'),p,width=8*pdf.scale,height=4*pdf.scale,units='in')
	write.csv(ch.sub[t>min(day.axis.breaks)&t<=max(day.axis.breaks)],file=pp(sub.dir,'/charging_',code,'.csv'))
	p <- ggplot(ch.sub[t>min(day.axis.breaks)&t<=max(day.axis.breaks)],aes(x=t,y=gwh,fill=charger.level))+geom_area(stat='identity')+facet_grid(streval(freeCol)~.)+scale_x_continuous(breaks=day.axis.breaks)+scale_fill_manual(values = the.ch.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x='Hour',y='Charging Power (GW)',title=pp('Charging Power by ',factor.labels[freeCol],ifelse(code=='','',' when '),code),fill='')+theme_bw()
	ggsave(pp(sub.dir,'/charging_',code,'_transpose.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
}

make.2d.charging.plot <- function(ch.sub,code,freeCols,the.ch.cols){
	day.axis.breaks <- seq(0,72,by=12)
	p <- ggplot(ch.sub,aes(x=t-24,y=gwh,fill=charger.level))+geom_area(stat='identity')+scale_x_continuous(breaks=day.axis.breaks)+scale_fill_manual(values = the.ch.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x='Hour',y='Charging Power (GW)',title=pp('Charging Power',ifelse(code=='','',' when '),code),fill='')+theme_bw()
	p <- p + streval(pp('facet_grid(',freeCols[1],'~',freeCols[2],')'))
	pdf.scale <- 1
	ggsave(pp(plots.dir,'_charging_2d',ifelse(code=='','',pp('_',code)),'.pdf'),p,width=14*pdf.scale,height=8*pdf.scale,units='in')
	write.csv(ch.sub,file=pp(plots.dir,'_charging_2d',ifelse(code=='','',pp('_',code)),'.csv'))
}

make.2d.metric.plot <- function(all.sub,code,freeCols,the.cols){
	all.sub <- join.on(all.sub,all.sub[,.(val=sum(value,na.rm=T)),by=c('run','metric')][,.(max.value=max(val,na.rm=T)),by=c('metric')],'metric','metric')
	all.sub[,scaled:=value/max.value]
	p <- ggplot(all.sub,aes(x=metric,y=scaled*100,fill=variable))+geom_bar(stat='identity')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x="",y="% of Reference Scenario",fill='')+theme_bw()
	p <- p + streval(pp('facet_grid(',freeCols[1],'~',freeCols[2],')'))
	pdf.scale <- 1
	ggsave(pp(plots.dir,'_metrics_2d',ifelse(code=='','',pp('_',code)),'.pdf'),p,width=14*pdf.scale,height=8*pdf.scale,units='in')
	write.csv(all.sub,file=pp(plots.dir,'/metric_2d',ifelse(code=='','',pp('_',code)),'.csv'))
}

make.1d.metric.plot <- function(all.sub,code,freeCol,sub.dir,the.cols,factor.labels,metric.units){
	the.levels <- streval(pp('exper$runs[,.(col=',freeCol,')]$col'))
	streval(pp('all.sub[,',freeCol,':=factor(',freeCol,',levels=u(the.levels))]'))
	make.dir(sub.dir)
	for(the.metric in u(all.sub$metric)){
		p <- ggplot(all.sub[metric==the.metric],aes(x=streval(freeCol),y=value/metric.units[metric==the.metric]$scale.factor,fill=variable))+geom_bar(stat='identity',colour='black')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x=factor.labels[freeCol],y=metric.units[metric==the.metric]$label,title=pp(the.metric,ifelse(code=='','',' when '),code),fill='')+theme_bw()
		pdf.scale <- 1
		ggsave(pp(sub.dir,'/metric_',the.metric,'_',code,'.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
		write.csv(streval(pp('all.sub[metric==the.metric,.(x=',freeCol,',y=value/metric.units[metric==the.metric]$scale.factor,fill=variable)]')),file=pp(sub.dir,'/metric_',the.metric,'_',code,'.csv'))
		if(the.metric=='Emissions'){
		  p <- ggplot(all.sub[metric==the.metric],aes(x=streval(freeCol),y=value/metric.units[metric==the.metric]$scale.factor,fill=variable))+geom_bar(stat='identity',colour='black')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x=factor.labels[freeCol],y=metric.units[metric==the.metric]$label,title=pp(the.metric,ifelse(code=='','',' when '),code),fill='')+theme_bw()+scale_y_continuous(limits = c(0,11.5))
		  pdf.scale <- 1
		  ggsave(pp(sub.dir,'/metric_',the.metric,'_',code,'_scaledTo11.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
		}else if(the.metric=='Cost'){
		  p <- ggplot(all.sub[metric==the.metric & variable=='Cost: Energy'],aes(x=streval(freeCol),y=value/metric.units[metric==the.metric]$scale.factor,fill=variable))+geom_bar(stat='identity',colour='black')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x=factor.labels[freeCol],y=metric.units[metric==the.metric]$label,title=pp(the.metric,ifelse(code=='','',' when '),code),fill='')+theme_bw()
		  pdf.scale <- 1
		  ggsave(pp(sub.dir,'/metric_',the.metric,'_',code,'_energyInset.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
		}else if(the.metric=='Person_Trips_per_SAEV'){
		  p <- ggplot(all.sub[metric==the.metric],aes(x=streval(freeCol),y=value/metric.units[metric==the.metric]$scale.factor,fill=variable))+geom_bar(stat='identity',colour='black',position='dodge')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x=factor.labels[freeCol],y=metric.units[metric==the.metric]$label,title=pp(the.metric,ifelse(code=='','',' when '),code),fill='')+theme_bw()
		  pdf.scale <- 1
		  ggsave(pp(sub.dir,'/metric_',the.metric,'_',code,'_dodged.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
		}
	}
}

make.1d.plots <- function(plots.dir,all,res,by.r,the.cols,factor.labels,metric.units,to.plot.ch.agg,the.ch.cols) {
	run.params <- copy(exper$runs)[,run:=1:.N]
	param.names <- exper$param.names

	make.dir(pp(plots.dir,'/_metrics_1d'))
	if(length(param.names)==1){
		make.1d.metric.plot(all,'',param.names[1],pp(plots.dir,'/_metrics_1d'),the.cols,factor.labels,metric.units)
	} else{
		param.inds <- data.table(t(combn(length(param.names),length(param.names)-1)))
		for(param.ind.i in 1:nrow(param.inds)) {
			the.param.inds <- unlist(param.inds[param.ind.i])
			param.vals <- sapply(the.param.inds,function(i){ exper$yaml$Factors[[i]]$Levels })
			param.combs <- data.table(expand.grid(param.vals))
			names(param.combs) <- param.names[the.param.inds]
			the.free.col <- param.names[(1:length(param.names))[!(1:length(param.names)%in%the.param.inds)]]
			for(comb.i in 1:nrow(param.combs)) {
				code <- pp(param.names[the.param.inds],unlist(param.combs[comb.i]),collapse='_')
				my.cat(pp('Fixing levels to: ',code))
				all.sub <- streval(pp('all[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
				make.1d.metric.plot(all.sub,code,the.free.col,pp(plots.dir,'/_metrics_1d/',code),the.cols,factor.labels,metric.units)
				ch.sub <- streval(pp('to.plot.ch.agg[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
				make.1d.charging.plot(ch.sub,code,the.free.col,pp(plots.dir,'/_metrics_1d/',code),factor.labels,the.ch.cols)
				by.r.sub <- streval(pp('by.r[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
				by.r.sub <- join.on(by.r.sub,res[['d-rmob-t']][,.(totalSAEVTrips=sum(demand,na.rm=T)),by=c('run','rmob')],c('run','rmob'),c('run','rmob'))
				energy.by.r.l <- res[['b-l-rmob-t']][run%in%u(by.r.sub$run),.(energyCharged=sum(energyCharged)),by=c('run','l','rmob')]
				if(sum(by.r.sub$value)>0&param.names[the.param.inds]%in%names(by.r.sub)) {
					make.1d.fleet.and.chargers.plot(by.r.sub,code,the.free.col,pp(plots.dir,'/_metrics_1d/',code),energy.by.r.l)
				}
			}
		}
	}
}

make.2d.plots <- function(all,to.plot.ch.agg,the.cols,the.ch.cols) {
	run.params <- copy(exper$runs)[,run:=1:.N]
	param.names <- exper$param.names

	if(length(param.names)==2) {
		make.2d.metric.plot(all,'',param.names,the.cols)
		make.2d.charging.plot(to.plot.ch.agg,'',param.names,the.ch.cols)
	} else if(length(param.names)>2) {
		param.inds <- data.table(t(combn(length(param.names),length(param.names)-2)))
		for(param.ind.i in 1:nrow(param.inds)) {
			the.param.inds <- unlist(param.inds[param.ind.i])
			param.vals <- sapply(the.param.inds,function(i){ exper$yaml$Factors[[i]]$Levels })
			param.combs <- data.table(expand.grid(param.vals))
			names(param.combs) <- param.names[the.param.inds]
			the.free.cols <- param.names[(1:length(param.names))[!(1:length(param.names)%in%the.param.inds)]]
			for(comb.i in 1:nrow(param.combs)){
				code <- pp(param.names[the.param.inds],unlist(param.combs[comb.i]),collapse='_')
				my.cat(pp('Fixing levels to: ',code))
				all.sub <- streval(pp('all[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
				make.2d.metric.plot(all.sub,code,the.free.cols,the.cols)
				ch.sub <- streval(pp('to.plot.ch.agg[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
				make.2d.charging.plot(ch.sub,code,the.free.cols)
			}
		}
	}
}

