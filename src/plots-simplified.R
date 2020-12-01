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

	# Plotting all single run plots
	for(run.i in u(vehs$run)) {
		plot.grid.all(run.i,plots.dir,res,generators.prepped$geners)
		plot.lightduty.all(run.i,plots.dir,lightduty.prepped$vehs,exper,all.inputs,lightduty.prepped$veh.ch,lightduty.prepped$personal.ev.ch,lightduty.prepped$en,lightduty.prepped$by.r)
		plot.heavyduty.all(run.i,plots.dir,heavyduty.prepped$vehs,exper,all.inputs,heavyduty.prepped$veh.ch,heavyduty.prepped$en,heavyduty.prepped$by.r)
	}
}

prepData.all.lightduty <- function(exper,all.inputs,res,plots.dir) {
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

	output <- list('veh.mv'=veh.mv,'d.dot'=d.dot,'l.dot'=l.dot,'veh.ch'=veh.ch,'vehs'=vehs,'veh.ch'=veh.ch,'personal.ev.ch'=personal.ev.ch,'en'=en,'batt'=batt,'by.r'=by.r)
	return(output)
}

prepData.all.heavyduty <- function(exper,all.inputs,res,plots.dir) {
	# Vehicle Distribution
	veh.mv <- data.table(cast(melt(res[['rmob-t-tb-td']],id.vars=c('t','tb','td','rmob','run'),measure.vars=c('truckvehiclesMoving')),tb + rmob + t + run ~ td))
	td.dot <- str_replace(inputs$sets$td,"-",".")
	streval(pp('veh.mv[,":="(',pp(pp('truckvehiclesMoving.',td.dot,'=`',inputs$sets$td,'`,`',inputs$sets$td,'`=NULL'),collapse=','),')]'))
	tl.dot <- str_replace(inputs$sets$tl,"-",".")
	veh.ch <- data.table(cast(melt(res[['rmob-t-tb-tl']],id.vars=c('t','tb','tl','rmob','run'),measure.vars=c('truckvehiclesCharging')),tb + rmob + t + run ~ tl))
	streval(pp('veh.ch[,":="(',pp(pp('truckvehiclesCharging.',tl.dot,'=`',inputs$sets$tl,'`,`',inputs$sets$tl,'`=NULL'),collapse=','),')]'))
	vehs <- join.on(res[['rmob-t-tb']],join.on(veh.mv,veh.ch,c('tb','rmob','t','run')),c('tb','rmob','t','run'))
	veh.ch <- data.table(melt(res[['rmob-t-tb-tl']],id.vars=c('t','tb','tl','rmob','run'),measure.vars=c('truckvehiclesCharging')))
	veh.ch[,kw:=unlist(lapply(str_split(tl,'tL'),function(ll){ as.numeric(ll[2])}))]
	veh.ch[,gw.charging:=kw*value/1e6]
	setkey(veh.ch,run,tl,rmob,t)
	veh.ch[,charger.level:=gsub('tL|tL0|tL00','',tl)]
	veh.ch[,charger.level:=paste(charger.level,'kW')]
	veh.ch$charger.level <- factor(veh.ch$charger.level,levels=unique(veh.ch$charger.level)[mixedorder(unique(veh.ch$charger.level))])

	# Energy balance
	en <- join.on(join.on(res[['rmob-t-tb-tl']],res[['rmob-tb-tl']],c('tl','rmob','tb','run'),c('tl','rmob','tb','run'))[,.(en.ch=sum(truckenergyCharged)),by=c('t','rmob','tb','run')],res[['rmob-t-tb-td']][,.(en.mob=sum(truckenergyConsumed)),by=c('t','rmob','tb','run')],c('tb','rmob','t','run'),c('tb','rmob','t','run'))
	batt <- join.on(res[['rmob-tb']],res[['tb']],c('tb','run'),c('tb','run'))
	batt[,soc:=truckfleetSize*truckbatteryCapacity]
	en <- join.on(en,batt,c('tb','rmob','run'),c('tb','rmob','run'),'soc')
	#en[t>0,soc:=0]
	setkey(en,tb,rmob,t)
	en[,soc:=soc+cumsum(en.ch-en.mob),by=c('tb','rmob','run')]
	en[,battery.level:=gsub('tb|tb0|tb00','',tb)]
	en[,battery.level:=paste(battery.level,'mi')]
	en$battery.level <- factor(en$battery.level,levels=unique(en$battery.level)[mixedorder(unique(en$battery.level))])

	# Fleet size and num chargers
	by.r <- rbindlist(list(res[['rmob-tl']][,':='(run=run,variable=tl,value=trucknumChargers,group='Chargers')],res[['rmob-tb']][,':='(run=run,variable=tb,value=truckfleetSize,group='Fleet')]),fill=T)
	setkey(by.r,run,variable)
	by.r[,var.clean:=ifelse(grepl('tL',variable),paste('Chgr:',variable,'kW'),paste('Bat:',variable,'mi'))]
	by.r[,var.clean:=gsub(' tL| tL0| tL00|tb00| tb| tb0',' ',var.clean)]
	by.r$var.clean <- factor(by.r$var.clean,levels=unique(by.r$var.clean)[mixedorder(unique(by.r$var.clean))])

	output <- list('veh.mv'=veh.mv,'td.dot'=td.dot,'l.dot'=l.dot,'veh.ch'=veh.ch,'vehs'=vehs,'veh.ch'=veh.ch,'en'=en,'batt'=batt,'by.r'=by.r)
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
	df[l!='Private EVs',charger.level:=pp('SAEV: ',charger.level)]
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

plot.lightduty.all <- function(run.i,plots.dir,vehs,exper,all.inputs,veh.ch,personal.ev.ch,en,by.r) {
	plot.lightduty.numberOfVehicles(run.i,plots.dir,vehs)
	plot.lightduty.charging(run.i,plots.dir,exper,all.inputs,veh.ch,personal.ev.ch)
	plot.lightduty.personalVehs(run.i,plots.dir,all.inputs)
	plot.lightduty.energyBalance(run.i,plots.dir,en)
	plot.lightduty.fleetDetails(run.i,plots.dir,by.r)
}

plot.lightduty.numberOfVehicles <- function(run.i,plots.dir,vehs) {
	day.axis.breaks <- seq(0,max(veh.ch$t),by=24)

	to.plot <- melt(vehs[run==run.i],id.vars=c('b','t','rmob','run'))
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

plot.lightduty.charging <- function(run.i,plots.dir,exper,all.inputs,veh.ch,personal.ev.ch) {
	day.axis.breaks <- seq(0,max(veh.ch$t),by=24)

	p <- ggplot(veh.ch[run==run.i],aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
		geom_area()+
		facet_wrap(~rmob,scales='free_y')+
		scale_fill_manual(values = getPalette(veh.ch$charger.level),guide=guide_legend(reverse=F))+
		labs(x='Hour',y='Load (GW)',fill='Charger Level')+
		theme_bw()
	pdf.scale <- 1
	ggsave(pp(plots.dir,'/run-',run.i,'/_ld_charging-saevs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
	
	to.plot <- rbindlist(list(veh.ch,personal.ev.ch),fill=T,use.names=T)[run==run.i]
	to.plot[,gwh:=gw.charging]
	to.plot[,ll:='']
	if(exper$runs[run.i]$fractionSAEVs<1.0)to.plot <- disag.the.private.load(to.plot,all.inputs[[run.i]]$parameters$personalEVUnmanagedLoads)
	to.plot[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
	to.plot[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
	the.ch.cols <- to.plot$col
	names(the.ch.cols) <- to.plot$charger.level
	
	p <- ggplot(to.plot,aes(x=t,y=gwh,fill=charger.level))+
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

plot.lightduty.personalVehs <- function(run.i,plots.dir,all.inputs) {
	day.axis.breaks <- seq(0,max(veh.ch$t),by=24)

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
	personal.evs.total <- melt(personal.evs.total,id='t',variable.name='Activity',value.name='Cars')
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


plot.heavyduty.all <- function(run.i,plots.dir,vehs,exper,all.inputs,veh.ch,en,by.r) {
	plot.heavyduty.numberOfVehicles(run.i,plots.dir,vehs)
	plot.heavyduty.charging(run.i,plots.dir,exper,all.inputs,veh.ch,personal.ev.ch)
	plot.heavyduty.energyBalance(run.i,plots.dir,en)
	plot.heavyduty.fleetDetails(run.i,plots.dir,by.r)
}

plot.heavyduty.numberOfVehicles <- function(run.i,plots.dir,vehs) {
	day.axis.breaks <- seq(0,max(veh.ch$t),by=24)

	to.plot <- melt(vehs[run==run.i],id.vars=c('tb','t','rmob','run'))
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
	day.axis.breaks <- seq(0,max(veh.ch$t),by=24)

	p <- ggplot(veh.ch[run==run.i],aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
		geom_area()+
		facet_wrap(~rmob,scales='free_y')+
		scale_fill_manual(values = getPalette(veh.ch$charger.level),guide=guide_legend(reverse=F))+
		labs(x='Hour',y='Load (GW)',fill='Charger Level')+
		theme_bw()
	pdf.scale <- 1
	ggsave(pp(plots.dir,'/run-',run.i,'/_hd_charging-saevs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
}

plot.heavyduty.energyBalance <- function(run.i,plots.dir,en) {
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