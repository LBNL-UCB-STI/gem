###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This makes plots related to mobility.
#
# Argument: the results list containing all GAMS outputs and the plotting directory
###############################################################################################

plots.mobility <- function(exper,inputs,res,plots.dir){
  getPalette = function(vals){ 
    ncol <- length(u(vals))
    if(ncol>9){
      colorRampPalette(brewer.pal(9, "Set1"))(ncol)
    }else if(any(u(vals)=='b075')){
      head(colorRampPalette(brewer.pal(9, "Set1"))(10),5)
    }else if(any(u(vals)=='L010')){
      tail(colorRampPalette(brewer.pal(9, "Set1"))(10),5)
    }else{
      colorRampPalette(brewer.pal(ncol, "Set1"))(ncol)
    }
  }
  
  # Vehicle Distribution
  veh.mv <- data.table(cast(melt(res[['b-d-rmob-t']],id.vars=c('t','b','d','rmob','run'),measure.vars=c('vehiclesMoving')),b + rmob + t + run ~ d))
  d.dot <- str_replace_all(inputs$sets$d,"-",".")
  streval(pp('veh.mv[,":="(',pp(pp('vehiclesMoving.',d.dot,'=`',inputs$sets$d,'`,`',inputs$sets$d,'`=NULL'),collapse=','),')]'))
  l.dot <- str_replace_all(inputs$sets$l,"-",".")
  veh.ch <- data.table(cast(melt(res[['b-l-rmob-t']],id.vars=c('t','b','l','rmob','run'),measure.vars=c('vehiclesCharging')),b + rmob + t + run ~ l))
  streval(pp('veh.ch[,":="(',pp(pp('vehiclesCharging.',l.dot,'=`',inputs$sets$l,'`,`',inputs$sets$l,'`=NULL'),collapse=','),')]'))
  vehs <- join.on(res[['b-rmob-t']],join.on(veh.mv,veh.ch,c('b','rmob','t','run')),c('b','rmob','t','run'))
  veh.ch <- melt(res[['b-l-rmob-t']],id.vars=c('t','b','l','rmob','run'),measure.vars=c('vehiclesCharging'))
  veh.ch[,kw:=unlist(lapply(str_split(l,'L'),function(ll){ as.numeric(ll[2])}))]
  veh.ch[,gw.charging:=kw*value/1e6]
  setkey(veh.ch,run,l,rmob,t)
  
  # Energy balance
  en <- join.on(res[['b-l-rmob-t']][,.(en.ch=sum(energyCharged)),by=c('t','rmob','b','run')],res[['b-d-rmob-t']][,.(en.mob=sum(energyConsumed)),by=c('t','rmob','b','run')],c('b','rmob','t','run'),c('b','rmob','t','run'))
  batt <- join.on(res[['b-rmob']],res[['b']],c('b','run'),c('b','run'))
  batt[,soc:=fleetSize*batteryCapacity]
  en <- join.on(en,batt,c('b','rmob','run'),c('b','rmob','run'),'soc')
  #en[t>0,soc:=0]
  setkey(en,b,rmob,t)
  en[,soc:=soc+cumsum(en.ch-en.mob),by=c('b','rmob')]
  
  for(run.i in u(vehs$run)){
    setkey(vehs,run,b,rmob,t)
    toplot <- melt(vehs[run==run.i],id.vars=c('b','rmob','t'))
    p <- ggplot(toplot,aes(x=t,y=value,fill=variable))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+scale_fill_manual(values = getPalette(toplot$variable))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    toplot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
    toplot[,variable.simp:=factor(variable.simp,c('Idle','Moving','Charging'))]
    setkey(toplot,variable.simp)
    p <- ggplot(toplot,aes(x=t,y=value,fill=variable.simp))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+
      scale_fill_manual(values = getPalette(toplot$variable.simp),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
  
    # Charging load
    p <- ggplot(veh.ch[run==run.i],aes(x=t,y=gw.charging,fill=fct_rev(l)))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+scale_fill_manual(values = rev(getPalette(veh.ch$l)),guide=guide_legend(reverse=F))+labs(x='Hour',y='Load (GW)',fill='Charger Level')
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_charging.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Energy balance
    p <- ggplot(en[run==run.i],aes(x=t,y=soc,colour=fct_rev(b)))+geom_line()+facet_wrap(~rmob,scales='free_y')+scale_colour_manual(values = rev(getPalette(en$b)),guide=guide_legend(reverse=F)) 
    ggsave(pp(plots.dir,'/run-',run.i,'/_soc.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    p <- ggplot(en,aes(x=t,y=soc,fill=fct_rev(b)))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+scale_fill_manual(values = rev(getPalette(en$b)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
  }
  
  tr <- rbindlist(list(melt(res[['r-t']],id.vars=c('t','rmob')),melt(en[,.(en.mob=sum(en.mob),en.ch=sum(en.ch)),by=c('rmob','t')],id.vars=c('t','rmob'))))
  tr[,group:=ifelse(variable=='price','Price',ifelse(str_sub(variable,1,3)=='en.','Energy','Cost'))]
  tr <- tr[group!="Cost"]
  prices <- tr[variable=='price']
  
  p <- ggplot(tr,aes(x=t,y=value,colour=variable))+geom_line()+facet_grid(group~rmob,scales='free_y') # geom_bar(stat='identity',position='dodge')
  ggsave(pp(plots.dir,'_energy-vs-price.pdf'),p,width=12*pdf.scale,height=8*pdf.scale,units='in')
  
  by.r <- rbindlist(list(res[['l-r']][,':='(variable=l,value=numChargers,group='Chargers')],res[['b-r']][,':='(variable=b,value=fleetSize,group='Fleet')]))
  setkey(by.r,variable)
  p <- ggplot(by.r,aes(x=r,y=value,fill=variable))+geom_bar(stat='identity')+facet_wrap(~group,scales='free_y')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+scale_fill_manual(values = getPalette(by.r$variable),guide=guide_legend(reverse=T))
  ggsave(pp(plots.dir,'_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
  
  toplot <- by.r[,.(percent=value/sum(value)*100,variable=variable),by=c('rmob','group')]
  toplot[,urb:=ifelse(grepl('RUR$',r),'Rural','Urban')]
  geo.ordered <- cbind(c(sapply(c('-RUR','-URB'),function(x){ pp(c('PAC-NL','PAC-CA','MTN','WNC','WSC-NL','WSC-TX','ENC','ESC','NENG','MAT-NL','MAT-NY','SAT-NL','SAT-FL'),x) })))[,1]
  toplot[,r:=factor(r,geo.ordered)]
  #toplot[,group:=factor(group,c('Fleet','Chargers'))]
  p <- ggplot(toplot,aes(x=factor(r),y=percent,fill=variable))+geom_bar(stat='identity')+facet_grid(group~urb,scales='free_x', space ='free_x')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+scale_fill_manual(values = getPalette(toplot$variable),guide=guide_legend(reverse=T))
  ggsave(pp(plots.dir,'_fleet-size-and-type-percent.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
  
  toplot <- res[['rmob']][,.(r,urbanFormFactor)]
  toplot[,urb:=ifelse(grepl('RUR$',r),'Rural','Urban')]
  geo.ordered <- cbind(c(sapply(c('-RUR','-URB'),function(x){ pp(c('PAC-NL','PAC-CA','MTN','WNC','WSC-NL','WSC-TX','ENC','ESC','NENG','MAT-NL','MAT-NY','SAT-NL','SAT-FL'),x) })))[,1]
  toplot[,Region:=factor(r,geo.ordered)]
  p <- ggplot(toplot,aes(x=Region,y=urbanFormFactor))+geom_bar(stat='identity')+facet_wrap(~urb,scales='free_x')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+coord_cartesian(ylim=c(1,max(toplot$urbanFormFactor)*1.01))
  ggsave(pp(plots.dir,'_urban-form-factor-by-region.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
  
  costs <- join.on(res[['rmob']],res[['r-t']][,.(energyCost=sum(energyCost),demandChargeCost=sum(demandChargeCost),vehicleMaintCost=sum(vehicleMaintCost)),by='rmob'],'rmob','rmob',c('energyCost','demandChargeCost','vehicleMaintCost'))
  vmt <- join.on(res[['b-d-rmob-t']],res[['d']],'d','d','travelDistance')
  vmt <- join.on(vmt,res[['d-rmob-t']],c('d','t','rmob'),c('d','t','rmob'),'velocity')
  vmt[,vmt:=vehiclesMoving*travelDistance]
  vmt[,pmt:=demandAllocated*travelDistance]
  fleet <- res[['b-r']]
  
  
  
}

