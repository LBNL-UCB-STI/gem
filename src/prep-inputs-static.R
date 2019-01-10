#############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare static inputs (those that will never vary in an 
# experiment).
#
# Argument: none
# Returns: list containing data tables used to run the model (as named data.tables)
#############################################################################################

prep.inputs.static <- function(){
  cat(pp('Creating static inputs\n'))

  ##### STATIC SETS #####

  regions <- c('ENC','ESC','MAT-NL','MAT-NY','MTN','NENG','PAC-CA','PAC-NL','SAT-FL','SAT-NL','WNC','WSC-TX','WSC-NL')
  rmob <- as.vector(sapply(regions,function(x){ pp(x,c('-RUR','-URB'))}))
  rmobtor <- data.table('r'=rep(regions,each=2),'rmob'=rmob)
  g <- generators$g
  gtor <- generators[,list(g,r)]
  hydro <- generators$g[generators$FuelType=='Hydro']
  solar <- generators$g[generators$FuelType=='Solar']
  wind <- generators$g[generators$FuelType=='Wind']

  inputs.sets <- list(t=pp('t',seq(1,length(days)*24)),rmob=rmob,r=regions,rmobtor=rmobtor,g=g,gtor=gtor,hydro=hydro,solar=solar,wind=wind)

  ##### STATIC PARAMETERS #####
  dates <- date.info(days,year)
  hours.to.simulate <- pp('t',(days-1)*24+1:24)

  load[,tt:=as.numeric(substr(t,2,nchar(as.character(t))))]
  setkey(load,r,tt)
  demandLoad <- load[load$t%in%hours.to.simulate,list(r,t,value=demandLoad)]
  demandLoad[,t:=NULL]
  demandLoad[,t:=inputs.sets$t,by='r']

  transCap <- transmission[,list(r1,r2,transCap)]
  names(transCap) <- c('r','o','value')
  transCost <- transmission[,list(r1,r2,transCost)]
  names(transCost) <- c('r','o','value')

  inputs.parameters <- list(demandLoad=demandLoad,transCap=transCap,transCost=transCost)

  inputs <- list(sets=inputs.sets,parameters=inputs.parameters)

  inputs
}
