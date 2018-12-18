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
  my.cat(pp('Creating static inputs'))

  ##### STATIC SETS #####

  regions <- c('ENC','ESC','MAT-NL','MAT-NY','MTN','NENG','PAC-CA','PAC-NL','SAT-FL','SAT-NL','WNC','WSC-TX','WSC-NL')
  g <- generators$g
  gtor <- generators[,list(g,r)]
  hydro <- generators$g[generators$FuelType=='Hydro']
  solar <- generators$g[generators$FuelType=='Solar']
  wind <- generators$g[generators$FuelType=='Wind']

  inputs.sets <- list(t=pp('t',seq(1,length(days)*24)),
                 rmob=as.vector(sapply(regions,function(x){ pp(x,c('-RUR','-URB'))})),
                 r=regions,g=g,gtor=gtor)

  ##### STATIC PARAMETERS #####

  demandLoad <- load[,list(r,t,demandLoad)]
  names(demandLoad) <- c('r','t','value')
  transCap <- transmission[,list(r1,r2,transCap)]
  names(transCap) <- c('r','o','value')
  transCost <- transmission[,list(r1,r2,transCost)]
  names(transCost) <- c('r','o','value')

  inputs.parameters <- list(demandLoad=demandLoad,transCap=transCap,transCost=transCost)

  inputs <- list(sets=inputs.sets,parameters=inputs.parameters)

  inputs
}
