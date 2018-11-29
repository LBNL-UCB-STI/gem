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

  regions <- c('ENC','ESC','MAT-NL','MAT-NY','MTN','NENG','PAC-CA','PAC-NL','SAT-FL','SAT-NL','WNC','WSC-TX','WSC-NL')

  inputs <- list(t=pp('t',seq(1,length(days)*24)),
                 rmob=as.vector(sapply(regions,function(x){ pp(x,c('-RUR','-URB'))})),
                 r=regions)

  inputs
}
