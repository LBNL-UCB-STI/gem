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

  inputs <- list()

  #### Region ####
  region.mobility <- 
  if('region' %in% param.names){
    inputs$region <- exper.row$region
  }else{
    inputs$region <- region
  }

  regions <- c('PAC-CA-URB','PAC-CA-RUR','MTN-URB','MTN-RUR')
  regions <- 'ALL'

  dists <- pp('d',1:num.dist.bins)

  inputs
}

