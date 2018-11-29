#############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs common to both the mobility and grid models.
# This is mostly sets.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
#############################################################################################

prep.inputs.common <- function(exper.row){
  param.names <- names(exper.row)

  inputs <- list(dists=pp('d',1:num.dist.bins))

  inputs
}

