#############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs required by the grid model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
#############################################################################################

prep.inputs.grid <- function(exper.row){
  param.names <- names(exper.row)

  my.cat(pp('Creating grid inputs'))

  inputs <- list()


  inputs
}

