# Author: Schatz Energy Research Center
# Original Version: Andy Harris
# Edits: Jerome Carman and/or Daug Saucedo and/or Andy Harris
# Version: 1
# Date: November 2, 2018
# Description: A function to generate a list of vmt weights to build a fleet with a given distribution of vmts.
#
# Required Variables
# mean_vmt: The average daily vehicle miles travelled
# bin_width: The bin size (in miles) on which we want to build a distribution
#
# Optional Variables
# sd_vmt: the standard deviation of daily vehicle miles travelled. Currently set to a default
#
# Version History
# 1: Initial development
#
vmt_WeightDistGen <- function(mean_vmt,bin_width,sd_vmt=20) {
  # Based on Barter et al 2015, Lin et al 2012, and Tamor et al 2015, we believe the gamma distribution to be a reasonable
  # approximation of the distribution of daily vmt. 
  
  # The gamma distribution is defined by 2 variables: shape (k) and scale (theta). The distribution mean is equal to 
  # shape * scale, and the variance is equal to shape * (scale)^2. Step one is to get these parameter values from the 
  # mean and standard deviation.
  
  scale.var <- (sd_vmt^2)/mean_vmt
  shape.var <- mean_vmt/scale.var
  
  # Create a data table loaded with the vmt bins we want
  vmt_weights <- data.table(name=seq(bin_width,500,bin_width))
  
  # To assign weights, calculate the value of the CDF at each VMT bin. We wiill then subtract the CDF value from the 
  # previous bin to get the weight percentage for that bin.
  vmt_weights[,cdf:=pgamma(name,shape.var,rate=1/scale.var)]
  vmt_weights[,prev.cdf:=c(0,cdf[.I-1])]
  vmt_weights[,weight:=cdf-prev.cdf]
  
  # Now trim the extraneous columns
  vmt_weights[,':='(cdf=NULL,prev.cdf=NULL)]
  return(vmt_weights)
}