
library('stringr')
library('data.table')
library('ggplot2')
library('optparse')
library('yaml')
library('reshape')
library('grid')
library('yaml')
library('gdxtools')
library('lubridate')

pp <- function(...,sep='',collapse=NULL){
  paste(...,sep=sep,collapse=collapse)
}

streval <- function(toeval){
	eval.parent(parse(text=toeval))
}

to.posix <- function(x,fmt="%Y-%m-%d",tz=""){
  as.POSIXct(strptime(x,fmt,tz))
}

u <- function(x,...){
  unique(x,...)
}

join.on <- function(dt1,dt2,keys1,keys2=NULL,include.from.dt2=NULL,included.prefix='',allow.cartesian=F){
  dt1 <- copy(dt1)
  dt2 <- copy(dt2)
  if(is.null(keys2))keys2<-keys1
  if(!all(keys1==keys2)){
    for(i in 1:length(keys1)){
      streval(pp('dt2[,',keys1[i],':=',keys2[i],']'))
    }
  }
  streval(pp('setkey(dt1,',pp(keys1,collapse=','),')'))
  streval(pp('setkey(dt2,',pp(keys1,collapse=','),')'))
  if(is.null(include.from.dt2)){
    cols.to.include <- names(dt2)[-which(names(dt2)%in%c(keys1,keys2))]
  }else{
    cols.to.include <- include.from.dt2
  }
  if(included.prefix!=''){
    for(col.to.include in cols.to.include){
      streval(pp('dt2[,',included.prefix,col.to.include,':=',col.to.include,']'))
    }
    cols.to.include <- pp(included.prefix,cols.to.include)
  }
  cols.to.include <- c(cols.to.include,keys1)
  res.dt <- streval(pp('dt2[,list(',pp(cols.to.include,collapse=','),')][dt1,allow.cartesian=',allow.cartesian,']'))
  return(res.dt)
}
