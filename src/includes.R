# Loading all required packages for GEM

library('stringr')
library('data.table')
library('ggplot2')
library('optparse')
library('yaml')
library('reshape')
library('grid')
library('gdxtools')
library('lubridate')
library('RColorBrewer')
library('forcats')
library('gtools')
library('sf')
library('cowplot')
library('tidyr')
library('dplyr')
library('maps')

# gdx.to.data.tables function parses the .gdx output file from the GAMS optimization and prepares
# them as data.table outputs in R.
gdx.to.data.tables <- function(mygdx){
  keys <- all_items(mygdx)$set
  keydims <- c()
  for(key in keys){
    keydims <- c(keydims,length(mygdx[key]$V1))
  }
  
  res <- list()
  for(var in c( all_items(mygdx)$parameters,all_items(mygdx)$variables)){
    if(all(dim(mygdx[var])==1)){
      if(!'scalar' %in% names(res)){
        res[['scalar']] <- data.table(mygdx[var])
      }else{
        res[['scalar']] <- cbind(res[['scalar']],data.table(mygdx[var]))
      }
      streval(pp("res[['scalar']][,':='(",var,"=value,value=NULL)]"))
    }else{
      the.keys <- head(names(mygdx[var]),-1)
      the.keys.key <- pp(sort(the.keys),collapse='-')
      if(!the.keys.key %in% names(res)){
        res[[the.keys.key]] <- data.table(mygdx[var])
      }else{
        if(nrow(res[[the.keys.key]])>0){
          res[[the.keys.key]] <- join.on(res[[the.keys.key]],data.table(mygdx[var]),the.keys,the.keys,'value')
        }else{
          res[[the.keys.key]] <- data.table(mygdx[var])
        }
      }
      streval(pp("res[[the.keys.key]][,':='(",var,"=value,value=NULL)]"))
    }
  }
  for(key in names(res)){
    if('t'%in%names(res[[key]])){
      res[[key]][,t:=as.numeric(substr(t,2,nchar(as.character(t))))]
    }
  }
  res
}

# date.info function parses a specific date/time format
date.info <- function(days,year){
  wdays <- weekdays(to.posix(pp(year,'-01-01 00:00:00+00'))+24*3600*(days-1))
  months <- month(to.posix(pp(year,'-01-01 00:00:00+00'))+24*3600*(days-1))
  day.types <- as.vector(sapply(wdays,function(the.wday){
    if(the.wday=='Sunday' || the.wday=='Saturday'){
      "SA/SU"
    }else if(the.wday=='Monday' || the.wday=='Friday'){
      "MO/FR"
    }else{
      "TU/WE/TH"
    }
  }))
  seasons <- sapply(months,function(the.month){
    if(the.month <=2 || the.month==12){
      'dec-feb'
    }else if(the.month>=3 && the.month <=5){
      'mar-may'
    }else if(the.month>=6 && the.month <=8){
      'jun-aug'
    }else if(the.month>=9 && the.month <=11){
      'sep-nov'
    }
  })
  list(wdays=wdays,months=months,day.types=day.types,seasons=seasons)
}

make.dir <-
  function(dir){
    if(!file.exists(dir))dir.create(dir,showWarnings=FALSE)
  }

# pp function is equivalent to paste0 (abbreviation for expediency)
pp <- function(...,sep='',collapse=NULL){
  paste(...,sep=sep,collapse=collapse)
}

# streval function parses a character object to be evaluated as code
streval <- function(toeval){
	eval.parent(parse(text=toeval))
}

# to.posix function parses a date-time character into a POSIXct format: YYYY/MM/DD
to.posix <- function(x,fmt="%Y-%m-%d",tz=""){
  as.POSIXct(strptime(x,fmt,tz))
}

# u function is equivalent to unique (abbreviation for expediency)
u <- function(x,...){
  unique(x,...)
}

# join.on is a merge function that allows for merging of data.tables without affecting the original table
# The function optimizes the join on keys that can be set as arguments into the function
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

# roundC function takes a numeric input and returns a character output that "prettifies" a number to 
# reduce the number of digits displayed
roundC <- function(x,dg=1){ formatC(x,format='f',digits=dg) }

# merge.baseGen is a function that merges two input results (from a given scenario run)
# for the grid parameters/variables.  The two runs consist of the experiment results and
# the results from the grid operation in the absence of any mobility demand in order to
# determine the consequential results.
merge.baseGen <- function(result,result.baseGen) {
  result$`g-t` <- merge(x=result$`g-t`,y=result.baseGen$`g-t`,by=c('g','t'))
  names(result$`g-t`) <- c('g','t','generation','base.generation')
  result$`o-r-t` <- merge(x=result$`o-r-t`,y=result.baseGen$`o-r-t`,by=c('r','t','o'))
  names(result$`o-r-t`) <- c('r','t','o','trans','base.trans')
  return(result)
}

# to.number function converts a number from character format into a numberic format
to.number <- function(x){ as.numeric(substr(as.character(x),2,nchar(as.character(x)))) }

# print.lst.status function is a diagnostic tool that returns a specific output from the
# GAMS output known as the listing (.lst) file.  The function will provide a solver and model
# status that describes whether or not the results are normally completed and whether the
# results are globally/locally optimal.
print.lst.status <- function(file) {
  lst.file <- readLines(file)
  print(grep('SOLVER STATUS',lst.file,value=TRUE))
  print(grep('MODEL STATUS',lst.file,value=TRUE))
}

# my.cat function concatenates and prints messages (more efficient than print)
my.cat <- function (message, ...){ cat(paste(message, "\n", sep = ""), ...) }
