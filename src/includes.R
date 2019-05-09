
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
library('RColorBrewer')
library('forcats')
library('gtools')

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

roundC <- function(x,dg=1){ formatC(x,format='f',digits=dg) }

merge.baseGen <- function(result,result.baseGen) {
  result$`g-t` <- merge(x=result$`g-t`,y=result.baseGen$`g-t`,by=c('g','t'))
  names(result$`g-t`) <- c('g','t','generation','base.generation')
  result$`o-r-t` <- merge(x=result$`o-r-t`,y=result.baseGen$`o-r-t`,by=c('r','t','o'))
  names(result$`o-r-t`) <- c('r','t','o','trans','base.trans')
  return(result)
}

to.number <- function(x){ as.numeric(substr(as.character(x),2,nchar(as.character(x)))) }

print.lst.status <- function(file) {
  lst.file <- readLines(file)
  print(grep('SOLVER STATUS',lst.file,value=TRUE))
  print(grep('MODEL STATUS',lst.file,value=TRUE))
}