

for(i in 1:length(all.inputs)){
  if(!is.null(all.inputs[[i]]$parameters$personalEVUnmanagedLoads)){
    tot <- sum(all.inputs[[i]]$parameters$personalEVUnmanagedLoads$value)
    my.cat(pp(i,': ',pp(all.inputs[[i]]$parameters$personalEVUnmanagedLoads[,.(frac=sum(value)/tot),by='l']$frac,collapse=',') ))
  }else{
    my.cat(pp(i,': empty'))
  }
}