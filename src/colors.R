  
  getPalette = function(vals){
    ncol <- length(u(vals))
    if(any(u(vals)=='b075',na.rm=TRUE)|any(u(vals)=='75 kWh',na.rm=TRUE)){
      nprivate <- sum(substr(u(vals),1,6)=='Privat')
      nsaevs <- ncol - nprivate
      c(colorRampPalette(brewer.pal(nsaevs, "Reds"))(nsaevs),
        colorRampPalette(brewer.pal(nprivate, "Blues"))(nprivate))
    }else if(any(u(vals)=='L010',na.rm=TRUE)|any(u(vals)=='10 kW',na.rm=TRUE)){
      nsaevs <- sum(substr(u(vals),1,1)=='L')
      nprivate <- ncol-nsaevs
      c(colorRampPalette(brewer.pal(nsaevs, "Purples"))(nsaevs),
        colorRampPalette(brewer.pal(nprivate, "Greens"))(nprivate))
    }else if(any(vals%in%c("Solar","Wind","Geothermal","Other","Hydro","NaturalGas","Pumps","Biomass","Coal","Nuclear"),na.rm=T)){
      #cs <- colorRampPalette(brewer.pal(10, "Set3"))(10)
      #names(cs) <- c("Solar","Wind","Geothermal","Other","Hydro","NaturalGas","Pumps","Biomass","Coal","Nuclear")
      #cs[vals]
      colorRampPalette(brewer.pal(ncol, "Greys"))(ncol)
    }else{
      colorRampPalette(brewer.pal(ncol, "Oranges"))(ncol)
    }
  }