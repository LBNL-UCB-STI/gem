  
  getPalette = function(vals){
    ncol <- length(u(vals))
    if(any(u(vals)=='b075',na.rm=TRUE)|any(u(vals)=='75 kWh',na.rm=TRUE)){
      nprivate <- sum(substr(u(vals),1,6)=='Privat')
      nsaevs <- ncol - nprivate
      c(rev(colorRampPalette(brewer.pal(nsaevs, "Reds"))(nsaevs)),
        rev(colorRampPalette(brewer.pal(nprivate, "Blues"))(nprivate)))
    }else if(any(u(vals)=='L010',na.rm=TRUE)|any(u(vals)=='10 kW',na.rm=TRUE)){
      nsaevs <- sum(substr(u(vals),1,1)=='L')
      nprivate <- ncol-nsaevs
      c(rev(colorRampPalette(brewer.pal(nsaevs, "Purples"))(nsaevs)),
        rev(colorRampPalette(brewer.pal(nprivate, "Greens"))(nprivate)))
    }else if(any(vals%in%c("Solar","Wind","Geothermal","Other","Hydro","NaturalGas","Pumps","Biomass","Coal","Nuclear"),na.rm=T)){
      #cs <- rev(colorRampPalette(brewer.pal(10, "Set3"))(10)
      #names(cs) <- c("Solar","Wind","Geothermal","Other","Hydro","NaturalGas","Pumps","Biomass","Coal","Nuclear")
      #cs[vals]
      rev(colorRampPalette(brewer.pal(ncol, "Greys"))(ncol))
    }else if(any(substr(u(vals),1,4)=='Idle',na.rm=TRUE)){
      nidle <- sum(substr(u(vals),1,4)=='Idle')
      nmoving <- ncol-nidle-1
      c(rev(colorRampPalette(brewer.pal(nidle, "Reds"))(nidle)),
        rev(colorRampPalette(brewer.pal(nmoving, "Blues"))(nmoving)),
        '#666666'
        )
    }else{
      rev(colorRampPalette(brewer.pal(ncol, "Oranges"))(ncol))
    }
  }