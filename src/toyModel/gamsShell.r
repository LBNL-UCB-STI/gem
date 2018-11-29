library(gdxrrw)
library(gdxtools)

igdx('/Applications/GAMS25.0/sysdir/')

g <- data.frame('g'=c('g1','g2','g3'))
gtor <- data.frame('g'=c('g1','g2','g3'),'r'=c('r1','r1','r2'))
genCost <- data.frame('g'=c('g1','g2','g3'),'value'=c(10,20,30))
demandLoad <- data.frame('t'=factor(rep(c('t1','t2','t3'),3)),'r'=factor(rep(c('r1','r2','r3'),each=3)),'value'=c(5,15,10,15,10,15,20,10,10))
maxGen <- data.frame('g'=c('g1','g2','g3'),'value'=c(100,100,100))

write.gdx('test.gdx',params=list(genCost=genCost,demandLoad=demandLoad,maxGen=maxGen),sets=list(g=g,gtor=gtor))
gams('dispatchModel.gms')

all_items(gdx('results.gdx'))

unique(gdx('test.gdx')['demandLoad'])
unique(gdx('test.gdx')['maxGen'])
