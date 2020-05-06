#CODE FITS REGRESSION MODEL TO ESTIMATE THE RELATIONSHIP BETWEEN AVERAGE FORECASTED BIOMASS CHANGE IN EACH FAO AREA AND AVERAGE FISHERIES LANDINGS IN EACH FAO AREA UNDER RCP 2.6 AND 8.5

######################################################
#INSTALL AND LOAD LIBRARIES
install.packages(c('plyr','robust'))
library(plyr)
library(nlme)

load('fdat_reprod.RData')

#IDENTIFY RELATIONSHIPS THAT REQUIRE SPATIAL METHODS
fdatr$spatm<-ifelse(fdatr$cat=='Stressor','y','n')
fdatr$spatm<-ifelse(fdatr$rcp=='8.5' & fdatr$lbl=='Human impact','y',fdatr$spatm)


RFUN<-function(d){
    d<-unique(d)
    d$se<-ifelse(d$se==0,min(d$se)/2,d$se)
    d$w<-1/d$se

#REGRESSIONS FOR RELATIONSHIPS FOR WHICH RESIDUAL VARIATION IS SPATIALLY DEPENDENT - BASED ON A PRIORI MORAN TESTS AND SEMI-VARIOGRAMS
    if(unique(d$spatm)=='y'){
        mod<-gls(wm~xt,correlation=corExp(form=~ lon + lat,nugget=TRUE),data=d,method='REML',weights=~w)
    } else {
        mod<-gls(wm~xt,data=d,method='REML',weights=~w)
    }
    s<-summary(mod)

return(data.frame(b=round(s$tTable[2,1],digits=4),
                 bpv=round(s$tTable[2,4],digits=4),
                 bse=round(s$tTable[2,2],digits=4),
                 n=dim(d)[1]))
}
meffs<-ddply(subset(fdatr,n>1 & is.na(x)==FALSE & is.na(wm)==FALSE),.(rcp,cat,lbl),.fun=RFUN,.progress='text')


#EXPECTED OUTPUT
  rcp cat               lbl       b    bpv    bse  n
1 2.6 SES Human development -1.2158 0.0767 0.6794 98
2 8.5 SES Human development  7.8079 0.0000 1.4430 99




