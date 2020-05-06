#THIS SCRIPT IMPORTS CSV DATAFILES WRITTEN BY MATLAB, REFORMATS SO THAT THEY ARE IN CORRECT FORMAT FOR THE ANALYSES IN R
#BECAUSE THE FILES (N=6) ARE QUITE LARGE (~4GB) A SINGLE, SMALLER FILE IS PROVIDED HERE AS AN EXAMPLE

#INSTALL AND LOAD REQUIRED PACKAGES
install.packages(c('tidyr','plyr','data.table','stringr'))
library(tidyr)
library(plyr)
library(data.table)
library(stringr)

#########################################################
#IMPORTS FILES THAT WERE WRITTEN FROM MATLAB, FORMATS AND WRITES TO RDATA FILES
importfun<-function(rcps){
#POINT TO DIRECTORY
print(getwd())
lfiles<-list.files()
lfiles<-lfiles[(grepl('csv',lfiles)==TRUE)]

#LOOP THROUGH ALL FILES IN DIRECTORY
l<-list()
for(i in 1:length(lfiles)){
print(lfiles[i])

system.time(d<-fread(paste(lfiles[i])))
names(d)[1:95]<-seq(2006,2100,1)

#ESM MODEL USED
d$oceanm<-ifelse(grepl('gfdl',d$file)==TRUE,'gfdl','ipsl')

#EMISSION SCENARIO
d$rcp<-ifelse(grepl('rcp2p6',d$file)==TRUE,'2.6',NA)
d$rcp<-ifelse(grepl('rcp4p5',d$file)==TRUE,'4.5',d$rcp)
d$rcp<-ifelse(grepl('rcp6p0',d$file)==TRUE,'6.0',d$rcp)
d$rcp<-ifelse(grepl('rcp8p5',d$file)==TRUE,'8.5',d$rcp)
d<-subset(d,rcp==rcps)
d$rcp<-as.numeric(d$rcp)

#VARIABLE
d$var<-ifelse(grepl('b10cm',d$file)==TRUE,'b10cm',NA)
d$var<-ifelse(grepl('b30cm',d$file)==TRUE,'b30cm',d$var)
d$var<-ifelse(grepl('tcb',d$file)==TRUE,'tcb',d$var)
d$var<-ifelse(grepl('blarge',d$file)==TRUE,'blarge',d$var)
d$var<-ifelse(grepl('bmed',d$file)==TRUE,'bmed',d$var)
d$var<-ifelse(grepl('bsmall',d$file)==TRUE,'bsmall',d$var)

#MEM MODEL
d$model<-str_extract(d$file, "[^_]+")

#FISHING
d$fish<-ifelse(grepl('no-fishing',d$file)==TRUE,'F','T')
d<-subset(d,fish=='F')

#IF SIZE CLASSES NOT INCLUDED, THEN CALCULATE
if(lfiles[i] %in% c('ecoocean.csv','apecosm.csv','macroecological.csv','dbpm.csv')){

#SEPARATE INTO SIZE CLASSES
a1<-(subset(d,var=='b10cm'))
a2<-(subset(d,var=='b30cm'))
a3<-(subset(d,var=='tcb'))

#ENSURE ALL FILE SIZES ARE IDENTICAL
a1$id<-gsub(' ','',paste(a1$oceanm,'_',a1$rcp,'_',a1$lon,'_',a1$lat))
a2$id<-gsub(' ','',paste(a2$oceanm,'_',a2$rcp,'_',a2$lon,'_',a2$lat))
a3$id<-gsub(' ','',paste(a3$oceanm,'_',a3$rcp,'_',a3$lon,'_',a3$lat))
dd<-join(a1,a2,by=c('id'),type='inner')
dd<-join(dd,a3,by=c('id'),type='inner')
dd<-unique(subset(dd,select=c('id')))
a1<-subset(a1,id %in% dd$id)
a2<-subset(a2,id %in% dd$id)
a3<-subset(a3,id %in% dd$id)

#ORDER TO ENSURE PROPER CALCULATIONS, BELOW
a1<-a1[order(a1$oceanm,a1$lon,a1$lat),]
a2<-a2[order(a2$oceanm,a2$lon,a2$lat),]
a3<-a3[order(a3$oceanm,a3$lon,a3$lat),]
print(dim(a1))
print(dim(a2))
print(dim(a3))

#BSMALL
b1<-a1
b1$var<-'bsmall'
b1[,1:95]<-a3[,1:95]-a1[,1:95]
#BMED
b2<-a1
b2$var<-'bmed'
b2[,1:95]<-a1[,1:95]-a2[,1:95]
#BLARGE
b3<-a2
b3$var<-'blarge'

d<-data.frame(rbind(a3,b1,b2,b3))
} else {
print('not add sizes')
}
d<-data.frame(d)
d<-d[,!(names(d) %in% c('file','id','fish'))]
d<-subset(d,var %in% c('tcb','bsmall','bmed','blarge'))

#RANDOM EFFECT ID
d$re<-gsub(' ','',paste(d$model,'_',d$oceanm,'_',d$var,'_',d$rcp))

#CELL ID
d$cell<-gsub(' ','',paste(d$lon,'_',d$lat))
l[[i]]<-d
}
return(data.frame(do.call('rbind',l)))
}

fdat2.6<-importfun('2.6')
fdat8.5<-importfun('8.5')



#################################################################

#STANDARDIZE RESPONSE TO % OF 'BASELINE' (2006-2026) OR OF MEAN
STDFUN<-function(d){
d$ival<-rowMeans(d[,1:21],na.rm=TRUE)
d$mval<-rowMeans(d[,1:95],na.rm=TRUE)
d$sdval<-apply(d[,1:95],1,function(x) sd(x,na.rm=TRUE))
#REMOVE ROWS WHERE NO INITIAL VALUES
d<-subset(d,is.na(sdval)==FALSE & is.na(ival)==FALSE)

#CREATE ADDITIONAL DATA FRAMES TO HOLD NEW VARIABLES
a1<-d
a1$units<-'pctb'
a2<-d
a2$units<-'pctm'
d$units<-'rsp'

#LOOP THROUGH YEARS AND APPLY TRANSFORMATIONS
nms<-names(d)[1:95]
for(i in 1:length(nms)){
print(nms[i])
dd<-subset(d,select=c(paste(nms[i]),'ival','mval','sdval'))
a1[,i]<-(dd[,1]/dd$ival)*100#PERCENT OF BASELINE
a2[,i]<-(dd[,1]/dd$mval)*100#PERCENT OF MEAN
}

dout<-rbind(d,a1,a2)
dout<-dout[,!(names(dout) %in% c('ival','mval','sdval'))]
dout$id<-gsub(' ','',paste(dout$cell,'_',dout$var,'_',dout$units))
return(dout)
}
fdat2.6<-STDFUN(fdat2.6)
fdat8.5<-STDFUN(fdat8.5)

fdat2.6<-subset(fdat2.6,units!='pctm')
fdat8.5<-subset(fdat8.5,units!='pctm')
fdat<-rbind(fdat2.6,fdat8.5)
fdat$id<-gsub(' ','',paste(fdat$cell,'_',fdat$var,'_',fdat$units,'_',fdat$rcp,'_',fdat$fish))


##########################################################
#SINCE THIS REPRODUCIBLE EXAMPLE USES FORECASTS FROM A SINGLE MEM, THE BELOW CODE WILL NOT WORK

#SUBSET ONLY IDS THAT CONTAIN AT LEAST 3 FORECASTS
dum<-data.frame(id=sort(unique(fdat$id)),
        nre=tapply(fdat$re,fdat$id,function(x) length(unique(x))))
dum<-subset(dum,nre>=3)
fdat<-subset(fdat,id %in% dum$id)

#SMALLER SUBSET OF FULL DATA FOR REPRODUCIBLE EXAMPLE
cell<-sample(unique(fdat$cell),size=100,replace=FALSE)
fdat<-subset(fdat,cell %in% cell)

save(fdat,file='')


