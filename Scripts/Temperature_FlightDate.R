# Load packages
library(raster)
library(rgdal)
library(sp)

# Load flight time data
FlightTemps<-read.csv("Data/FlightTemps.csv")

# Read in shapefiles
shapes<-list.files("G:/Google_Drive/dragonflies/Spatial_Layers/Shapes",pattern="*.shp",full.names=TRUE)

Shapefiles <- lapply(shapes[2:13],shapefile)


States<-shapefile(shapes[15])
IN<-subset(States,subset=(NAME=="Indiana"))
NJ<-subset(States,subset=(NAME=="New Jersey"))
OH<-subset(States,subset=(NAME=="Ohio"))
MD<-SpatialPoints(cbind(-76.809,39.033))
WV<-subset(States,subset=(NAME=="West Virginia"))
CT<-subset(States,subset=(NAME=="Connecticut"))
NY<-subset(States,subset=(NAME=="New York"))
ME<-subset(States,subset=(NAME=="Maine"))
VT<-subset(States,subset=(NAME=="Vermont"))
OK<-subset(States,subset=(NAME=="Oklahoma"))

FlightLocations<-list(
Shapefiles[[5]],Shapefiles[[3]],IN,Shapefiles[[10]],NJ,
Shapefiles[[12]],OK,Shapefiles[[4]],MD,Shapefiles[[8]],
OH,WV,CT,NY,Shapefiles[[2]],ME,VT)

# Daily Mean Temperatures #
DailyTemp<-list.files("G:/DragonFlies/PRISM_1994_4km",pattern="*bil.bil",full.name=TRUE)

# read in raster data
MeanDaily <-lapply(DailyTemp,raster)

meanDaily<-stack(MeanDaily)

# Determine the amount of Degree days above 0c for each first flight time #
FFDays<-c(54,62,69,69,72,74,79,81,83,84,95,95,104,105,107,126,130)
LFDays<-c(295,9,288,319,360,309,304,340,314,5,365,315,336,275,268,282,227)

RstrLocations<-list()
for(i in 1:17){
RstrLocations[i]<-rasterize(FlightLocations[[i]],meanDaily[[i]])
}

FirstFlightTemp<-LastFlightTemp<-array(NA,c(17,4))
for(i in 1:17){
FirstFlightTemp[i,]<-zonal(meanDaily[[FFDays[i]]],RstrLocations[[i]],fun='mean')
LastFlightTemp[i,]<-zonal(meanDaily[[LFDays[i]]],RstrLocations[[i]],fun='mean')
}

FirstFlightTempYears<-LastFlightTempYears<-array(NA,c(17,20))
FirstFlightTempYears[,20]<-FirstFlightTemp[,2]
LastFlightTempYears[,20]<-LastFlightTemp[,2]


Latitude<-c(34.453,32.062,39.920,35.611,40.110,35.384,36.551,33.614,39.033,34.544,40.415,
38.643,41.575,42.921,34.818,45.274,44.075)

meanTempFF<-apply(FirstFlightTempYears,1,mean)
meanTempLF<-apply(LastFlightTempYears,1,mean)
SETempFF<-apply(FirstFlightTempYears,1,sd)/sqrt(17)
SETempLF<-apply(LastFlightTempYears,1,sd)/sqrt(17)

par(bty="l")
plot(meanTempFF~Latitude,pch=19,ylim=c(-1,20),ylab="Flight Temperature (C)",yaxt="n")
segments(Latitude,meanTempFF-SETempFF,Latitude,meanTempFF)
segments(Latitude,meanTempFF+SETempFF,Latitude,meanTempFF)
axis(2,las=2)
par(new=TRUE)
plot(meanTempLF~Latitude,pch=21,ylim=c(-1,20),axes=FALSE,ylab="",xlab="",yaxt="n",xaxt="n")
segments(Latitude,meanTempLF-SETempLF,Latitude,meanTempLF)
segments(Latitude,meanTempLF+SETempLF,Latitude,meanTempLF)

abline(lm(meanTempFF~Latitude))
abline(lm(meanTempLF~Latitude),lty=2)
legend(32,1,legend=c("Spring","Fall"),pch=c(19,21),bty="n",lty=c(1,2))




# Monthly Mean Temperatures 30yr #
monthlyMeanTemp<-list.files("G:/DragonFlies/PRISM_30yr",pattern="*.asc.asc",full.name=TRUE)
MonthlyMean<-list()
for(i in 1:12){
MonthlyMean[i]<-raster(monthlyMeanTemp[i])
}
extract(DD[[1]],IN)
temp<-array(NA,c(17,17))
for(i in 1:17){
for(n in 1:17){
temp[n,i]<-extract(DD[[i]],FlightLocations[[n]],fun=mean,na.rm=TRUE)
}
}

##### Find the latitude of 9 degrees on the first flight date.

#make spatial lines to extract values#

#place<-c(FlightTemps[,1])

#longlines<-lInes<-liNes<-SL<-list()
#for(i in 1:17){
#longlines[[i]]<-matrix(c(FlightTemps[i,9], 23.5,FlightTemps[i,9],50),2,2,byrow=TRUE)
#lInes[[i]]<-Line(longlines[[i]])
#liNes[[i]]<-Lines(list(lInes[[i]]),ID=place[i])
#SL[[i]] <-SpatialLines(list(liNes[[i]]))
#}

folder<-"G:/DragonFlies/PRISM_"
km<-"_4km"
year<-1994:2014
# Daily Mean Temperatures #
for(y in 1:21){
DailyTemp<-list.files(paste0(folder,year[y],km),pattern="*bil.bil",full.name=TRUE)
#DailyTemp<-list.files("G:/DragonFlies/PRISM_1994_4km",pattern="*bil.bil",full.name=TRUE)
#DailyTemp<-list.files("D:/Hallworth/PRISM_tmean_daily/PRISM_1993_4km",pattern="*bil.bil",full.name=TRUE)

MeanDaily<-list()
for(i in 1:365){
MeanDaily[i]<-raster(DailyTemp[i])
}

meanDaily<-stack(MeanDaily)
temp9<-list()
for(i in 1:17){
temp9[i]<-calc(meanDaily[[FFDays[i]]],fun=function(x){x>9.46})
}

v<-ids<-cells<-xy<-combine<-max<-list()
lat<-rep(NA,17)
for(i in 1:17){
v[[i]]<-data.frame(extract(temp9[[i]],SL[[i]]))
ids[[i]]<-init(temp9[[i]],v='cell')
cells[[i]]<-extract(ids[[i]],SL[[i]])
xy[[i]]<-lapply(cells[[i]],function(x){xyFromCell(temp9[[i]],x)})
combine[[i]]<-data.frame(xy[[i]],v[[i]])
colnames(combine[[i]])<-c("x","y","t")
max[[i]]<-apply(subset(combine[[i]],subset=(t==1)),2,max,na.rm=TRUE)
lat[i]<-max[[i]][2]
}
#latitudeYr<-array(NA,c(17,22))
latitudeYr[,y]<-lat
latitudeYr[latitudeYr==-Inf]<-NA
}
#year<-1994:2013
#labels<-paste0("t",year)
#colnames(latitudeYr[,1:20])<-labels
#rownames(latitudeYr)

#write.csv(latitudeYr[,1:20],"Data/NineDegreeLat.csv")

ninelat<-read.csv("Data/NineDegreeLat.csv",header=TRUE)
names(ninelat)
ninelat<-ninelat[,2:21]

mean.nine<-apply(ninelat,1,mean,na.rm=TRUE)
se.nine<-(apply(ninelat,1,sd,na.rm=TRUE)/sqrt(20))

firstflight<-read.csv("Data/FlightTimesSummary.csv",header=TRUE)
names(firstflight)
jul.flight<-with(firstflight,juldate_first)

fit1<-lm(jul.flight~mean.nine)
summary(fit1)

pdf("/Users/studdsc/Dropbox/dragonfly migration/figures/first flight vs nine isotherm.pdf",width=5,height=5)

plot(jul.flight~mean.nine,
xlim=c(34.8,47.2),ylim=c(50,150),
pch=19,cex=1.5,xlab="",ylab="")
segments(mean.nine,jul.flight,mean.nine+se.nine,jul.flight)
segments(mean.nine,jul.flight,mean.nine-se.nine,jul.flight)

mtext((expression("Latitude of 9.46 "^{o}*"C isotherm")),side=1,line = 2.5)
mtext(side=2,"First flight date",line=2.5,cex=1)

dev.off()

plot(FFDays~MeanLatTemp9,ylim=c(30,150),xlim=c(32,48),pch=19,xlab="Latitude of 9*C isotherm",ylab="First Flight Date")
segments(MeanLatTemp9,FFDays,MeanLatTemp9+SELatTemp9,FFDays)
segments(MeanLatTemp9,FFDays,MeanLatTemp9-SELatTemp9,FFDays)
abline(lm(FFDays~MeanLatTemp9))

par(bty="l")
plot(FlightTemps[,8]~MeanLatTemp9,ylim=c(32,48),xlim=c(32,48),pch=19,xlab="Latitude of 9*C isotherm",ylab="Latitude of First Flight Date")
segments(MeanLatTemp9,FlightTemps[,8],MeanLatTemp9+SELatTemp9,FlightTemps[,8])
segments(MeanLatTemp9,FlightTemps[,8],MeanLatTemp9-SELatTemp9,FlightTemps[,8])
abline(1,1,lty=2)
abline(lm(FlightTemps[,8]~MeanLatTemp9))

names(FlightTemps)
MeanFirstFlightTemp<-apply(FlightTemps[,12:31],1,mean,na.rm=TRUE)
SEFirstFlightTemp<-(apply(FlightTemps[,12:31],1,sd,na.rm=TRUE)/sqrt(20))

par(bty="l")
plot(FFDays,MeanFirstFlightTemp,ylim=c(-1,15),xlim=c(50,130),pch=19,xlab="First Flight Date",ylab="Temperature at First Flight")
segments(FFDays,MeanFirstFlightTemp,FFDays,MeanFirstFlightTemp-SEFirstFlightTemp)
segments(FFDays,MeanFirstFlightTemp,FFDays,MeanFirstFlightTemp+SEFirstFlightTemp)

