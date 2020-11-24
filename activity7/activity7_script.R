#activity7
library(lubridate)


datD <- read.csv("/Users/erikglover/Documents/Environmental Data/dm_export_19601015_20201023 copy.csv")


datD$date <- as.Date(datD$ValidStart, "%Y-%m-%d")

datD$month <- month(datD$date)
datD$year <- year(datD$date)

datS <- datD[datD$month >= 6 & datD$month <= 9,]

##New York D1
maxS <- aggregate(datD$D1, by=list(datD$year), FUN="max")

colnames(maxS) <- c("year","maxD1")

plot(maxS$year, maxS$maxD1, main="New York Max % in Moderate Drought per Year", xlab="Year", ylab="D1 Moderate Drought")

#checking residuals
maxS.mod <- lm(maxS$maxD1 ~ maxS$year)
maxS.res <- rstandard(maxS.mod)
#set up qq plot
qqnorm(maxS.res)
#add qq line
qqline(maxS.res)

#Shapiro-Wilk normality test
shapiro.test(maxS.res)
#p-value=0.003277 so we reject that the data is normally distributed

library(ggplot2)

ggplot(data=datD,aes(x=date, y=D1))+geom_path()+ggtitle("Moderate drought in New York State")+theme(plot.title = element_text(hjust = 0.5))+ylab("% of state in moderate drought")



#New York D2
maxSD2 <- aggregate(datD$D2, by=list(datD$year), FUN="max")

colnames(maxSD2) <- c("year","maxD2")

plot(maxSD2$year, maxSD2$maxD2, main="New York Max % in Severe Drought per Year", xlab="Year", ylab="D2 Severe Drought")

#checking residuals
maxSD2.mod <- lm(maxSD2$maxD2 ~ maxSD2$year)
maxSD2.res <- rstandard(maxSD2.mod)
#set up qq plot
qqnorm(maxSD2.res)
#add qq line
qqline(maxSD2.res)

#Shapiro-Wilk normality test
shapiro.test(maxSD2.res)
#p-value = 5.581e-06 so we reject that the data is normally distributed
summary(maxSD2.mod)



####Massachusetts D1
datMAD <- read.csv("/Users/erikglover/Documents/Environmental Data/dm_export_19901119_20201119.csv")


datMAD$date <- as.Date(datMAD$ValidStart, "%Y-%m-%d")

datMAD$month <- month(datMAD$date)
datMAD$year <- year(datMAD$date)

datMAS <- datMAD[datMAD$month >= 6 & datMAD$month <= 9,]

maxMAS <- aggregate(datMAD$D1, by=list(datMAD$year), FUN="max")

colnames(maxMAS) <- c("year","maxD1")

plot(maxMAS$year, maxMAS$maxD1, main="Massachusetts Max % in Moderate Drought per Year", xlab="Year", ylab="D1 Moderate Drought")

ggplot(data=datMAD,aes(x=date, y=D1))+geom_path()+ggtitle("Moderate drought in Massachusetts")+ylab("% of state in moderate drought")


#checking residuals
maxMAS.mod <- lm(maxMAS$maxD1 ~ maxMAS$year)
maxMAS.res <- rstandard(maxMAS.mod)
#set up qq plot
qqnorm(maxMAS.res)
#add qq line
qqline(maxMAS.res)

#Shapiro-Wilk normality test
shapiro.test(maxMAS.res)
#p-value = 0.0985 so we cannot reject that the data is normally distributed
#make residual plot
plot(maxMAS$maxD1, maxMAS.res, 
     xlab = "Max temp per year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Regression table
summary(maxMAS.mod)



####Massachusetts D2
maxMASD2 <- aggregate(datMAD$D2, by=list(datMAD$year), FUN="max")

colnames(maxMASD2) <- c("year","maxD2")

plot(maxMASD2$year, maxMASD2$maxD2, main="Massachusetts Max % in Severe Drought per Year", xlab="Year", ylab="D2 Severe Drought")

ggplot(data=datMAD,aes(x=date, y=D2))+geom_path()+ggtitle("Severe drought in Massachusetts")+ylab("% of state in severe drought")


#checking residuals
maxMASD2.mod <- lm(maxMASD2$maxD2 ~ maxMASD2$year)
maxMASD2.res <- rstandard(maxMASD2.mod)
#set up qq plot
qqnorm(maxMASD2.res)
#add qq line
qqline(maxMASD2.res)

#Shapiro-Wilk normality test
shapiro.test(maxMASD2.res)
#p-value = 0.0004065 so we reject that the data is normally distributed
summary(maxMASD2.mod)



###maps###

#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)

library(raster)
library(rgeos)

maps <- readOGR("/Users/erikglover/Documents/Environmental Data/USDM_20161018_M (1)/USDM_20161018.shp")

plot(maps, col = "lightblue2", border="grey50")

maps0 <- maps[maps@data$DM==0,]
plot(maps0, add=TRUE, col="orange")

states <- readOGR("/Users/erikglover/Documents/USA/STATES.shp")
statesP <- spTransform(states,maps@proj4string)

plot(statesP, col="gray" )
NewYork <- statesP[statesP@data$STATE_NAME=="New York",]
d0NY <- raster::crop(maps0,NewYork)
maps1 <- maps[maps@data$DM==1,]
maps2 <- maps[maps@data$DM==2,]
maps3 <- maps[maps@data$DM==3,]
maps4 <- maps[maps@data$DM==4,]
d1NY <- raster::crop(maps1,NewYork)
d2NY <- raster::crop(maps2,NewYork)
d3NY <- raster::crop(maps3,NewYork)
d4NY <- raster::crop(maps4,NewYork)
plot(NewYork, main="Drought in Massachusetts and New York on 10/18/16", font.main=1)
plot(d0NY, add=TRUE, col="green")
plot(d1NY, add=TRUE, col="yellow")
plot(d2NY, add=TRUE, col="orange")
plot(d3NY, add=TRUE, col="red")
Mass <- statesP[statesP@data$STATE_NAME=="Massachusetts",]
plot(Mass, add=TRUE)
d0MA <- raster::crop(maps0,Mass)
d1MA <- raster::crop(maps1,Mass)
d2MA <- raster::crop(maps2,Mass)
d3MA <- raster::crop(maps3,Mass)
d4MA <- raster::crop(maps4,Mass)
plot(d0MA, add=TRUE, col="green")
plot(d1MA, add=TRUE, col="yellow")
plot(d2MA, add=TRUE, col="orange")
plot(d3MA, add=TRUE, col="red")

legend(x="topleft", y=0.01, legend=c("Abnormally Dry", "Moderate Drought", "Severe Drought", "Extreme Drought"),
       col=c("green", "yellow", "orange", "red"), fill = c("green", "yellow", "orange", "red"), bty="n",
       title="Drought Levels", text.font=2, bg='white', cex = 0.75)

