library(ggplot2)

#read in weather station file from the data folder
datW <- read.csv("/Users/erikglover/Documents/Environmental Data/a02/noaa2011124.csv")
#specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)
#set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS[2]

#make dataframe
datP <- na.omit(data.frame(PRCP = datW$PRCP, 
                   NAME = datW$NAME,
                   year = datW$year))

#total annual precipitation for site
precip <- aggregate(datP$PRCP, by=list(datW$NAME, datP$year), FUN="sum")
#use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME", "year", "totalP")
#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="length")$x

pr <- pr[pr$ncount >= 364,]

#look at only livermore california and morrisville new york preciptiation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

#make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year",
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
#arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
#las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

#make new dataframe with max temp, name, year
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))
meantemp <- aggregate(datW$TMAX, by=list(datW$NAME,datW$year), FUN="mean", na.rm=TRUE)
meantemp <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)
colnames(meantemp) <- c("NAME","year","mean annual temp")
meantemp$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x
tm <- meantemp[meantemp$ncount >= 364, ]

nd <- tm[tm$NAME == nameS[3], ]
ny <- tm[tm$NAME == nameS[5], ]

#mean annual temperature plot
plot(nd$year, nd$`mean annual temp`,
     type = "b",
     pch = 19,
     ylab = "Annual mean maximum temperature (C)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 20),
     main =("Annual mean maximum temperature in Mandan North Dakota vs Morrisville New York"),
     cex.main= 1)
#add y axis
axis(2, seq(0,50, by=5), las=2 )
#add ny
points(ny$year, ny$`mean annual temp`,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("topleft", #position
       c("North Dakota", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn




install.packages("ggplot2")
#base r plot
plot(pr$year, pr$totalP)
#gg plot
ggplot(data = pr, 
       aes(x = year,
           y = totalP,
           color = NAME))+ #data for plot
  geom_point(alpha=0.5)+ #make points at data point
  geom_path(alpha=0.5)+ #use lines to connect data points
  labs(x= "year", y = "Annual precipitation (mm)")+ #make axis labels
  theme_classic()+ #change plot theme
  scale_color_manual(values = c("#E495A5", "#BDAB66", "#65BC8C", "#55B8D0", "#C29DDE"))
 
  #install and run a color palette program
  install.packages("colorspace")
  colorspace::rainbow_hcl(5)
  
  ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
    geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
    geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
    theme_classic() #git rid of ugly gridlines

  sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]
  
  #specify date format
  #%Y means a four number year 
  #- indicates that the date uses dashes to seperate
  #%m means month
  #%d means day
  sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

  ggplot(data=sub, aes(x=DATE, y=TMAX))+
    geom_point()+
    geom_path()+
    theme_classic()+
    labs(x="year", y="Maximimum temperature (C)")

  ggplot(data=sub, aes(x=DATE, y=PRCP))+
    geom_col(fill="royalblue3")+
    theme_classic()+
    labs(x="year", y="Daily precipitation (mm)")  

  #precipitation and max temp for Aberdeen, WA
  
  wa <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
  
  wa$DATE <- as.Date(wa$DATE,"%Y-%m-%d")
  
  ggplot(data=wa, aes(x=DATE, y=TMAX))+
    geom_point()+
    geom_path()+
    theme_classic()+
    labs(x="year", y="Maximimum temperature (C)")
  
  ggplot(data=wa, aes(x=DATE, y=PRCP))+
    geom_col(fill="royalblue3")+
    theme_classic()+
    labs(x="year", y="Daily precipitation (mm)")  

  #new dataframe for Aberdeen, Wa from 2000-2019
  datA <- datW[datW$NAME == nameS[1] & datW$year >= 2000,]
  abmin <- datA[datA$TMIN, datA$DATE,]
  
  datA$DATE <- as.Date(datA$DATE,"%Y-%m-%d")
  #
  ggplot(data=datA, aes(x=DATE, y=TMIN))+
    geom_col(fill="royalblue3")+
    theme_classic()+
    labs(x="year", y="Daily minimum temperatures (C)")+
    ggtitle("Daily minimum temperatures in Aberdeen WA")
  
  
  
  ggplot(data = datA, aes(x=as.factor(year), y=TMIN))+ 
    geom_violin(fill=rgb(0.933,0.953,0.98))+ 
    geom_boxplot(width=0.2,size=0.25, fill="grey90")+ 
    theme_classic()+
    labs(x="year", y="Daily minimum temperatures (C)")+
    ggtitle("Daily minimum temperatures in Aberdeen WA")
  