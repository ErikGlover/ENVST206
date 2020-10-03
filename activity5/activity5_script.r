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
pr <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="sum")
colnames(pr) <- c("NAME", "year", "totalP")
pr$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="length")$x

pr <- pr[pr$ncount >= 364,]

install.packages("ggplot2")
#base r plot
plot(pr$year, pr$totalP)
#gg plot
ggplot(data = pr, 
       aes(x = year,
           y = totalP,
           color = NAME))+
  geom_point()+
  geom_path()+
  labs(x= "year", y = "Annual precipitation (mm)")+
  theme_classic()

  
  
  
  
