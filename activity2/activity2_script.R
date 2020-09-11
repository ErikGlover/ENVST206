#activity 2

heights <- c(3,2,3)


datW <- read.csv("/Users/erikglover/Documents/Environmental Data/a02/noaa2011124.csv")

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

#look at the first tree height
heights[1]
## [1] 30
#look at the 2nd and 3rd tree heights
heights[2:3]
## [1] 41 20

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]

#look at all values in row 1
Mat.bycol[1,]

#look at all values in column 2
Mat.bycol[,2]

str(datW)

datW$NAME <- as.factor(datW$NAME)

#example of a character vector
y = c("a", "b", "c", "d", "e")
length(y)

#example of numerical vector
snowinches <-- c(1.3, 4.2, 6.7, 3.5, 4.6)
snowinches/2

#example of integer vector
cows <-- c(2, 4, 5, 8, 10)
cows*2

#example of a factor vector
v <- gl(3, 2, labels = c("red", "blue","green", "yellow", "purple"))
print(v)

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#next look at the standard deviation
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)


#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

help(hist)

hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

help(dnorm)

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
                                                         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm gives me the value at which all values and below equal the probability in my argument
#Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnorm if climate change increases temps by 4
pnorm(18.51026,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#histogram of daily precipitation 
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Daily precipitation (cm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white",)
    
#range of data
range(datW$PRCP,na.rm=TRUE)

#total precipitation by site and year 
x <- datW$PRCP
aggregate(x = datW$PRCP, by = list(datW$siteN, datW$year), FUN=sum, na.rm=TRUE)

#assign the aggregate a new variable
annualPRCPsum <- aggregate(x = datW$PRCP, by = list(datW$siteN, datW$year), FUN=sum, na.rm=TRUE)


colnames(annualPRCPsum) <- c("NAME", "Year","PRCP")

#histogram of annual precipitation in Aberdeen
hist(annualPRCPsum$PRCP[annualPRCPsum$NAME == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Total annual precipitation (cm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

#histogram of annual precipitation in Mandan
hist(annualPRCPsum$PRCP[annualPRCPsum$NAME == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Total annual precipitation (cm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")


#probability of year with 700mm or less precipitation in Aberdeen
pnorm(7,
      mean(annualPRCPsum$PRCP[annualPRCPsum$NAME == 1],na.rm=TRUE),
      sd(annualPRCPsum$PRCP[annualPRCPsum$NAME == 1],na.rm=TRUE))

#probability of year with 700mm or less precipitation in Mandan
pnorm(7,
      mean(annualPRCPsum$PRCP[annualPRCPsum$NAME == 3],na.rm=TRUE),
      sd(annualPRCPsum$PRCP[annualPRCPsum$NAME == 3],na.rm=TRUE))



