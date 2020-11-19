#activity7
library(lubridate)


datD <- read.csv("/Users/erikglover/Documents/Environmental Data/dm_export_19601015_20201023 copy.csv")


datD$date <- as.Date(datD$ValidStart, "%Y-%m-%d")

datD$month <- month(datD$date)
datD$year <- year(datD$date)

datS <- datD[datD$month >= 6 & datD$month <= 9,]

maxS <- aggregate(datD$D1, by=list(datD$year), FUN="max")

colnames(maxS) <- c("year","maxD1")

plot(maxS$year, maxS$maxD1)


library(ggplot2)

ggplot(data=datD,aes(x=date, y=D1))+geom_path()+ggtitle("Moderate drought in New York State")+ylab("% of state in severe drought")

#subset by summmer months
NYsummer <- selectByDate(datD, 
             month = 6-8)


