#activity7

datD <- read.csv("/Users/erikglover/Documents/Environmental Data/dm_export_19601015_20201023 copy.csv")


datD$date <- as.Date(datD$ValidStart, "%Y-%m-%d")

library(ggplot2)

ggplot(data=datD,aes(x=date, y=D2))+geom_path()+ggtitle("Severe drought in New York State")+ylab("% of state in severe drought")




mean(datD$D2)
sd(datD$D2)
