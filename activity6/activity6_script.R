install.packages(c("sp","rgdal","dplyr"))

#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)

#read in shapefiles
#readOGR in rgdal does this
#glaciers in 1966
g1966 <- readOGR("/Users/erikglover/Documents/Environmental Data/a06/GNPglaciers/GNPglaciers_1966.shp")

#glaciers in 2015
g2015 <- readOGR("/Users/erikglover/Documents/Environmental Data/a06/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

#map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col = "lightblue2", border="grey50")

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

g1966@proj4string

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#lets combine area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name

gAll <- full_join(gdf66,gdf15, by="GLACNAME")

#calculate the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

plot(gAll$area66, gAll$gdiff, pch=19, xlab="1966 Glacier Area (square km)", ylab="% Change in Area (square km)", yaxt="n")

#join data with the spatial data table and overwrite into spatial data table 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
#use spplot to shade polygons based on the % change of labels
#first argument is the spatial object
#second is the column in of data to display with the different colors
#add a title using main
#col changes the color of the borders. This argument sets them to transparent
spplot(g1966, "gdiff", main="% change in area", col="transparent")

#look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")

mean(gAll$gdiff)
sd(gAll$gdiff)

min(gAll$gdiff)
max(gAll$gdiff)

#Boulder
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, main = "Boulder Glacier in 1966-2015", col="slategray")

boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier"]
plot(boulder15, add=TRUE, col="blue")
legend("bottomleft", c("Boulder 1966", "Boulder 2015"), col=c("slategray", "blue"), fill=("slategray"), bty="n")

legend("bottomleft", c("Boulder 1966", "Boulder 2015"), col=fill, bty="n")

#Pumpelly
pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
plot(boulder66, main = "Pumpelly Glacier in 1966-2015", col="slategray")

pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(pumpelly15, add=TRUE, col="blue")
legend("bottomleft", c("Pumpelly 1966", "Pumpelly 2015"), col=c("slategray", "blue"), fill=("slategray"), bty="n")

