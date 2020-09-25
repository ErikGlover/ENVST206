datB <- read.csv("/Users/erikglover/Documents/Environmental Data/a04/beaver_dam.csv")

plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

#set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)

#get standarized residuals
dam.res <- rstandard(dam.mod)

#set up qq plot
qqnorm(dam.res)
#add qq line
qqline(dam.res)

shapiro.test(dam.res)
## 
##  Shapiro-Wilk normality test
## 
## data:  dam.res
## W = 0.92993, p-value = 0.3793

#make residual plot
plot(datB$dams.n, dam.res, 
     xlab = "beaver damns", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)
summary(dam.mod)
## 
## Call:
## lm(formula = datB$area.ha ~ datB$dams.n)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.639  -7.191  -1.006   6.954  14.772 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 606.00410    4.13835 146.436  < 2e-16 ***
## datB$dams.n   0.31769    0.08037   3.953  0.00272 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.251 on 10 degrees of freedom
## Multiple R-squared:  0.6097, Adjusted R-squared:  0.5707 
## F-statistic: 15.62 on 1 and 10 DF,  p-value: 0.002718

#make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")
#add regression line
#make line width thicker
abline(dam.mod, lwd=2)

pheno <- read.csv("/Users/erikglover/Documents/Environmental Data/a04/red_maple_pheno.csv")

#set up panel of plots with one row and two columns
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

#latitude plot
plot(pheno$Lat,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Latitude (degree)")

#elavation plot
plot(pheno$elev,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Elevation (feet)")

#maximum temperature plot
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (degrees celsius)")

#site description plot
plot(as.factor(pheno$siteDesc),pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Site description")

plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)

mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)

mlFitted <- fitted(mlr)

#get standarized residuals
mlr.res <- rstandard(mlr)

##qq plot
qqnorm(mlr.res)
qqline(mlr.res)

#plot
plot(mlFitted, mlr.res)
abline(h=0)

summary(mlr)



