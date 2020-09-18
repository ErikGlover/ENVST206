#activity2

ch4 <- read.csv("/Users/erikglover/Documents/Environmental Data/a03/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)


plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")


shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
##  Shapiro-Wilk normality test
## 
## data:  ch4$CH4_Flux[ch4$herbivory == "Ex"]
## W = 0.93325, p-value = 0.4158


shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])
##  Shapiro-Wilk normality test
## 
## data:  ch4$CH4_Flux[ch4$herbivory == "Ctl"]
## W = 0.87763, p-value = 0.08173


#dependent variable ~ independent variable
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)
## 
##  Bartlett test of homogeneity of variances
## 
## data:  ch4$CH4_Flux by ch4$herbivory
## Bartlett's K-squared = 0.21236, df = 1, p-value = 0.6449

t.test(ch4$CH4_Flux ~ ch4$herbivory)
## 
##  Welch Two Sample t-test
## 
## data:  ch4$CH4_Flux by ch4$herbivory
## t = 1.5328, df = 21.569, p-value = 0.1399
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -4.644609 30.844370
## sample estimates:
## mean in group Ctl  mean in group Ex 
##         18.814645          5.714765

#read in insect data
datI <- read.csv("/Users/erikglover/Documents/Environmental Data/a03/insect_richness.csv")
datI$urbanName <- as.factor(datI$urbanName)
datI$urbanType <- as.factor(datI$urbanType)

#Shapiro test for urban areas
shapiro.test(datI$Richness[datI$urbanName == "Natural"])
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])
shapiro.test(datI$Richness[datI$urbanName == "Developed"])
shapiro.test(datI$Richness[datI$urbanName == "Dense"])

#Bartlett test
bartlett.test(list(datI$Richness[datI$urbanName == "Natural"], datI$Richness[datI$urbanName == "Suburban"], datI$Richness[datI$urbanName == "Developed"],datI$Richness[datI$urbanName == "Dense"])) 

#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run the ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table
summary(in.aov)
##                 Df Sum Sq Mean Sq F value  Pr(>F)   
## datI$urbanName   3   1944   647.9   4.898 0.00254 **
## Residuals      236  31216   132.3                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = in.mod)
## 
## $`datI$urbanName`
##                         diff         lwr        upr     p adj
## Developed-Dense     1.433333  -3.5966663  6.4633329 0.8819569
## Natural-Dense      12.583333   3.3998525 21.7668141 0.0026479
## Suburban-Dense      3.785714  -0.8060261  8.3774547 0.1456297
## Natural-Developed  11.150000   1.7397324 20.5602676 0.0128693
## Suburban-Developed  2.352381  -2.6776186  7.3823805 0.6210733
## Suburban-Natural   -8.797619 -17.9810999  0.3858618 0.0658910

#make a plot
#make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)

tapply(datI$Richness, datI$urbanName, "mean")
##     Dense Developed   Natural  Suburban 
##  19.83333  21.26667  32.41667  23.61905

#set up contigency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

#make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

#Conduct a chi-squared test
chisq.test(species)
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  species
## X-squared = 7.9642, df = 1, p-value = 0.004771