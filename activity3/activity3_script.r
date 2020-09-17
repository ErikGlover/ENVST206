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
