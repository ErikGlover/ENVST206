#activity2

ch4 <- read.csv("/Users/erikglover/Documents/Environmental Data/a03/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)


plot(ch4$CH4_Flux ~ ch4$herbivory)

shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#dependent variable ~ independent variable
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

t.test(ch4$CH4_Flux ~ ch4$herbivory)
