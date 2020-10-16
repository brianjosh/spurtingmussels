#Analyses to support "Fishing for hosts: larval spurting by the endangered 
#thick-shelled river mussel, Unio crassus" (Aldridge DC, Brian JI, Cmiel A, 
#Lipinska A, Lopes-Lima M, Sousa R, Teixeira A, Zajac K, Zajac T).

-----------------------------------------------------------

#Load necessary libraries

library(car)
library(dplyr)

-----------------------------------------------------------
  
#Analysis of fish attraction data 

fishwilcox <- read.csv("fish_attraction.csv", header=T)
str(fishwilcox)

fishmod1 <- lm(proportion ~ area, data=fishwilcox)

par(mfrow = c(2, 2))
plot(fishmod1)
leveneTest(fishmod1)

#Assumption of equal not satisfied - attempt log and arcsine transformation

fishwilcox <- mutate(fishwilcox, logproportion = log(proportion+1))
fishwilcox <- mutate(fishwilcox, asinproportion = asin(sqrt(proportion)))

fishmod2 <- lm(logproportion ~ area, data=fishwilcox)
fishmod3 <- lm(asinproportion ~ area, data=fishwilcox)

par(mfrow = c(2, 2))
plot(fishmod2)
leveneTest(fishmod2)
plot(fishmod3)
leveneTest(fishmod3)

#Assumptions still not satisfied - utilise non-parametric Wilcoxon paired test

wilcox.test(fishwilcox$proportion ~ fishwilcox$area, paired=TRUE)

#Confirm pattern consistent for individual mussels observed

aggregate(fishwilcox[ ,6], list(fishwilcox$mussel.ID, fishwilcox$area), mean)

-----------------------------------------------------------
  
#Analysis of mussel length vs. maximum spurt distance 

length <- read.csv("length_distance.csv", header=T)
str(length)

lengthmod <- lm(maxsquirt ~ length, data=length)
par(mfrow = c(2, 2))
plot(lengthmod) #Assumptions satisfied

summary(lengthmod)
anova(lengthmod)

-----------------------------------------------------------
  
# Analysis of spurt volume and number of glochidia per spurt

gloch <- read.csv("volume_glochidia.csv", header=T)
str(gloch)

glochmod <- lm(loggloch ~ volume, data=gloch)
par(mfrow = c(2, 2))
plot(glochmod) #Assumptions satisfied 

summary(glochmod)
anova(glochmod)

