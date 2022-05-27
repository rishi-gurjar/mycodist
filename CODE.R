library(tidyverse)
library(ggplot2)
library(tidyr)
library(multcompView)

dat <- read.csv("/Users/rishigurjar/Desktop/MycoResearch/dat/MycoData.csv")
dat1 <- read.csv("/Users/rishigurjar/Desktop/MycoResearch/dat/ShanDiv.csv")

names(dat)
head(dat)
attach(dat)
View(dat)

dist = dat[ which(dat$DistStatus == 'Y'), ]
dist
undist = dat[which(dat$DistStatus == 'N'), ]
undist

chem = dat[which(dat$GenDistType == 'Chemical'), ]
chem

above = dat[which(dat$GenDistType == 'Above Ground'), ]
above

agri = dat[which(dat$GenDistType == 'Agriculture'), ]
agri

shanundist = dat1[which(dat1$GenDistType == 'Undisturbed'), ]
shanundist

shanabove = dat1[which(dat1$GenDistType == 'Above Ground'), ]
shanabove

shanagri = dat1[which(dat1$GenDistType == 'Agriculture'), ]
shanagri



mean(chem$Value)
mean(above$Value)
mean(agri$Value)
mean(undist$Value)

mean(shanundist$Value)
mean(shanabove$Value)
mean(shanagri$Value)

sd(chem$Value) #ADD SD


dist1 = dat1[ which(dat1$DistStatus == 'Y'), ]
dist1
undist1 = dat1[which(dat1$DistStatus == 'N'), ]
undist1

# Species Richness vs Disturbance Y/N
boxplot(dist$Value, undist$Value, names = c("Disturbed", "Undisturbed"), 
        col = c("Blue", "Red"), ylab = "Species Richness")

# Shannon Index vs Disturbance Y/N
boxplot(dist1$Value, undist1$Value, names = c("Disturbed", "Undisturbed"), 
        col = c("Blue", "Red"), ylab = "Shannon Index")

#multiple box plot
ggplot(dat, mapping = aes(x = as.factor(GenDistType)
                          , y = Value)) +
  geom_boxplot(aes(fill = GenDistType)) + 
  theme_classic() +
  labs(x ="General Disturbance Type"
       , y = "Species Richness"
       , title = "Species Richness vs General Disturbance Type")+
  scale_fill_discrete(name = "General Disturbance Type")+
  geom_jitter()



shapiro.test(dist$Value)
#p = 0.0002733

shapiro.test(undist$Value)
#p = 0.0008428
#use wilcox test for difference because data is non-normal

wilcox.test(dist$Value, undist$Value, exact=FALSE)
#p = 0.2403
wilcox.test(dist1$Value, undist1$Value, exact=FALSE)
#p = 0.272

#t-test out of curiosity
t.test(dist1$Value, undist1$Value)
#p = 0.1877

#Tukey test ERROR
ANOVA=aov(Value ~ GenDistType)
TUKEY = TukeyHSD(x=ANOVA, conf.level=0.95)
plot(TUKEY , las=1 , col="blue")

TUKEY #p value
summary(ANOVA)

#Tukey test ERROR
ANOVA=aov(Value ~ GenDistType)
TUKEY = TukeyHSD(x=ANOVA, conf.level=0.95)
plot(TUKEY , las=1 , col="blue")

TUKEY #p value
summary(ANOVA)
