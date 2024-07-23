# Read Data

if(!require(devtools)) install.packages("devtools")
devtools::install_github("thoree/BIAS.data")


if(!require(devtools)) install.packages("devtools")
devtools::install_github("unitedstates/congress-legislators")


unitedstates/congress-legislators



library(BIAS.data)
data(pines)
head(pines)

#Prepration data for analysis

summary(pines)
str(pines)
dim(pines)

table(pines$Treatment)
table(pines$Family)
table(pines$Box)

class(pines$Treatment)
class(pines$Family)
class(pines$Box)


library(dplyr)
 

pines<- pines %>%
  mutate(Treatment= factor(Treatment, c("Control", "MeJA", "PipA")))


pines<- pines %>%
  mutate(Family= factor(Family, c("a", "b", "c", "d")))


pines<- pines %>%
  mutate(Box= factor(Box, c("1", "2", "3", "4")))


#Model 

pines.nested <- pines %>%
  mutate(Treatment.Family.Box= Treatment:Family:Box)


#Fit to data

fixed.obj <- lm(ddCt ~ Treatment+Family , data=pines)
summary(fixed.obj)
anova(fixed.obj)
library(effects)
plot(Effect("Treatment", fixed.obj))
plot(Effect("Family", fixed.obj))
plot(allEffects( fixed.obj))


library(nlme)
library(lme4)
options(contrasts = c("contr.treatment", "contr.ploy"))
# crossed.obj1 <- lme(ddCt ~ Treatment +Family +Treatment:Family , random = ~1|Box, data = pines)
crossed.obj <- lmer(ddCt ~ Treatment +Family +Treatment:Family +(1|Box), data = pines)
summary(crossed.obj)
anova(crossed.obj)


nested.obj <- lmer(ddCt ~ Treatment +Family +Treatment:Family +(1|Treatment:Family:Box) , data = pines)
summary(nested.obj)

anova(fixed.obj)

