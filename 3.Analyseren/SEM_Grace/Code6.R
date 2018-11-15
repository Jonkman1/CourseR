####### SEM.6-SEM VERSUS MULTIPLE REGRESSION

### Set working directory, load data, recode data
setwd("G:/SEM/Grace") #commands in bold
dat <- read.csv("SEM.6-SEM versus Multiple Regression data.csv")
names(dat)
attach(dat)

### Recode vars to roughly same scale
rich     <- rich/10
firesev  <- firesev
abiotic  <- abiotic/10
hetero   <- hetero*10
age      <- age/10
distance <- distance/10
t.dat <- data.frame(rich, firesev, abiotic, hetero, age, distance)

### Load Libraries
library(lavaan)
library(AICcmodavg)
source("lavaan_modavg.R")  # from "http://jarrettbyrnes.info/ubc_sem/lavaan_materials/lavaan.modavg.R"


### Multiple regression models using lm
mr.lm1 <- lm(rich ~ firesev + abiotic + hetero + age + distance, data=t.dat); summary(mr.lm1)
mr.lm2 <- lm(rich ~ firesev + abiotic + hetero + distance, data=t.dat); summary(mr.lm2)
mr.lm3 <- lm(rich ~ abiotic + hetero + distance, data=t.dat); summary(mr.lm3)

anova(mr.lm1, mr.lm2, mr.lm3)
AIC(mr.lm1, mr.lm2, mr.lm3)

AICc(mr.lm1); AICc(mr.lm2); AICc(mr.lm3)


########### SEM FOR FIRE RECOVERY STUDY (using lavaan) #########
# First run most comprehensive model “fire.2”
# and check for missing paths

# specify “mod.2”
mod.2 <- 'rich ~ abiotic + hetero + distance + firesev + age
	         abiotic ~ distance
	  	     hetero ~ distance
		       age ~ distance
		       firesev ~ age'
mod.2.fit <- sem(mod.2, data=t.dat)
summary(mod.2.fit)

# specify “mod.1”
mod.1 <- 'rich ~ abiotic + hetero + distance + firesev + 0*age
	         abiotic ~ distance
	  	     hetero ~ distance
		       age ~ distance
		       firesev ~ age'
mod.1.fit <- sem(mod.1, data=t.dat)
summary(mod.1.fit)

anova(mod.1.fit, mod.2.fit)
aictab.lavaan(list(mod.1.fit, mod.2.fit), c("mod.1", "mod.2"))

# Results from selected model
summary(mod.1.fit, rsq=T, standardized=T)
summary(mod.1.fit, rsq=T, std.all=T)


