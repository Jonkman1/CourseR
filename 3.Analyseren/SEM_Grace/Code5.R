####### SEM.5-THE TEST OF MEDIATION Rscript

### Set working directory, load data, recode data
setwd("G:/SEM/Grace") #commands in bold
dat <- read.csv("SEM.5-Test of Mediation data.csv")
names(dat)
attach(dat)

### Recode vars to roughly same scale
age     <- age/10
firesev <- 1*firesev
cover   <- cover*10
t.dat <- data.frame(age, firesev, cover)  # overwrite file with recoded data

### Load Libraries
library(lavaan)
library(AICcmodavg)
source("lavaan_modavg.R")  # from "http://jarrettbyrnes.info/ubc_sem/lavaan_materials/lavaan.modavg.R"


######### TEST OF MEDIATION ##########
### Net (total) effect of age on cover
mod.1 <- 'cover ~ age'                 # Specify model
mod.1.fit <- sem(mod.1, data=t.dat)   # Fit the model
summary(mod.1.fit, stand=T, rsq=T)     # Extract results

### Three degrees of mediation
## Complete mediation
comp.mod  <- 'cover ~ firesev
              firesev ~ age'
comp.mod.fit <- sem(comp.mod, data=t.dat)

## Partial mediation
partial.mod <- 'cover ~ firesev + age
                firesev ~ age'
partial.mod.fit <- sem(partial.mod, data=t.dat)

## No mediation
nomed.mod <- 'firesev ~ 0*age
              cover ~ age + 0*firesev'
nomed.mod.fit <- sem(nomed.mod, data=t.dat)

## Compare models with anova
anova(comp.mod.fit, partial.mod.fit, nomed.mod.fit)

## Compare three models with AICc
aictab.lavaan(list(comp.mod.fit, partial.mod.fit, nomed.mod.fit), c("Complete", "Partial", "None"))

## Compare two models with AICc
aictab.lavaan(list(comp.mod.fit, partial.mod.fit), c("Complete", "Partial"))

## Complete mediation alternative specification with 0* multiplier
comp.mod2  <- 'cover ~ firesev + 0*age
               firesev ~ age'
comp.mod2.fit <- sem(comp.mod2, data=t.dat)

## Compare two models with AICc  - same result
aictab.lavaan(list(comp.mod2.fit, partial.mod.fit), c("Complete2", "Partial"))

# a small digression: asking for the intercepts
comp.mod.fit <- sem(comp.mod, meanstructure=T, data=t.dat)
summary(comp.mod.fit)

### Compute indirect and total effects for partial mediation model
## Partial mediation
mod.4 <- 'cover ~ b*firesev + c*age
          firesev ~ a*age

          # define quantities 
          direct   := c 
          indirect := a*b
          total    := c + (a*b)
         '

# Fit the model
mod.4.fit <- sem(mod.4, data=t.dat)

# Extract results
summary(mod.4.fit, stand=T, rsq=T)