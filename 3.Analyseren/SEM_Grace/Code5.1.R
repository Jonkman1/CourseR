### Test of Mediation Exercise
### 3-variable submodel from Whalen et al.
### last updated 15.04.08

setwd("G:/SEM/Grace")
dat <- read.csv("SEM.5-Test of Mediation Exercise data.csv")
names(dat)
attach(dat)

### Examine Group Difference
boxplot(Epiphytes ~ Pesticide, xlab="Pesticide", ylab="Epiphyte Chlorophyll a")

### lavaan modeling
library(lavaan)

### Full mediation model
# Specify model
full.med <- 'Gammarids ~ Pesticide
             Epiphytes ~ Gammarids'

# Fit model
full.med.fit <- sem(full.med, dat)

# Examine model fit
show(full.med.fit)

# Examine diagnostics - modification indices
subset(modindices(full.med.fit), mi > 3.8)

# Examine diagnostics - residuals
residuals(full.med.fit, type="standardized")

### Partial mediation model
# Specify model
part.med <- 'Gammarids ~ Pesticide
             Epiphytes ~ Gammarids + Pesticide'

# Fit model
part.med.fit <- sem(part.med, dat)

# Examine model fit and parameters
summary(part.med.fit)

### No mediation model
# Specify model
no.med <- 'Gammarids ~ Pesticide
           Epiphytes ~ 0*Gammarids + Pesticide'

# Fit model
no.med.fit <- sem(no.med, dat)

# Examine model fit
show(no.med.fit)

### Model Comparison - AICc
library(AICcmodavg) 
source("lavaan.modavg.R")
aictab.lavaan(list(full.med.fit, part.med.fit, no.med.fit), c("Full", "Partial", "None"))
                                                       
### Examine results for selected model - par.med
summary(part.med.fit, rsq=TRUE, standardized=TRUE)