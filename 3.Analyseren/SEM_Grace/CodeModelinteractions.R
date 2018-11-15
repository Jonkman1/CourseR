##### MODELING INTERACTIONS
### Data from Cherry, J.A., McKee, K.L., and Grace, J.B. 2009. Journal of Ecology 97:67-77.

setwd("G:/SEM/Grace")
# setwd("F:/ppt_files/_education/FSP/Modeling Interactions/Original Files for Review")

dat <- read.csv("SEM-Modeling Interactions_data.csv")
names(dat)
attach(dat)

### Transformations
ln.C3prod <- log(C3prod +1)

### Plots
dev.new(height=4, width=8); par(mfrow=c(1,2)); hist(C3prod); hist(ln.C3prod)
CO2trt <- factor(CO2)
salt.trt <- factor(Salinity)
boxplot(ln.C3prod ~ salt.trt + CO2trt, xlab="Salinity by TRT Interaction", ylab="Log C3 Prod", main="Left 3 bars are Ambient CO2 and Right 3 Elevated CO2")
interaction.plot(salt.trt, CO2trt, C3prod, type = c("l", "p", "b", "o", "c"))

### Standard analyses
summary(lm(C3prod ~ Salinity * CO2))
summary(lm(ln.C3prod ~ Salinity * CO2))


### lavaan modeling
library(lavaan)
dat2 <- data.frame(ln.C3prod, CO2, Salinity, CxS)

## Model 1

# specify model
mod.1 <- 'ln.C3prod ~ CO2 + Salinity + CxS' 

# fit model
mod.1.fit <- sem(mod.1, data=dat2)

# request output
summary(mod.1.fit, rsq=T)

## Model 2

# specify model (using est from model 1)
mod.2 <- 'Comp <~ CO2 + 1*Salinity + CxS
          ln.C3prod ~ Comp' 

# fit model
mod.2.fit <- sem(mod.2, data=dat2)

# request output
summary(mod.2.fit, standardized=T, rsq=T)
