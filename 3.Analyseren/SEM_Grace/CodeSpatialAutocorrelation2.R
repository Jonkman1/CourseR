### SEM.Sp1.Exercise-Rcode - Exercise to accompany SEM.Sp1-Spatial Autocorrelation

# Exercise relates to Harrison and Grace (2007) American Naturalist 170, pp. S5–S15.

### Set directory
setwd("G:/SEM/Grace")

### Read and check data
exdat <- read.csv("SEM.Sp1-Spatial Autocorrelation Lavaan Exercise_data.csv")
names(exdat)
summary(exdat)
dim(exdat)
attach(exdat)

### Load needed libraries and functions
library(lavaan)
library(ape) 
source("lavSpatialCorrect.R") 

### lavaan modeling
# Specify model
ex.mod <- 'coverNT ~ regNDVI
           propMT ~ regNDVI + coverNT
           propCFP ~ regNDVI + coverNT
           propWTD ~ regNDVI + 0*coverNT'

# Fit model
ex.mod.fit <- sem(ex.mod, exdat, meanstructure=T)

# Examine model with uncorrected parameters
summary(ex.mod.fit, rsq=T, standardized=T)

### Correct for spatial autocorrelation
# Execute correction function
lavSpatialCorrect(ex.mod.fit, lat, long)

