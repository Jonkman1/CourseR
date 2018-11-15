##### Two-level data example
### Note, there are 8,069 segments nested within 112 subestuaries, so segments = lower level; subestuaries = upper level
### The variable "sav" (key response) is measured at lower level (n=8,069)
### The variable "pctCrop" (continuous predictor) is measured at upper level (n=112)
### The variable "riprap" (dichotomous predictor) is measured at the lower level (n=8,069)

# set working directory
setwd("G:/SEM/Grace")

##### Read in data
sav1.dat <- read.csv("SEM.NestedData_code.csv",header=T)
names(sav1.dat)
attach(sav1.dat)

# load libraries
library(lavaan)
library(lavaan.survey)

### Simple lavaan model
# Step 1: Specify model equations
savmod <- 'sav ~ crop'

# Step 2: Estimate model using the ‘sem’ function
savmod.fit <- sem(savmod, data=sav1.dat, meanstructure=T,)

# View unadjusted results
summary(savmod.fit, rsq=T)

### Adjusting for sample design
# Step 3: Describing the sample structure using "svydesign"
design1 <- svydesign(ids = ~subest, nest=TRUE, data=sav1.dat)

# Step 4: Post-processing original lavaan object
savmod.adj <- lavaan.survey(lavaan.fit = savmod.fit, survey.design = design1)

# Step 5: Get summary of adjusted results
summary(savmod.adj, rsq=T)

############## NOTE: if we had a more complex model and needed modification indices, here is some useful code 
# Extract only chi-square for adjusted model
savmod.adj

# Get select modification indices
mi <- modindices(survey.fitp1) # capture all mod indices in object "mi"
print(mi[mi$op == "~",])       # print out only directed path results
print(mi[mi$op == "~~",])      # print out only correlations

# Get residuals from adjusted object
resid(survey.fitp1, type="standardized")
