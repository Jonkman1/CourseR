### SEM.Sp1-Rcode - SPATIAL AUTOCORRELATION ILLUSTRATION WITH MATTISON ET AL (2012) DATA
### from Matteson, K.C., Grace, J.B., and Minor, E.S. 2012.
#   Direct and indirect effects of land use on floral resources and flower-visiting insects across an urban landscape.
#   Oikos 122:682-694. (http://onlinelibrary.wiley.com/doi/10.1111/j.1600-0706.2012.20229.x/abstract)
#   last modified 15.01.31

### Set directory
setwd("G:/SEM/Grace")

### Read data
dat <- read.csv("SEM.SP1-Spatial Autocorrelation Lavaan Procedures_data.csv")
names(dat)
summary(dat)
dim(dat)

### Rename some variables
x       <- dat$N.point.X
y       <- dat$N.point.Y
dev     <- dat$N.developmentLVs
veg     <- dat$N.vegLVs
floral  <- dat$N.floralLVs
insects <- dat$N.pollinatorsLVs

sem.dat <- data.frame(x, y, dev, veg, floral, insects)

### Load needed library
library(lavaan)

### lavaan modeling
# Specify model
neigh.mod <- 'floral ~ dev + veg
              insects ~ veg + floral'

# Fit model
neigh.mod.fit <- sem(neigh.mod, sem.dat, meanstructure=T)

# Examine model with uncorrected parameters
summary(neigh.mod.fit, rsq=T)

### Correct for spatial autocorrelation
source("lavSpatialCorrect.R") # access the function
library(ape) # load supporting library

# Execute correction function
lavSpatialCorrect(neigh.mod.fit, x, y)

