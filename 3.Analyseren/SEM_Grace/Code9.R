################## MODELING WITH LATENT VARIABLES #########
### last updated 15.07.27

# load library
library(lavaan)

##### Illustrating how to specify measurement error in a model in lavaan
# use covariance matrix as input data
mod1.dat <- '
  3.14
  1.064  0.999'
mod1.cov <- getCov(mod1.dat, names=c("x", "y"))
print(mod1.cov)
print(cov2cor(mod1.cov)) # examine correlation matrix

### lv model without error specified
lv.mod1 <- '
   # declare latent variables
      xi =~ x     
      eta =~ y

   # declare latent regression
      eta ~ xi'

# fit model
lv.fit1 <- sem(lv.mod1, sample.cov= mod1.cov, sample.nobs= 15)
summary(lv.fit1, rsq=T, standardized=T)

### lv model with error specified
lv.mod2 <- '
   # declare latent variables
      xi =~ x     
      eta =~ y

   # declare latent regression
      eta ~ xi

   # specifying error variance for x
      x ~~ 0.597*x'

# fit model
lv.fit2 <- sem(lv.mod2, sample.cov= mod1.cov, sample.nobs= 15)
summary(lv.fit2, rsq=T, standardized=T)


##### Specifying and testing "confirmatory factor model" (CFA)
# load data - for this example and the next
setwd("F:/ppt_files/_education/Code-Data-Materials/Posted on Website")
perf.dat <- read.csv("SEM.9.1-Modeling with Latent Variables-data.csv")
attach(perf.dat)
names(perf.dat)

# initial model
lvmod.1 <- ' 
     # Latent variable definition
       Perform=~ stems + infls + clonediam
               + leafht + leafwdth'

# fit model (note we get a warning, but it is not a fatal error)
lvmod.1.fit <- sem(lvmod.1, data=perf.dat)
summary(lvmod.1.fit, rsq=T, standardized=T)

# request modification indices (several choices here)
modindices(lvmod.1.fit)  #this gives us everything
mi <- modindices(lvmod.1.fit) #create index object
print(mi[mi$op == "~",])   #request only ~ links
print(mi[mi$op == "~~",])  #request only ~~ links
print(mi1[([mi1$mi > 3.0,] & [!(mi1$mi=="<NA>"),])]) # request only large ones

# modified CFA model with error covariance added
lvmod.2 <- ' # Latent variable definition
               Perform=~ stems + infls + clonediam
                        + leafht + leafwdth

             # Error Covariances
               leafht ~~ leafwdth'

# fit model
lvmod.2.fit <- sem(lvmod.2, data=perf.dat)
summary(lvmod.2.fit, rsq=T, standardized=T)

##### Putting performance into context in the full model
# specify model
lvmod.3 <- ' # Latent variable definition
               Perform=~ stems + infls + clonediam
                        + leafht + leafwdth

             # Error Covariances
               leafht ~~ leafwdth

             # Regressions
               Perform ~ geneticdist
               leafht ~ latitude
               leafwdth ~ latitude'

# fit model
lvmod.3.fit <- sem(lvmod.3, data=perf.dat)
summary(lvmod.3.fit, rsq=T, standardized=T)


