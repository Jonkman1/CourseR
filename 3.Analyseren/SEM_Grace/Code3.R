####### SEM.3-MODEL EVALUATION

##### WORKING FROM SEM.2.1 MATERIAL
# Set working directory and load data
setwd("G:/SEM/Grace") #commands in bold
dat <- read.csv("SEM.3-Model Evaluation data.csv")
attach(dat)

##### Lavaan modeling
library(lavaan)

# Step 1: Specify model
mod.1 <- 'y1 ~ x1
          y2 ~ x1
          y3 ~ y1 + y2'

# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=dat)

# Oops, problem - request "varTable(fit)" to diagnosis
varTable(mod.1.fit)

### Recode vars to roughly same scale
x1 <- x1/10
y1 <- y1/10
y2 <- y2*10
y3 <- y3/100
t.dat <- data.frame(x1, y1, y2, y3)  # overwrite file with recoded data

# Repeat Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=t.dat)

# Step 3: Extract results
summary(mod.1.fit) 

##### SEM.3 MATERIAL
fitMeasures(mod.1.fit)
summary(mod.1.fit, fit.measures=T)

### Request Modification Indices
summary(mod.1.fit, modindices=TRUE)


### MODEL 2  
#Step 1: Specify model
mod.2 <- 'y1 ~ x1
         y2 ~ x1
         y3 ~ y1 + y2 + x1'

# Step 2: Estimate model
mod.2.fit <- sem(mod.2, data=t.dat)

# Step 3: Extract results
summary(mod.2.fit)     # standard stuff

# Compare models 1 and 2
anova(mod.1.fit, mod.2.fit)

# What about other fit measures?
fitMeasures(mod.2.fit) # ask for all
AIC(mod.2.fit)


### MODEL 3  
#Step 1: Specify model
mod.3 <- 'y1 ~ x1
         y2 ~ x1
         y3 ~ y1 + y2 + x1
         y1 ~~ y2'

# Step 2: Estimate model
mod.3.fit <- sem(mod.3, data=t.dat)

# Step 3: Extract results
summary(mod.3.fit)     # standard stuff

##### Sample-size corrected AIC
# get "http://jarrettbyrnes.info/ubc_sem/lavaan_materials/lavaan.modavg.R"
library(AICcmodavg)   # need lavaan.modavg.R package 
source("lavaan_modavg.R")

aictab.lavaan(list(mod.1.fit, mod.2.fit, mod.3.fit), c("Model1", "Model2", "Model3"))

### More output for selected model
summary(mod.2.fit, rsq=T, standardized=T)
standardizedSolution(mod.2.fit, type = "std.all")
