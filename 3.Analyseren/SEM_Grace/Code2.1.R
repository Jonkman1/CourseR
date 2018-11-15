################## THE 4-VARIABLE INTRO TO LAVAAN EXAMPLE #########
### LAST UPDATED 15.03.26
# Set working directory and load data
setwd("G:/SEM/Grace") #commands in bold
dat <- read.csv("SEM.2.1-Intro to Lavaan data.csv")

# Examine 
summary(dat)

##### Lavaan modeling
library(lavaan)

# Step 1: Specify model
mod.1 <- 'y1 ~ x1
          y2 ~ x1
          y3 ~ y1 + y2'

# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=dat)

# Check variances
varTable(mod.1.fit)

## Recode vars to roughly same scale
x1 <- dat$x1/100
y1 <- dat$y1/100
y2 <- dat$y2
y3 <- dat$y3/100

### Create Transformed Dataset
# overwrite file with recoded data
t.dat <- data.frame(x1, y1, y2, y3)
summary(t.dat)

##### Repeat Lavaan modeling

# Step 1: Specify model
mod.1 <- 'y1 ~ x1
          y2 ~ x1
          y3 ~ y1 + y2'

# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=t.dat)

# Step 3: Extract results
summary(mod.1.fit) 
