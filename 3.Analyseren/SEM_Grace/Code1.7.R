##### INTERPRETING THE EFFECTS OF CATEGORICAL PREDICTORS
### Data from Cherry, J.A., McKee, K.L., and Grace, J.B. 2009. Journal of Ecology 97:67-77.

setwd("G:/SEM/Grace")

dat <- read.csv("SEM.1.7-SEM Essentials Categorical Predictors_data.csv")
names(dat)
head(dat)
attach(dat)

### Examine net difference
dev.new(height=4, width=4); boxplot(ElevChange ~ CO2, col="grey", ylab="Elevation Change", xlab="CO2 Treatment")

### lavaan modeling
library(lavaan)
# specify model
mod <- 'ElevChange ~ CO2'

# fit model
mod.fit <- sem(mod, data=dat)

# request summaries
summary(mod.fit, rsq=T, standardized=T)
standardizedSolution(mod.fit, type = "std.all") # extra information
standardizedSolution(mod.fit, type = "std.nox") # extra information

### Compute standardized coefficient by hand
est = 5.280
sd.elev <- sd(ElevChange)
sd.co2  <- sd(CO2)

std.all <- est*(sd.co2/sd.elev)
print(std.all)

### What is the sd of ElevChange?
print(sd.elev)
hist(ElevChange, col="grey")  # extra information

### What is sd of CO2?
print(sd.co2)

### What if we had a categorical variable with unequal numbers of 0s and 1s?
new.cat <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1)
print(sd(new.cat))

### Range standardization
range.elev <- max(ElevChange) - min(ElevChange)
print(range.elev)
range.co2  = 1

std.range <- est*(range.co2/range.elev)
print(std.range)

# We can visualize the range of values for ElevChange
plot(ElevChange, pch=16)
