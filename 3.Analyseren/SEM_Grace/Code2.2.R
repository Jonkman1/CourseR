################## SEM.2.2-Local Estimation Rscript #########
# Set working directory and load data
setwd("G:/SEM/Grace") #commands in bold
dat <- read.csv("SEM.2.2-Local Estimation data.csv")
attach(dat)

## Recode vars to roughly same scale
x1 <- x1/100
y1 <- y1/100
y2 <- y2
y3 <- y3/100
t.dat <- data.frame(x1, y1, y2, y3)

# Specify model
y1.mod <- lm(y1 ~ x1, data=t.dat)
y2.mod <- lm(y2 ~ x1, data=t.dat)
y3.mod <- lm(y3 ~ y1 + y2, data=t.dat)

# Capture residuals
x1.res <- x1
y1.res <- resid(y1.mod)
y2.res <- resid(y2.mod)
y3.res <- resid(y3.mod)

# Look at conditional independences
dev.new(height=4, width=8); par(mfrow=c(1,2))
plot(y3.res ~ x1.res, pch=16); cor.test(y3.res, x1.res); abline(lm(y3.res ~ x1.res), lwd=2)
plot(y2.res ~ y1.res, pch=16); cor.test(y2.res, y1.res)

# Respecify model
y1.mod <- lm(y1 ~ x1, data=t.dat)
y2.mod <- lm(y2 ~ x1, data=t.dat)
y3.mod2 <- lm(y3 ~ y1 + y2 + x1, data=t.dat)

# Examine revised model parameters
summary(y1.mod)
summary(y2.mod)
summary(y3.mod2)

### Illustrate evaluation of y3 submodel: Compare revised model against simpler models
# Create suite of alternative models
y3.mod2 <- lm(y3 ~ y1 + y2 + x1, data=t.dat)
y3.mod1 <- lm(y3 ~ y1 + y2,      data=t.dat)
y3.mod3 <- lm(y3 ~ y1 + x1,      data=t.dat)
y3.mod4 <- lm(y3 ~ y2 + x1,      data=t.dat)

# Compare using AICc
library(AICcmodavg)
aictab(list(y3.mod2,y3.mod1,y3.mod3,y3.mod4), modnames=c("y3.mod2","y3.mod1","y3.mod3","y3.mod4"))

### Use ggm to run Shipley test
library(ggm)
cov.dat <- as.matrix(cov(t.dat))
dag <- DAG(y1 ~ x1, y2 ~ x1, y3 ~ y1 + y2) 
shipley.test(dag, S=cov.dat, n=90)

### Lefcheck's piecewiseSEM procedures
library(piecewiseSEM)

# Original Model
modlist1 = list(
  lm(y1 ~ x1, data=t.dat),
  lm(y2 ~ x1, data=t.dat),
  lm(y3 ~ y1 + y2, data=t.dat))
# derive the fit statistics:
get.sem.fit(modlist1, t.dat)

# Revised Model
modlist2 = list(
  lm(y1 ~ x1, data=t.dat),
  lm(y2 ~ x1, data=t.dat),
  lm(y3 ~ y1 + y2 + x1, data=t.dat)
  )
# derive the fit statistics:
  get.sem.fit(modlist2, t.dat)
