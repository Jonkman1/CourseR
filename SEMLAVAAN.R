# Sem with Lavaan

# First example
# load the lavaan package (only needed once per session)
library(lavaan)
# specify the model
HS.model <- ' visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed =~ x7 + x8 + x9 '
# fit the model
fit <- cfa(HS.model, data=HolzingerSwineford1939)
# display summary output
summary(fit, fit.measures=TRUE)

# Second example
library(lavaan) # only needed once per session
model <- '
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit <- sem(model, data=PoliticalDemocracy)
summary(fit, standardized=TRUE) 
