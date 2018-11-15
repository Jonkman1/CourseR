##### SEM.10.1 - Composites and Formative Indicators - version 1.1

### Simulate some data for an example
library(MASS)
mu = c(10, 20)
Sigma <- matrix(c(10, 3, 3, 2), 2, 2)
set.seed(1, kind = NULL, normal.kind = NULL)
xs <- mvrnorm(n = 50, mu, Sigma)
cause1 = xs[,1]
cause2 = xs[,2]
set.seed(3, kind = NULL, normal.kind = NULL)
yerror = rnorm(50, mean=0, sd=2)
response <- 0.812*cause1 + 0.673*cause2 + yerror
sim.dat <- data.frame(cause1, cause2, response)
cor(sim.dat)


## lavaan model
library(lavaan)

# model without composite (typically as a first step)
mod.1 <- 'response ~ cause1 + cause2'
mod.1.fit <- sem(mod.1, data=sim.dat)
summary(mod.1.fit, rsq= T, standardized=T)

# lavaan model with composite - using coefficient estimate from uncomposited model above
Cmod.1 <- 'Comp <~ 0.838*cause1 + cause2
           response ~ Comp'
Cmod.1.fit <- sem(Cmod.1, data=sim.dat)
summary(Cmod.1.fit, rsq=T, standardized=T)
                                                                                        
# model with composite using coef from non-composited model
Cmod.1alt <- 'Comp <~ 0.838*cause1 + cause2
              response ~ Comp'
Cmod.1alt.fit <- sem(Cmod.1alt, data=sim.dat)
summary(Cmod.1alt.fit, rsq=T, standardized=T)
                                         
# compute composite scores by hand ignoring intercept
Comp.a <- 0.838*cause1 + 0.590*cause2
# create new data set
dat2 <- data.frame(cause1, cause2, response, Comp.a)

# model direct effect of composite on response
Cmod.2 <- 'response ~ Comp.a'
Cmod.2.fit <- sem(Cmod.2, data=dat2)
summary(Cmod.2.fit, rsq=T, standardized=T)

# Bonus - one more specification of composite that gives same result
Cmod.3 <- 'Comp.a ~ 0.838*cause1 + cause2
           response ~ Comp.a'        
Cmod.3.fit <- sem(Cmod.2, data=dat2)
summary(Cmod.3.fit, rsq=T, standardized=T)



