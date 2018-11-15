# Chapter 9
# Bayesian multilevel analysis
library(MCMCglmm)

# Data invoeren
library(readr)
PrimeTime <- read_csv("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/PrimeTime.csv")
prime_time.nomiss<-na.omit(PrimeTime)
attach(prime_time.nomiss)
# Eerste model
model9.1<-MCMCglmm(geread~gevocab, random = ~school, data=prime_time.nomiss)
plot(model9.1)
#autocorrelation random effects)
autocorr(model9.1$VCV)
#autocorrelation fixed effects)
autocorr(model9.1$Sol)
summary(model9.1)

# change defaults of iterations, thinning and burnin
model9.1b<-MCMCglmm(geread~gevocab, random = ~school, 
                    data=prime_time.nomiss, nitt=100000, thin =50, burnin = 10000)
plot(model9.1b)
summary(model9.1b)

# Including Level 2 Predictors
model9.2<-MCMCglmm(geread~gevocab + senroll, random = ~school, data=prime_time.nomiss)
plot(model9.2)
summary(model9.2)
#autocorrelation random effects)
autocorr(model9.2$VCV)
#autocorrelation fixed effects)
autocorr(model9.2$Sol)

model9.3<-MCMCglmm(geread~gevocab, random = ~school, 
                    data=prime_time.nomiss, nitt=40000, thin =100, burnin = 3000)
plot(model9.3)
summary(model9.3)
#autocorrelation random effects)
autocorr(model9.3$VCV)
#autocorrelation fixed effects)
autocorr(model9.3$Sol)

# Relationship of vocabulary score and reading achievement vary across schools
model9.4<-MCMCglmm(geread~gevocab, random = ~school + gevocab, data=prime_time.nomiss)
plot(model9.4)
summary(model9.4)
#autocorrelation random effects)
autocorr(model9.4$VCV)
#autocorrelation fixed effects)
autocorr(model9.4$Sol)

# Using information: defining priors 
# var<-matrix(c(1,0,0,0.1), nrow = 2, ncol = 2)
# prior.model9.5<-list(B=list(mu=c(0,.15), V = var))
# model9.5<-MCMCglmm(geread~npamem, random = ~school, 
#                   data = prime_time.nomiss, prior = prior.model9.5)
# plot(model9.5)
#autocorrelation random effects)
# autocorr(model9.5$VCV)
#autocorrelation fixed effects)
# autocorr(model9.5$Sol)
# summary(model9.5)

# Dichotome afhankelijke variabele
# library(readr)
# MathFinal <- read_csv("H:/MLMR/MathFinal.csv")
# View(MathFinal)
# model9.6<-MCMCglmm(score2~numsense, random = ~school, family = "categorical", data = MathFinal)
# plot(model9.6)
#autocorrelation random effects)
# autocorr(model9.6$VCV)
#autocorrelation fixed effects)
# autocorr(model9.6$Sol)
# summary(model9.6)

# Count variable
library(readr)
Rehab <- read_csv("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/Rehab.csv")
View(Rehab)
attach(Rehab)
model9.7<-MCMCglmm(heart~trt+sex, random = ~rehab, 
          family = "poisson", data=Rehab)
plot(model9.7)
#autocorrelation random effects)
autocorr(model9.7$VCV)
#autocorrelation fixed effects)
autocorr(model9.7$Sol)
summary(model9.7)

model9.8<-MCMCglmm(heart~trt+sex + hours, random = ~rehab + trt, 
                   family = "poisson", data=Rehab)
plot(model9.8)
#autocorrelation random effects)
autocorr(model9.8$VCV)
#autocorrelation fixed effects)
autocorr(model9.8$Sol)
summary(model9.7)
