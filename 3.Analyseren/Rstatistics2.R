##                         ________________________

##                          REGRESSION MODELS IN R
##                         ________________________

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

# read the states data
states.data <- readRDS("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/dataSets/states.rds") 
# states.data <- read_dta("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/dataSets/states.dta")
                     
View(states.data)


# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
View(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat) 

## Plot the data before fitting models
plot(sts.ex.sat)

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
##   - Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   - Use function methods to get more information about the fit

confint(sat.mod)

## Linear Regression Assumptions
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))


## Interactions and factors
## ========================

sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table


# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))


## Regression with binary outcomes
## ===============================

## Logistic regression

NH11 <- readRDS("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

## [CDC website] http://www.cdc.gov/nchs/nhis.htm


str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))


library(effects)
plot(allEffects(hyp.out))


## Multilevel Modeling
## ===================


library(lme4)


##    variable  Description                                                                                        
##   --------------------------------------------------------------------------------------------------------------
##    school    School ID - a factor.                                                                              
##    normexam  Normalized exam score.                                                                             
##    schgend   School gender - a factor.  Levels are 'mixed', 'boys', and 'girls'.                                
##    schavg    School average of intake score.                                                                    
##    vr        Student level Verbal Reasoning (VR) score band at intake - 'bottom 25%', 'mid 50%', and 'top 25%'. 
##    intake    Band of student's intake score - a factor.  Levels are 'bottom 25%', 'mid 50%' and 'top 25%'./     
##    standLRT  Standardised LR test score.                                                                        
##    sex       Sex of the student - levels are 'F' and 'M'.                                                       
##    type      School type - levels are 'Mxd' and 'Sngl'.                                                         
##    student   Student id (within school) - a factor                                                              

Exam <- readRDS("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/dataSets/Exam.rds")


# null model, grouping by school but not fixed effects.
Norm1 <-lmer(normexam ~ 1 + (1|school),
              data=Exam, REML = FALSE)
summary(Norm1)


##   Predict exam scores from student's standardized tests scores

Norm2 <-lmer(normexam~standLRT + (1|school),
             data=Exam,
             REML = FALSE) 
summary(Norm2) 


anova(Norm1, Norm2)

## Random slopes

Norm3 <- lmer(normexam~standLRT + (standLRT|school), data=Exam,
               REML = FALSE) 
summary(Norm3) 


anova(Norm2, Norm3) 


##   Use the /states.rds/ data set.
states <- readRDS("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/dataSets/states.rds")

##   1. Examine/plot the data before fitting the model
states.en.met <- subset(states, select = c("metro", "energy"))
summary(states.en.met)
plot(states.en.met)
cor(states.en.met, use="pairwise")

##   2. Print and interpret the model `summary'
mod.en.met <- lm(energy ~ metro, data = states)
summary(mod.en.met)

##   3. `plot' the model to look for deviations from modeling assumptions
plot(mod.en.met)




##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
mod.en.metro.by.waste <- lm(energy ~ metro * waste, data = states)

##   1. Try adding a region to the model. Are there significant differences
##      across the four regions?
mod.en.region <- lm(energy ~ metro * waste + region, data = states)
anova(mod.en.region)

## Exercise 2 prototype

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
NH11 <- readRDS("C:/Users/HarrieJonkman/OneDrive - Verwey-Jonker Instituut/Rcursus/3.Analyseren/dataSets/NatHealth2011.rds")

nh11.wrk.age.mar <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
summary(nh11.wrk.age.mar)
NH11 <- transform(NH11,
                  everwrk = factor(everwrk,
                      levels = c("1 Yes", "2 No")),
                  r_maritl = droplevels(r_maritl))

mod.wk.age.mar <- glm(everwrk ~ age_p + r_maritl, data = NH11,
                      family = "binomial")

summary(mod.wk.age.mar)

##   2. Predict the probability of working for each level of marital
##      status.
library(effects)
data.frame(Effect("r_maritl", mod.wk.age.mar))


##   Use the dataset, bh1996:
library(multilevel)
data(bh1996, package="multilevel")

##   1. Create a null model predicting wellbeing ("WBEING")
library(lme4)
mod.grp0 <- lmer(WBEING ~ 1 + (1 | GRP), data = bh1996)
summary(mod.grp0)

##   3. Run a second multi-level model that adds two individual-level
##      predictors, average number of hours worked ("HRS") and leadership
##      skills ("LEAD") to the model and interpret your output.
mod.grp1 <- lmer(WBEING ~ HRS + LEAD + (1 | GRP), data = bh1996)
summary(mod.grp1)

##   4. Now, add a random effect of average number of hours worked ("HRS")
##      to the model and interpret your output. Test the significance of
##      this random term.
mod.grp2 <- lmer(WBEING ~ HRS + LEAD + (1 + HRS | GRP), data = bh1996)
anova(mod.grp1, mod.grp2)


######

