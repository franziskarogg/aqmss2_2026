### Assignment 9.2 - Franziska Rogg 
### Survival Analysis

library(carData)
library(MASS)
library(nnet)
library(pscl)
library(AER)
library(marginaleffects)
library(tidyverse)
library(survival)
library(broom)
library(ggplot2)
lung = lung
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment9')


#We now analyze time-to-event data using the lung dataset from the survival package, which contains survival times for patients with advanced lung cancer from the North Cen- tral Cancer Treatment Group. 
#The outcome of interest is time from enrollment to death. 
#Some patients are still alive at the end of the study — they are right-censored. 
#Recall from class that ignoring censoring (e.g., dropping censored observations or treating them as events) produces biased estimates, which motivates the use of survival models.

#-------------------------------------------------------------------------------
##1. Kaplan-Meier Survival curves
#-------------------------------------------------------------------------------

lung$dead = lung$status -1

#a) 
summary(lung)
nrow(lung) #total number of observations: 228
table(lung$dead) #63 censored, 165 dead (number of deaths)
63/228 #=> 27.6% are censored 
#This means that almost 28% percent live longer than the study times, however it does not mean that this is the survival rate!


#b) Estimation of overall Kaplan-Meier survival curve
m_surv = survfit(Surv(time, dead) ~ 1, data = lung)
summary(m_surv) #The summary() output shows, at each event time, the number at risk, the number of events, the estimated survival probability, and the confidence interval. In a comment, report the estimated median survival tim
m_surv

#Median survival time: 310
#This number means that 50% of the patients died by day 310


#c) 
m_surv_sex = survfit(Surv(time, dead) ~ sex, data = lung)
summary(m_surv_sex)
surv_sex_df = tidy(m_surv_sex)

#Plot
ggplot(surv_sex_df, aes(x = time, y = estimate, color = strata, fill = strata)) +
  geom_step() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Kaplan-Meier Survival Curves by Sex",
       x = "Time (days)",
       y = "Survival Probability",
       color = "Sex", fill = "Sex") +
  scale_color_manual(values = c("sex=1" = "blue", "sex=2" = "red"),
                     labels = c("Male", "Female")) +
  scale_fill_manual(values = c("sex=1" = "blue", "sex=2" = "red"),
                    labels = c("Male", "Female")) +
  theme_minimal()

ggsave("surv_by_sex.pdf", width = 6, height = 4)


#Log-rank test
survdiff(Surv(time, dead) ~ sex, data = lung)

#The log-rank test tests whether men and women die at the same rate over time or is it different for one group.
#The p-value of the log-rank test is p = 0.001, which means it is significant
#Survival curves between men and women are statistically significantly different. 
#Females survive significantly longer than males. Males experienced more deaths than expected (112 vs 91.6) while females experienced fewer (53 vs 73.4). 



#-------------------------------------------------------------------------------
##2. Cox proportional hazards model
#-------------------------------------------------------------------------------

#a) Fitting cox proportional hazard model 
m_coxph = coxph(Surv(time, dead) ~ age + sex + ph.ecog, data = lung)
summary(m_coxph)

#The output shows both raw coefficients (log-hazard scale) and exponentiated coefficients (hazard ratios).
#Recall from class: a hazard ratio below 1 means lower hazard (longer survival), above 1 means higher hazard (shorter survival). 
#What does the hazard ratio for sex tell us about survival differences between men and women? Is it statistically significant?

#Hazard ratio (exponentiated coefficient) for sex:  0.575445 
#For a one-unit increase of sex, therefore for women, the hazard ratio is below 1 meaning lower hazard and therefore longer survival!
#The p-value(<0.001) shows that this is statistically significant. 

#b) Interpretation of the hazard ratio for ph.ecog
#Hazard ratio ph.ecog: 1.589991 > 1 => higher hazard and shorter survival 
#For one-unit increase in this variable, meaning the more activity/performance decreases toward being fully bedridden, is associated with a 59% higher hazard of death!

#c) The Cox model assumes proportional hazards: the effect of each covariate is constant over time
#To test this assumption: 

cox.zph(m_coxph)
#age      0.66
#sex      0.13
#ph.ecog  0.15
#GLOBAL   0.22

#All p-values are not significant and therefore none of the covariates violates the assumption of proportional hazard. 
#This means that all effects can be assumed to be constant over time, the effect of e.g. age does not increase/decrease over time. 

#d) 
#The Kaplan-Meier analysis did suggest that looking at both sexes separately does make sense since the survival curves are statistically significantly different. 
#Significant predictors in the cox model: age and ph.ecog, both positive; sex is not significant
#The proportional hazard assumption holds, as the high p-values fo the cox.zph test show. None of the covariate's effect diminishes/increases over time. 



