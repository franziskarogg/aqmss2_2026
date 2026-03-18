###Assignment 6.1 - Franziska Rogg 
###Card-Krueger Minimum Wage - canonical difference-in-differences 

#The setup:
#2 groups: a treatment group and a control group
#2 time periods: one pre-treatment and one post-treatment
#1 treatment date: everyone in the treatment group gets treated at the same time

#You difference out:
#Within treated group — removes time-invariant characteristics of treated units
#Within control group — removes common time trends
#The difference of those differences — isolates the treatment effect



library(dplyr)
library(tidyr)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(fixest )
library(readstata13)
library(plm)
library(did)
library(etwfe)

setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment6')
minwage = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")
df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")

##1. Data setup and exploration
head(df) #this is wide => #each row is one restaurant
#In wide data, each unit appears once, and different time periods are stored in separate columns.
#In long data, each unit appears multiple times, once for each time period. => Each row = one observation in one time period

#Most panel-data and DiD methods require long data => each row is one unit-time observation.

#a) NJ dummy and averages
df = df %>% 
  mutate(NJ = ifelse(location != "PA", 1, 0)) 
#we find a treatment group here: not PA  = treatment => NJ, if it is PA then = > 0 
#setting up the treated state which is location = 1 => NJ
#the ! means: "not equal to"

table(df$NJ)
#Number of restaurants in NJ(=1 ) => 291 => contains several regions within NJ
#Number of restaurants in PA (=0) => 67 => PA is treated as a single region here

df %>% 
  group_by(NJ) %>% 
  summarize(
    mean_wageBefore = mean(wageBefore, na.rm = TRUE), 
    mean_wageAfter = mean(wageAfter, na.rm = TRUE)
  )

#Before the policy change, the averages of NJ restaurants and PA ones are very similar, while after the policy change, they differ more significantly. 
#This suggests that the policy on average change did raise wages in the treated state, as the average is higher than the average before the policy, and also higher than the control group after the policy. 

#b) Manual DiD - Full time employment
means = df %>% 
  group_by(NJ) %>% 
  summarize(
    before = mean(fullBefore, na.rm = TRUE), #trend over time for before in both groups
    after = mean(fullAfter, na.rm = TRUE), #trend over time after both groups
    change = after-before) #change in trend - both groups
means

nj_change = means$change[means$NJ == 1] #this shows the treatment effect within group NJ
pa_change = means$change[means$NJ == 0] #treatment effect within group PA
did_est_manual = nj_change - pa_change #difference in within group difference
did_est_manual
cat("Did estimate:", round(did_est_manual, 3), "\n")

#The DiD estimate is the difference in within-group changes. 
#A positive value means full-time employment grew more (or fell less) in NJ than in PA after the minimum wage increase, 
#which contradicts the standard prediction that higher minimum wages reduce employment.




#c) Reshape to long format for regression
#wide only needed for plots mostly - long better for model
df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ   = ifelse(location != "PA", 1, 0))
nrow(df_long)


##2. DiD regression

#a)Basic Did regression
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")
#post = effect of time when treatment is 0 => effect of time on control unit "counterfactual", based on assumption though! this has to be justified then in e.g theory
#nJ = unit effect
#post x NJ = effect of predictor => did

#if we do it with the normal model it would look like this (not using the feols)
m0 = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
m1 = lm(full_emp ~ post * NJ, data = df_long)
m2 = feols(full_emp ~ treated | id + post, data = df_long) #we dont have variable treated
m3 = lm(full_emp ~ treated + factor(id) + factor(post), data = df_long)

#Answer from Solution: 
#The coefficient on post:NJ is the DiD estimator and should match the manual calculation from 1.1b. The post coefficient captures the pre–post change in PA (the counterfactual trend), the NJ coefficient captures the baseline NJ–PA gap, and the interaction captures the additional change in NJ relative to that trend.

#b) Adding chain fixed effects
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"),
  output = "markdown")


#Answer from Solution: 
#The DiD estimate does not change when chain fixed effects are added. Chain FEs absorb baseline differences in staffing levels across fast-food chains (e.g., Wendy’s may have structurally different employment levels than KFC), but since chain type is roughly balanced across states, controlling for it has little impact on the DiD coefficient.


#c) 
#Answer from Solution: 
#The parallel trends assumption here requires that, absent the NJ minimum wage increase, employment trends in
#NJ and PA fast-food restaurants would have been the same from February to November 1992. This is plausible
#because both states share a similar economic environment and the two surveys were close together in time, limiting
#opportunities for diverging trends. A concrete violation would occur if NJ experienced an independent economic
#shock during this period — for instance, if a major employer opened or closed plants in NJ between the two survey
#waves, this would change NJ employment for reasons unrelated to the minimum wage, biasing the DiD estimate.



##3. Wages as a validation check

#a) DiD for wages(first-stage check): 
df_long_wage = df %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(
    cols = c(wageBefore, wageAfter), 
    names_to = "period", 
    values_to = "wage") %>% 
  mutate(
    post = ifelse(period == "wageAfter", 1, 0), 
    NJ = ifelse(location != "PA", 1, 0))

m_wage = feols(wage ~ post *NJ, data = df_long_wage, cluster =~id)
modelsummary(m_wage, stars = TRUE, gof_map = c("nobs", "r.squared"), 
             output = "markdown")

#Answer from Solution: 
#The interaction coefficient post:NJ is positive and statistically significant: wages rose substantially in NJ relative to PA after the policy change, and the magnitude is consistent with the $0.80 minimum wage increase ($5.05 - $4.25). This is precisely the sign and magnitude one would expect if the law was actually binding.

#b) 
#Answer from Solution: 
#The wage DiD serves as a “first stage” or manipulation check. If wages had not risen in NJ after the minimum
#wage increase, it would be unclear whether the study is truly estimating the effect of a minimum wage change at all — the law might not have been binding, or firms might have already been paying above the new minimum. The fact that wages did rise in NJ gives us confidence that the treatment actually occurred as intended, so the employment
#DiD can be credibly interpreted as a causal response to the minimum wage increase rather than a spurious or null comparison.
