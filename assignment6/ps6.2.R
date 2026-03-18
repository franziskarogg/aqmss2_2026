###Assignment 6.2 - Franziska Rogg 
###Staggered DiD - Teen employment (Callaway-Santa ́nna)

#Now we have different times of treatment adoption ≠ the canonical did

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
data(mpdta)
df = mpdta

##1. Data structure & visualization
#a)
head(df)
n_distinct(df$first.treat) #There are 4 unique treatment cohorts
length(unique(df$countyreal)) #There are 500 counties in the data
table(mpdta$first.treat)

#Staggered treatment adoption: Why is it a problem to simply compare treated vs. untreated counties? 
#As the counties receive treatment at different times, meaning that they adopt the policies in different years. 
#Simply comparing treated vs. untreated would risk that you might include already-treated units as a. control for units that receive treatment later => forbidden comparison
#this might make the estimation with TWFE invalid in this staggered setting. 


#b) Plot: Average "lemp" over years for each treatment cohort
mpdta_avg = mpdta %>% 
  mutate(cohort = factor(first.treat, 
                         levels = c(0, 2004, 2006, 2007), 
                         labels = c("Never treated", "Adopted 2004", "Adopted 2006", "Adopted 2007"))) %>% 
  group_by(year, cohort) %>% 
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

p1 = ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) + 
  geom_line() + 
  geom_point() + 
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")
p1
ggsave("p1_avg_lemp_years.png", p1, width = 6, height = 4)

##2. Naive TWFE vs. Callaway-Santa ́nna estimator

#a) Estimation of naive TWFE model - treating all counties as a single group

#First: we need to create a time_varying treatment indicator
mpdta = mpdta %>% 
  mutate(treated_post = as.integer(first.treat > 0 & year >= first.treat)) # Create a binary indicator variable that equals 1 if this observation belongs to a treated unit and the observation occurs during or after the year that unit received treatment — and 0 otherwise.
#this does the same - just using ifelse instead of as.integer
mpdta = mpdta %>% 
  mutate(treated_post = ifelse(first.treat > 0 & year >= first.treat, 1, 0))

#Then: Regression TWFE
m_twfe = feols(lemp ~ treated_post | countyreal + year, 
               data = mpdta, cluster = ~countyreal)
summary(m_twfe) # How does treatment affect log employment, controlling for county-level and year-level fixed effects?

#Coefficient on treated_post: -0.036549
#This means that treatment - policy adoption -  is generally associated with a 0.036 decrease in log teen employment, controlling for county and year. 
#This model pools all treatment cohorts together, therefore it assumes that the effect size is similar across all cohorts only taking into account the different times of adoption. 
#Furthermore, it assumes that post adoption the effect does not vary anymore, therefore it assumes that teen employment is only affect by this treatment and that it might not vary after adoption due to other reasons. 

#b) Callaway - Saintanna Estimator

#This estimates group-time average treatment effects separately for each cohort and time period, using never-treated counties as the control group
#att_gt: Average Treatment Effect on the Treated for each (g, t) group-time combination.
cs_out = att_gt(
  yname= "lemp", #outcome variable
  gname = "first.treat", #group variable - defines the treatment cohort
  idname = "countyreal", #unit identifier - each county is a unit that is tracked over time
  tname = "year", #time variable - year of observation
  xformla = ~lpop, #covariates to control for - here log population
  data = mpdta, 
  control_group = "nevertreated") #defining the comparison group, which never received treatment
cs_out
#To aggregate to an overall ATT
aggte(cs_out, type = "simple") #Takes all the individual group-time treatment effects and collapse them into one overall average treatment effect on the treated — weighted by cohort size.


#Overall ATT estimate: -0.0418
#Is it similar to or different from the naive TWFE estimate? 
#=> The overall ATT estimate is slightly more negative in comparison to the naive TWFE, therefore suggesting that log teen employment decrease a bit more when using the Callaway-Santanna estimator. 

#c) Event-study version of Callaway-Santanna
#On average across all treatment cohorts, what was the effect 1 year before treatment, in the treatment year, 1 year after, 2 years after, etc.?
cs_dyn = aggte(cs_out, type = "dynamic")
p2 = ggdid(cs_dyn)
p2
ggsave("p2_event_study_cs.png", p2, width = 6, height = 4)

#Comment: The plot shows on average across all treatment cohorts what the effect was in the respective years pre or post treatment.
#In the periods before treatment the estimates are not statistically distinguishable from zero, as they all gather close to the dashed-zero line and all of their confidence intervals do include zero. 
#This tells as that the parallel trend assumption is generally supported in this case as there is no great systematic divergence in trends before treatment which could contaminate the post-treatment effects. 
#Post-treatment effects seem to be rather dynamic which might suggest that the treatment effects evolves and changes gradually post treatment rather than hitting immediately post treatment with its full effect size. 
#Therefore the effect of policy adoption on log teen employment grows over time & might also decay over time, as after the third year post adoption the average trend increases again, closer to pre-treatment values. 


##3. Pre-testing the parallel trends assumption

#The group-time ATT estimates from att gt() include pre-treatment periods — specifically, ATT(g, t) for t < g — which can be used to construct a formal joint test of the parallel trends assumption.
#a) CS estimator with bootstrapped SE's => valid uniform confidence bans and a joint pre-test: 
cs_out_bt = att_gt(
  yname = "lemp", 
  gname = "first.treat", 
  idname = "countyreal", 
  tname = "year", 
  xformla = ~lpop, 
  data = mpdta, 
  control_group = "nevertreated", 
  bstrap = TRUE, 
  cband = TRUE)

summary(cs_out_bt)

#P-value for the pre-test of parallel trends assumption: 0.23267
#What does this p-test do? 
#It runs a joint hypothesis test across all pre-treatment estimates, checking whether they are all simultaneously close to zero. 
#Null hypothesis? 
#There is no systemic difference between treatment and control groups pre-treatment. 
#What does a large p-value tell us? 
#This p-value is way above conventional significance levels, therefore we fail to reject the H0. This does not necessarily prove the parallel trend assumption, however it shows that the data is consistent with it. 


#b) Visualization of all group-time ATT estimates, pre and post-treat
p3 = ggdid(cs_out_bt)
p3
ggsave("mpdta_att_gt.pdf", p3, width = 10, height = 6)

# Each panel corresponds to a treatment cohort; negative event-time val- ues are pre-treatment periods.
#Yes, the pre-treatment estimates are all close to zero and/or cannot be statistically distinguished from it, across all adoption cohorts. 
#All of the confidence intervals of the pre-treatment estimates do include the value of zero. 


#c) Limitations of pre-testing 
#The pre-test of parallel trends can only tell us whether there might be a systemic divergence in trends BEFORE treatment or not. However, it fails to inform us whether this parallel trend will continue like this post treatment or whether there might by dynamic trends after the adoption of the policy. 
#Also it does not tell us whether this parallel trend definitely holds, pre or post treatment. 
#A confounder that only emerges AFTER treatment would violate parallel trends without ever showing up in the pre-test. 
#Passing the pre-test is a necessary condition for credibility, but it is not sufficient to definitely prove parallel trends pre or post treatment. 



##4. Comparing control group specifications


#By default, the CS estimator uses never-treated units as the control group. 
#An alternative is not-yet-treated units — counties that will eventually receive treatment but have not yet been treated at time t. 
#This expands the control group (more observations, potentially more precision) but introduces a different assumption: that outcomes for not-yet-treated units are unaffected by anticipation of their own future treatment.

#a) Re-estimation of CS model - using not-yet-treated counties as the control group: 
cs_out_nyt = att_gt(
  yname = "lemp", 
  gname = "first.treat", 
  idname = "countyreal", 
  tname = "year", 
  xformla = ~lpop, 
  data = mpdta, 
  control_group = "notyettreated")

aggte(cs_out_nyt, type = "simple")

#Overall ATT: -0,0414
#Comparison to 2b) (-0.0418): this overall ATT is very similar to the estimation using "nevertreated" as a control. 
#They are similar in size and magnitude, suggesting that expanding the control group does not affect the size and direction of this treatment effect. 

#b) Event-study plot for this model
cs_dyn_nyt = aggte(cs_out_nyt, type = "dynamic")
p4 = ggdid(cs_dyn_nyt)
p4
ggsave("mpdta_event_study_nyt.pdf", width = 7, height = 4)

#The pre-trends look slightly different compared to the nevertreated event study, however they all still center around zero and are not statistically distinguishable from 0. 
#The post treatment effects show very similar dynamic and cumulative trends post treatment, as well as a decay of the treatment effect 3 years after adopting the policy. 
#Overall, the expanded control group did not change the conclusions we made from the estimation referencing never treated as a control group.  

#c) NYT vs. nevertreated control group; Trade-off? 
#Never-treated as a control group seems to be preferable when it is "big enough" as a control group and there might be the risk of anticipation effects when including not-yet-treated units. 
#However not-yet-treated units might be useful to be included when the control group is too small otherwise and there is no anticipation effect. 




##5. Discussion: why does TWFE fail in staggered settings?

#a) Naive TWFE in staggered settings, limitations? 
#In staggered Did settings, different cohorts receive treatment at different times. 
#A naive TWFE compares treatment effects irrespective of the time of treatment adoption. 
#This risks that i.e. already-treated units might be incorporated in the control group for later treated units => forbidden comparison. 
#This might bias the estimations as the already-treated units are not "clean controls" anymore. The naive TWFE compares against a control group that is affected by the treatment itself and also by time effects. 
#If treatment effects are heterogeneous across time and cohorts, this would compare immediate treatment effects to accumulated treatment effects over time, which might distort the results in size and direction. 

#b) Which model is more credible? TWFE vs. CS
#2a: Coefficient on treated_post: -0.036549
#2b: #Overall ATT estimate: -0.0418

#These two estimates are reasonable similar in magnitude and direction, the Callaway-Santanna estimate is slightly more negative. 
#The Callaway-Santanna is more credible in general, as it avoids the "forbidden comparison". Each cohort is compared against a clean counterfactual, making the results more credible.
#Furthermore the CS model is supported with the pre-trends-test, that lets us assume parallel trends across cohorts pre-treatment. 
#Additionally, based on the event-studies, the dynamic trends post treatment, can be captured, which can give further inside into how the effect accumulates over time and how it decays at a certain point in time post-treatment. 