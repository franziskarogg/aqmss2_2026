###Assignment 3.1 - Franziska Rogg 
library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)
library(marginaleffects)
##1.1 Setup & Data preparation
#a) Loading the dataset
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment3')
anes <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")
df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")
#raw file, via git hub, go on view raw, then copy link 
#mutate() creates a new variable and preserves the existing one, while transmute() replaces the variable.

class(NA_character_)
class(NA_real_)
class(NA)

df = df %>% 
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x), 
    age = ifelse(V201507x < 0, NA, V201507x), 
    female = case_when(
      V201600 == 2 ~ 1,
      V201600 == 1 ~ 0, 
      TRUE ~ NA_real_), 
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14, 
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_), 
    income = ifelse(V201617x < 0, NA, V201617x), 
    party_id = ifelse(V201231x < 0, NA, V201231x)
    )
nrow(df)   

#b) all ways to drop/filter the na's out
df = subset(df, !is.na(income)) # use either this
df = df %>% filter(!is.na(income)) #or this, but they both give the same
nrow(df)

df = na.omit(df) # don't use this, sometimes this just omits everything
nrow(df)

#c) Overall Turnout rate; voted == 1 und summary statistics
mean(df$voted, na.rm = TRUE) # gives same as without the na.rm = TRUE; but use this, use this!!!!!!!!
summary(df)

mean(df$voted) #this might not work when using the subsetting or the other one to omit the na's, don't use it
#86.1% of people in this sample report voting


##1.2 Exploratory Visualization
#a)Bar chart of turnout by education level:
turnout_by_edu = df %>%
  group_by(education) %>%
  summarise(turnout = mean(voted))
ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) +
  geom_col() +
  labs(x = "Years of education", y = "Turnout rate")

#b) the more years of education the higher the turnout rate
#Answer in Solution: Turnout increases with education: respondents with more years of education are more likely to report voting. The pattern is monotonic.

##1.3 Linear probability model
#a) Estimation of LPM: 
lpm = lm(voted ~ age + education + income + female, data = df)

#b)
wha

#d) Check predicted probabilities
#should be betwen 0&1, as we look at probabilities
preds_lpm = predict(lpm)
sum(preds_lpm < 0) # mow many predicted probs are below 0
sum(preds_lpm > 1) # how many predicted probs are above 1
range(preds_lpm)
#exceed 1 (1.17) => this is the main weakness of LPM


##1.4 Logistic regression

#a) Estimation of logit model
logit = glm(voted ~ age + education + income + female, family = binomial, data = df)

#logit with interaction
logit2 = glm(voted ~ age + education*female + income, 
             family = binomial, data = df)
tidy(logit2)
#interpretation of interaction: how much more education adds to the outcome because you are a women; but hard to interpret wiht the coefficients itself!!


nd = data.frame( #we did the same here just with newdata
  age = c(25, 50), 
  education = c(10,10), 
  income = rep(20, 2), 
  female = rep(1,2)
)
predict(logit, newdata = nd)
predict(logit, newdata = nd, type = "response")
exp(0.81)/(1+exp(0.81))
exp(1.73)/(1+exp(1.73))



#b) 
tidy(logit )

#c) Odds ratio
exp(0.29) #use e.g. for female exp(0.29) to get to the 1.33, the probability of voting increases by 33 for women 
exp(coef(logit)) 

#The odds ratio for education indicates the multiplicative change in the odds of voting for each additional year of
#education. An odds ratio above 1 means more education is associated with higher odds of voting.

#d) To Verify all predicted probabilities are bounded:
preds_logit = predict(logit, type = "response")
range(preds_logit)
#All predicted probabilities are between 0 and 1 => bounded

##1.5 Comparing LPM and logit
#a) Average marginal effects: 
avg_slopes(logit) #this converts the effect on odds into probability changes 

#b) 
#The AMEs from the logit model are similar to the LPM coefficients, as expected when predicted probabilities are mostly in a moderate range. Both approaches tell a broadly similar story about the relationship between each predictor and voter turnout.

#c)Side-by-Side table: 
modelsummary(list("LPM"=lpm, "logit"=logit),
             vcov = list("robust", NULL), output = "markdown")
#for plot predictions, you can also put conditions inside with condition = c("education", "female")

##1.6 Predicted probabilities
#a) Predicted Probabilities across education
p1 = plot_predictions(logit, condition = "education")
ggsave("pred_prob_education.png", p1, width = 6, height = 4)

#b) Predicted Probabilities by age and gender: 
p2 = plot_predictions(logit, condition = c("age", "female"))
ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)

#c) 
#Answer from Solution: Education shows a clear positive relationship with turnout. Age also has a positive effect. The plot by gender shows that both men and women follow similar age-turnout patterns, with any gender gap being modest relative to the age effect.

##1.7 Presenting results
#a)-b) Coefficient plot
p3 = modelplot(list("LPM" = lpm, "Logit" = logit),
               vcov = list("robust", NULL))
ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)

#c) 
#Answer from solution: For this dataset, the LPM and logit lead to similar substantive conclusions: age, education, and income are all positively associated with turnout, and gender has a modest or negligible effect. The differences between LPM and logit matter more when predicted probabilities are close to the boundaries (0 or 1). In this sample, turnout is relatively common, so the linear approximation works reasonably well
