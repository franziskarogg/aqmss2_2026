###Assignment 3.2 - Franziska Rogg 
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment3')
library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)
library(marginaleffects)
##1.1 Setup & Data preparation

#a) Loading the dataset & create factor variables 
df <- read.csv("/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/data/star.csv")
star = read.csv('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/data/star.csv') %>% 
  mutate(classtype = factor(classtype,
                            levels = c(1,2,3),
                            labels = c("Small","Regular","Regular+Aide"))) %>% 
  mutate(race = factor(race,
                       levels = c(1,2,3,4,5,6),
                       labels = c('White', 'Black','Asian','Hispanic', 'Native American', 'Other'))) %>%
  
#b)Create binary variable small 
    mutate(small = ifelse(classtype == 'Small', 1, 0)) 
#c)
star = star %>% filter(!is.na(hsgrad)) #dropping NA's in hsgrad
star = droplevels(star)
nrow(star) #3037 observations remain


#d)High school graduation rate: overall and by classtype

#overall
mean_overall_hsgrad <- mean(star$hsgrad)
print(mean_overall_hsgrad) #0.833
#by classtype
mean_classtype_hsgrad <- star %>%
  group_by(classtype) %>% 
  summarise(mean = mean(hsgrad))
print(mean_classtype_hsgrad)

#Overall hsgrad-rate is around 83%. When looking at the the different class sizes it can be observed that the mean graduation rates of small (0.836) and regular+aide (0.839) are a little higher than for regular classes (0.825)
#However, the differences between the graduation rates of the respective classtypes and also the overall rate are relatively small. 

##2.2 LPM and logit

#a) Estimation of LPM
lpm1 = lm(hsgrad ~ small, data = star)
tidy(lpm1)

#b) Estimation of logit
logit1 = glm(hsgrad ~ small, family = binomial, data = star)
tidy(logit1)
exp(coef(logit1))


#c) 
#Interpretation of LPM coefficient on small: what is the estimated difference in graduation probability between small and non-small classes? 
#The coefficient on small represents the estimated change in the probability of graduating highschool for an one-unit increase in the small variable. 
#If the student was in a small class (small = 1), the probability of graduating highschool increased by 0.00375 (approximately 0.4%); compared to students in regular or regular+aide classes. 


#d) AME from logit
avg_slopes(logit1)

#How does this compare to the LPM coefficient?
#The AME represents the average change in the predicted probabilities from the logit of an outcome, it averages the effect across all observations. 
#Therefore, the log-odds are converted in to average probabilities, which makes the AME estimate comparable to the LPM coefficient. 
#The AME is also 0.00375, suggesting that being in a small class increases the likelihood of graduating highschool by 0.00375 => 0.4%


##2.3 Adding controls

#a) Estimation of LPM and logit with controls 
lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star)
tidy(lpm2)
logit2 = glm(hsgrad ~ small + race + yearssmall, family = binomial, data = star)
tidy(logit2)

#b) Compare the coefficient on small between the bivariate and controlled models

#LPM bivariate: small coefficient: 0.00375
#LPM controlled: small coefficient: -0.0756
#When comparing the coefficients on small of the controlled versus the bivariate model, it can be seen that the coefficient changes in size and magnitude.
#While the small classes in the bivariate model increase the likelihood to graduate highschool, the controlled model suggests that small classes decrease the probability of graduating by 0.0756 (approx: 7.6%)
#logit bivariate: small coefficient: 0.0271
#logit controlled: small coefficient:-0.0562
#The coefficient of the bivariate model also changes in effect size and direction, once more controls are added. 

#What does this tell about randomization? 
#Under proper randomization we would assume that the treatment effect would be independent of further controls, adding them to the model should not systematically change the treatment coefficient. 
#This might imply that students in small classes are different from students in regular or regular+aide classes, based on multiple reasons, not just based on the treatment. 


#c) Interpretation of the coefficient on yearssmall from the logit
avg_slopes(logit2)
#The AME represents the average change in predicted probability across all individuals in the sample.
#The yearssmall coefficient (0.0283) shows that, on average, one additional year in small classes increases the likelihood of graduating college by 2.383%, when holding all other predictors constant. 
#If a student would spend e.g. 3 years in small classes the average probability of graduating would therefore increase by 3*0.0283.


##2.4 Predicted Probabilities

#a) Using the controlled logit, computation of predicted graduation probabilities
#white student, small class, yearssmall = 3
#black student, class = regular, yearssmall = 0
nd = data.frame( 
  race = c("White", "Black"), 
  small = c(1,0), 
  yearssmall = c(3, 0)
)
predictions(logit2, newdata = nd, type = "response") 
# for white student: 95% confidence interval = [84.7%, 89.1%]; for black student: 95% confidence interval = [69.6%, 76.3%] 

#b) Plot predicted graduation probabilities across yearssmall for small vs. non-small classes
p1 = plot_predictions(logit2, condition = c("yearssmall", "small"))
ggsave("p1_pred_yearssmall_small.png", p1, width = 6, height = 4)



##2.5 Interactions
#a) Does the small effect on graduation differ by race? 
logit3 = glm(hsgrad ~ small*race + yearssmall, family = binomial, data = star)
tidy(logit3)
#b) 
avg_slopes(logit3, variables = "small", by = "race") 

#c)Is the small class effect larger for some groups than others?
#Yes, the effect of small classes does differ significantly between racial groups. 
#For all groups, except for 'Asian' the small effect further decreases, however the effect size of the racial group on the small class effect differs. 
#For Asian students the small effect even increases by additional 22.3%, therefore for this group the small effect is the largest, while for black students the effect of small classes decreases by 10.1%. 





##2.6 Presentation of results & discussion
#a) 
modelsummary(
  list(
    "LPM bivariate" = lpm1,
    "LPM controlled" = lpm2,
    "Logit bivariate" = logit1,
    "Logit controlled" = logit2,
    "Logit interaction" = logit3
  ),
  vcov = list("robust", "robust", NULL, NULL, NULL),
  output = "modelsummary.html")

#b)
p2 = modelplot(list("LPM1" = lpm1, "LPM2" = lpm2, "Logit1" = logit1, "Logit2" = logit2, "Logit3" = logit3),
               vcov = list("robust", "robust", NULL, NULL, NULL))
ggsave("p2_coefplot_lpm_logit.png", p2, width = 6, height = 4)


#c) Comment: 
#Overall the STAR data suggests, that small class sizes did show a significant effect on the probability of graduating high school, however whenn adding controls the effect size and direction did significantly change. 
#This suggests, that class size alone does not fully determine the probability of graduation, as the effect of small groups largely differs between e.g. racial groups. Furthermore it also depends to a large extent on the amount of years spent in a small class
#In general, I would conclude that small classes rather decrease the probability of graduating across racial groups and years spent in the respective class size
#When comparing the LPM with the estimation of the marginal effects based on the logit, the results do tell a very similar story in terms of effect sizes and directions. 
#The LPM provides the effect directly in probabilities, making interpretation very straightforward while the logit model accounts for the nonlinear nature of probabilities for this binary outcome, but the average marginal effects tell a very similar substantive story to the LPM.
#The experimental nature of this evidence does make the results more credible than observational studies, as the class allocation was done randomly. However, it can also be seen that the randomization effect is limited, since adding control to the model did change the small class effects significantly when compared to the bivariate model. 



