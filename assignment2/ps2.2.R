###Assignment 2.1 - Franziska Rogg 
library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)

##2.1 Data preparation
#a) 
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment2')
star = read.csv('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/data/star.csv')

#b) - c) Create Factor Variables
star = read.csv('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/data/star.csv') %>% 
  mutate(classtype = factor(classtype,
                          levels = c(1,2,3),
                          labels = c("Small","Regular","Regular+Aide"))) %>% 
  mutate(race = factor(race,
                       levels = c(1,2,3,4,5,6),
                       labels = c('White', 'Black','Asian','Hispanic', 'Native American', 'Other'))) %>%
  
#d) Create Binary Variable 
  mutate(small = ifelse(classtype == 'Small', 1, 0))

#e) 
length(star$g4reading) #6325
length(na.omit(star$g4reading)) #2353
length(star$g4math) #6325
length(na.omit(star$g4math)) #2395
summary(star)

##2.1 Comparing Groups
#a)  Group means
means_small <- star %>% 
  filter(classtype =='Small') %>% 
  summarise(mean = mean(g4reading, na.rm = TRUE)) %>% 
  pull(mean)

means_regular <- star %>% 
  filter(classtype =='Regular') %>% 
  summarise(mean = mean(g4reading, na.rm = TRUE)) %>% 
  pull(mean)

means_regular_aide <- star %>% 
  filter(classtype =='Regular+Aide') %>% 
  summarise(mean = mean(g4reading, na.rm = TRUE)) %>% 
  pull(mean)

means_not_small <- star %>% 
  filter(small == 0) %>% 
  summarise(mean = mean(g4reading, na.rm = TRUE)) %>% 
  pull(mean)

#Highest Scoring Group: Small Classtype

#b) Bivariate Regression g4reading
m1 = lm(g4reading ~ small, data = star)
tidy(m1)
#Interpretation of Coefficient: For a one-unit increase in the'small' variable, therefore reducing the Group size to small compared to 'not small' (regular, regular+aide), 
# the reading scores of fourth graders increase by 3.1.

#c) Compare with difference in means
diff_means_small_reg_aide <- means_small - means_regular_aide
print(diff_means_small_reg_aide) 
#don't match as the regression is run on small, that means that it compares to 'not small' which consists of regular and regular+aide

diff_means_small_not_small <- means_small - means_not_small
print(diff_means_small_not_small) 
tidy(m1)
#The regression coefficient (3.10) almost matches the difference in means when comparing the group mean of small classes in reading versus the mean of 'not small' classes in reading

#d) Bivariate Regression g4math
m2 = lm(g4math ~ small, data = star)
tidy(m2)
#The effect of small class groups on math scores of fourth graders is also positive but significantly smaller than in regards of reading scores. 
#Therefore: For a one-unit increase in the 'small' variable, that means reducing class size from 'not small' to 'small', increases math scores of 4th graders by 0.591


##2.3 Adding Controls
#a) Multivariate Regression
m3 = lm(g4reading ~ small + race + yearssmall, data = star)
modelsummary(list(m1,m3))

#b)Comparison with bivariate regression + quality of Randomization? 
#The multivariate regression shows the conditional effect of small classes on the reading scores, while keeping race and the years a student spends in small classes constant. 
#When holding these additional controls constant, small class sizes decrease reading scores of 4th graders by -4.00.
#In comparison to the small coefficient of the bivariate regression (3.10), this changes a lot in magnitude as well as in direction of the effect. 
#This suggests that the positive effect of small on reading scores of 4th grades is rather due to omitted variables than due to its own effect. 
#Therefore, yearssmall and race might have a stronger effect on the reading scores than the class size. 
#In regards of randomization, this change in effect size and magnitude tells us that treatment and control group might differ systematically or due to other variables and small class assignment is not independent of race, or yearssmall.
#Furthermore, it might be the case that the multivariate regression controls for a variable affected by the treatment itself, as yearssmall is dependent on the assignment of class size. 
#If there would be perfect randomization, the coefficient of small would not change as drastically as it does here, when adding more controls.

#c) Interpretation of yearssmall coefficient: 
#This coefficient (2.170) captures the positive effect of a one-unit increase in the yearssmall variable on the reading scores of 4th graders.
#That suggests, that if the times spent in smaller classes increases by 1 year the readings scores increase by 2.170 holding the other predictors constant. 


##2.4 Interactions
#a) -b) Fit the model with interaction and print results
m4 = lm(g4reading ~ small*race + yearssmall, data = star)
broom::tidy(m4)
#The interactions between small and the different races change the effect of small on the outcome, the reading scores of fourth graders.
#The value of the respective interaction coefficient shows how much the effect of small on the reading scores changes according to the students' race in comparison to the reference group (white students)- 

#c) Estimated effect of small class for Black vs. White students? 

#Black Students
effect_black_students <- coef(m4)['small'] + coef(m4)['small:raceBlack'] 
print(effect_black_students)
#For Black students: We add the value of the interaction coefficients to the coefficient 'small' of the reference group
#Adding the effect of race increases the effect of small class sized by 6.97, therefore leading to a increase of reading scores by 1.66 in this study. 

#White Students => White students are the reference group (race = 1), therefore the estimated effect of small classes on reading scores for 4th graders is -5.32. 
#Holding all other estimators constant, small class sizes decrease reading scores by 5.32
effect_white_students <- coef(m4)['small']
print(effect_white_students)

#d)
#From a statistical point of view this estimated interaction suggests a potentially difference between racial groups when it comes to the effect of small class sized on educational outcomes. 
#However, the levels of uncertainty, when looking at the p-values, are very high, which suggests that these results might not be statistically meaninful. 
#Therefore, we might not be able to confidently say that this difference matters in the real world. 
#Furthermore, the conceptual meaningfulness of this interaction can be questioned, as we have no further background on the composition of the whole class in terms of race, do the effects change depending on whether the groups are more homogenous or heterogeneous?
#Also it would be interesting to see, whether these differences between the effects sizes for different ethnic groups happen because of different socioeconomic backgrounds, or discrimination or other disadvantages in the schools? 
#In theory, I would argue, that it could be plausible because of different reasons, that class size effects differ across racial groups due to different contexts and student needs, however in this sample the interaction does not seem to be meaningful. 


##2.5
#a) Model Summary
modelsummary(list("Bivariate_Reading" = m1, "Bivariate_Math" = m2, "Multiple" = m3, "Interaction" = m4),
             vcov = "robust", output = "modelsummary.html")
#b) Model Plot
modelplot(m1)
ggsave('plot1.pdf')
modelplot(m3)
ggsave('plot2.pdf')
modelplot(m4)
ggsave('plot3.pdf')

##2.6 Discussion
#a) Effect of small class sizes on student achievement
#When looking at the bivariate regression, the raw difference in means suggests that students in small classes perform better.
#However, when adding more controls, the estimated effect of class size does change drastically. 
#This suggests that parts of the initial positive effect of class size (from the bivariate regression) may also reflect student-specific traits and the time spent in the respective class serialize(
#Overall, the results suggest that the effect of class size is sensitive to further specifications and predictors. 
#b) Credibility of Evidence
#The credibility of these results is higher than other observational studies, since class assignment was done randomly and not dependent on parents' choices, socioeconomic background or school sorting. 
#Therefore treatment and control groups should be comparable on average, allowing for causal interpretation of the results and limiting confounding factors. 
#c) Limitations of data
#The test scores measure one-time academic performances in fourth grade and therefore may not fully capture long-term and longitudinal educational outcomes. 
#Furthermore, generalizeability of the study results beyond the context of Tennesse can be seen as limited, since schooling systems, contents taught etc. differ between states, regions and schools. 



