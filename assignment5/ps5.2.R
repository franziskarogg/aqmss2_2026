###Assignment 5.2 - Franziska Rogg 
library(dplyr)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(fixest )
library(readstata13)
library(plm)
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment5')

teacheval= read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/teaching_evals/teaching_evals.dta")
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/teaching_evals/teaching_evals.dta")

##1.Setup and Data exploration

#a) Unique instructors and courses in the data? 
n_distinct(df$InstrID) #there are 48 instructors in the data
n_distinct(df$CourseID) #there are 254 courses in the data
length(unique(df$InstrID))
length(unique(df$CourseID))

mean(table(df$InstrID)) #On average instructors appear in roughly 17-18 course-year pairs
#It feels like this is a rather long panel, as each instructor is observed many times over many years

#b) Cross-sectional scatter plot of Eval on Apct
ggplot(df, aes(x = Apct, y = Eval)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_minimal()+ 
  labs(x = "Share of A/A- grades", y = "Average course evaluation (5-point scale ")

#Across courses and and instructors, a higher percentage of students receicing A or A-, therefore higher levels of grading generosity, are associated wiht higher average course evaluations.
#However this plot pools observations across instructors and courses, therefore it might reflect both within course and variations and permanent between instructors differences in grading generosity and course evaluations. 
#This makes it difficult to draw causal conclusions, as we cannot determine where the differences are coming from. 

##2. Pooled OLS

#a) Pooled OlS with all three regressors
m1 = lm(Eval ~ Apct + Enrollment + Required, data = df)
modelsummary(m1)

#A one-percentage-point increase in the share of A grades is associated with a increase in evalution scores by 0.359
#This suggests, that the higher the grading generosity of instructors, the better their course evaluations might turn out, across all courses and instructors, while holding Enrollment and Required constant. 

#b) 
#The pooled OLS estimate for Apct might be biased as it ignores unobserved, time-invariant differences between different courses or instructors that might be corellated with grading generosity and evaluation scores. 
#Examples for unobserved characteristics could be varying teaching styles of the instructors. If they are very good at explain the course contents, course evaluations might be better, while the grades might also increase as the students understand the content better. 
#Also, the difficulty of the course the instructing is teaching might drive grading generosity and evaluation scores.
#I feel like the expected bias is upward - making the scores higher than they actually are


##3.Fixed effects models 

#a - b) Instructor fixed effects & TWFE
m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

modelsummary(
  list("Pooled OlS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe), 
  vcov = ~InstrID, 
  stars = TRUE, 
  gof_map = c("r.squared", "nobs"))

#Clustering Standard Errors around InstrID means that we center the standard errors for each instructor and do not spread them across all observations. 
#The instructor fixed effect is controlling for any time-invariant unobservable characteristics, e.g. certain personality traits or teaching styles that do not change over time. 
#The FE coefficient on Apct is a little bit smaller than the estimate form the pooled OLS. That means the omitted variable bias in the pooled OLS estimate is directed upwards as in bivariate model, grading generosity increases course score more thatn in the FE model. 

##4. Random effects and Hausmann test

#a) Random effects model - using plm
#Main assumption for random effects model: unobserved instructor-level heterogeneity is uncorrelated with the regressors

pdata = pdata.frame(df, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required, 
           data = pdata, model = "random")


#b) Hausman test to assess whether FE or RE is more appropriate

m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)





