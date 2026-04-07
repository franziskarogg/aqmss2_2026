### Assignment 8.2 - Franziska Rogg

library(dplyr)
library(tidyr)
library(ggplot2)
library(modelsummary)
library(sf)
library(spData)
library(spdep)
library(spatialreg)
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment8')

#Loading data
data(world)

#-------------------------------------------------------------------------------
##1. Spatial Lag Model (SLM)
#-------------------------------------------------------------------------------

#The Spatial Lag Model (also called the Spatial Autoregressive model, or SAR) posits that the outcome itself diffuses across space: y = ρWy + Xβ + ε. 
#Fit it using lagsarlm() from spatialreg, with the same formula, data, and weights as the SEM.

#a) SLM Model

#using queen contiguity
slm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(slm_fit)

#Rho: -0.0042561
#p-value: 0.805
#coefficient on log_gdp

#rho is not significant as the p-value is very large! 

#using distance-based weights
slm_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(slm_dist)


#b) Interpretation of rho: 
#rho captures whether there is a spillover of the neighbour's outcome on the outcome of a certain country. 
#In this case rho is negative, suggesting that there is no such spillover effect
#If rho would be positive it would suggest that there is a positive spatial diffusion, clustering in y => the outcome. If a country's life expectancy is higher, then the neighbour country's life expectancy would also expected to be higher. 

#c) 
#The coefficient on log_gdp in the SLM output is not the marginal effect of GDP on life expectancy, as it only captures the immediate effect of a country's own GDP on life expectancy, but it does not capture the effect the neighbouring country's life expectancy on the own life expectancy. 
#The equilibrium matrix implies that as Xi changes by one unit, the outcome Yi also changes, and this change in outcome for country i, then also affects the outcome Yj of a neighbouring country. This then spills over to more neighbours => ripple effect through the whole neighbour network
#The matrix captures all the spillover effects, not just the immediate effect. 

#-------------------------------------------------------------------------------
##2. Direct & Indirect Effects
#-------------------------------------------------------------------------------

#a) Equilibrium direct & indirect effect
set.seed(500)
impacts = impacts(slm_fit, listw = listw, R = 500)
summary(impacts)

#Direct effect: 5.548223
#Indirect effect: -0.02353893
#Total effect: 5.524685

#The direct effect is a little bit higher than the coefficient of the SLM output, and almost identical with the ols output. 

#b)  Substantive meaning of the indirect effect: 
#Recall from class: the indirect effect captures the spillover from unit i’s x to all other units’ y, after the spatial feedback loop reaches equilibrium. 
#If log GDP per capita in Country A increases by 1 unit, what does the indirect effect say about life expectancy in neighboring countries?
#The indirect effect says how a change in outcome in country A, affects the outcomes of neighbouring countries, that means a one-unit increase in log_GDP in country A, decreases the life expectancy in neighbouring countries (as the indirect effect is negative in this case?)


#c) 
#Here, the total effect is smaller than the direct effect, as the indirect effect is negative? 
#The indirect effect would be larger if rho would be larger, in this case rho is close to 0, suggesting that there is none or very little spatial spillover. 
#If rho is larger than zero and significant, than the indirect effect would also be expected to be bigger. 


#-------------------------------------------------------------------------------
##3. Model Comparison
#-------------------------------------------------------------------------------

#a)
AIC(ols_fit, sem_fit, slm_fit)

#ols_fit: AIC of 966, 
#sem_fit, AIC of 895, 
#slm_fit: AIC of 968

#Lower AIC indicates better fit!
#The SEM_fit has the lowest 895, which agrees with the LM-test-based choice of model!
#The SEM was the best choice for this case. 

#b) 
#Yes, there was spatial autocorrelation in the OLS residuals, as the Moran's I showed:  0.437486921; p-value = 8.054e-12
#Based on the LM-tests I selected the SEM model, as it was statistically more significant than the SLM
#The coefficients for ols and slm fit are very similary, while the sem_fit coefficient on log_gdp is significantly lower (3.96)
#The SLM implies that there is no/only little spillover effect in regards of life expectancy across borders, as rho is negative and not significant. That means the indirect effect is very small (negative) here. 
#A major limitation of using queen contiguity weights for country-level data is that island states are missed in the matrix. Countries are only neighbours if they have a shared point, and therefore it excludes island states, such as e.g. Indonesia. 


summary(ols_fit)
summary(sem_fit)
summary(slm_fit)


#-------------------------------------------------------------------------------
##4. Extension: Spatial Durbin Model 
#-------------------------------------------------------------------------------

#The Spatial Durbin Model (SDM) nests both SEM and SLM by including a spatially lagged dependent variable and spatially lagged covariates. 
#It is estimated with lagsarlm() by adding the Durbin = TRUE argument.

#a) Fitting the SDM
sdm_fit = lagsarlm(lifeExp ~ log_gdp, data = world, 
                   listw = listw, zero.policy = TRUE, Durbin = TRUE)
summary(sdm_fit)

#Yes the lag.log_gdp is significant (very low p-value)
#Having a significant lag.log_gdp coefficient means that the neighbours GDP has an additional effect on a countries life expectancy, in addition to the own GDP effect of life expectancy
#Here the effect is negative — countries surrounded by wealthier neighbors tend to have lower life expectancy than their own GDP would predict.

#b)
AIC(sdm_fit) #940
#It is lower than the AIC of the ols and slm fit, but still significantly higher than the SEM fit, meaning that the SEM is still the best fit! THerefore the SDM and its added complexity is not necessary.  
