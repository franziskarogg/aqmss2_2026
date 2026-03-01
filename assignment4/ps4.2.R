###Assignment 4.2 - Franziska Rogg 
library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)
library(marginaleffects)
library(readstata13)
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment4')


##2.1 Data exploration

#a) Load dataset & summary statistics
infantmortality = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/infantmortality.dta")
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/infantmortality.dta")
summary(df) #countries in dataset: 101

#b) Histograms of income and infant
p1 = ggplot(df, aes(x = income)) + 
  geom_histogram() + 
  labs(
    x = "Income per capita",
    y = "Frequency",
    )
p1

p2 = ggplot(df, aes(x = infant)) + 
  geom_histogram() + 
  labs(
    x = "Infant mortality (per 1,000 live births)", 
    y = "Frequency"
  )
p2

#Both of the Histograms are right-skewed; as most countries cluster around low income per capita and at low infant mortality rates. 
#Only a few countries have very high values, far to the right, leaving a long right tail in both cases. 
#As the summary statistics show; for both variables the mean > median, also suggesting right-skewed distribution of the observations. 

ggsave("p2_histogram_infant.png", p2, width = 6, height = 4)
ggsave("p1_histogram_income.png", p1, width = 6, height = 4)

#c) Scatter plot of income vs. infant (level)
p3 =  ggplot(df, aes(x = income, y = infant, color = region)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(
    x = "Income per capita", 
    y = "Infant mortality (per 1,000 live births)", 
    color = "Region")
p3

#For all regions (except for Europe) the relationship clearly seems to be non-liner as most of the observations at low levels of income and between similar levels of infant mortality. 
#Therefore the linear fit does not capture the curvature of the data very well.
#Except for Africa(there it is positive!), the relationship is slightly negative that means with increasing income per capita, infant mortality slighlty decreases.
#For Europe the datapoints are less clustered and seem to be better captured with the linear fit. 

#d)Scatter plot of income vs. infant (log-log)
p4 =  ggplot(df, aes(x = log(income), y = log(infant), color = region)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(
    x = "Income per capita", 
    y = "Infant mortality (per 1,000 live births)", 
    color = "Region")
p4

#Log-log relationship => more linear!
#The log-log transformation seems to spread out income levels and levels of infant mortality allowing for a much more linear distribution of the observations
#For Americas, Asia, Europe a clear negative linear relationship can be observed. 

ggsave("p2_histogram_infant.png", p2, width = 6, height = 4)
ggsave("p1_histogram_income.png", p1, width = 6, height = 4)
ggsave("p3_scatter_income_infant.png", p3, width = 6, height = 4)
ggsave("p4_scatter_logincome_loginfant.png", p4, width = 6, height = 4)


##2.2 Comparing specifications

#a) Estimation of level-level model: 
m1 = lm(infant ~ income, data = df)
tidy(m1)

#b) Estimation of log-log model: 
m2 = lm(log(infant) ~ log(income), data = df)
tidy(m2)

#c)Interpretation of coefficients

#M1: What is the predicted change in infant mortality for a 1,000 increase in income
coef(m1)["income"] * 1000
#The coefficient on income (-0.0209) gives the predicted change in infant mortality (per 1,000 live births) for a one-dollar increase in income (per capita). Infant mortality decreases by -0.0209. 
#For a 1,000$ increase in income, the coefficient is to be multiplied by 1,000. Therefore the infant mortality decreases by 20.9 as income per capita increases by 1,000. Therefore the number of infant deaths is predicted to decrease by 20.9

#M2: What does the log-log coefficent (which is an elasticity) mean here? 
coef(m2)["log(income)"] * 10
#The log-log model, transforms the coefficient on the log of income into an elasticity, therefore showing relative changes
#A 10% increase in income is therefore associated with a -5.1% decline in infant mortality 

#d) Residuals vs. fitted values plot for M1, M2
#M1, level-level model 
m1_aug = augment(m1) #this adds predicted values & residuals
p5 = ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")
p5

#M2, log-log model 
m2_aug = augment(m2) #this adds predicted values & residuals
p6 = ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Log-Log (m2)")
p6

# Which specifications has the better residual pattern? 
# For m1 the residual plot shows a clear curved pattern, indicating that with increasing fitted values the spread of residuals also increases, suggesting heteroskedasticity. 
# Also the linear specification of the model clearly misses the non-linear relationship.
# For m2: the log-log transformation significantly imporves the residual pattern as they are more constantly centered around 0. 
#However it can still be seen that for some fitted values the spread of residuals increases as well, which suggests that some heteroskedasticity still remains!
#Overall, based on the pattern of the residuals, the log-log specification shows a better pattern than M1.

ggsave("p5_residuals_fitted_m1aug.png", p3, width = 6, height = 4)
ggsave("p6_residuals_fitted_m2aug.png", p4, width = 6, height = 4)


##2.3 Multiple regression with controls

#a) Estimation of Log-Log model with controls for region and oil-exporting status
m3 = lm(log(infant) ~ log(income) + region + oil, data = df)
tidy(m3)

#b) Does controlling for region and oil status change the income effect? 
#No, adding controls to the model does not change the effect of income itself, it only adjusts the intercept (parallel shift of Y as controls are added)
# The value of the coefficient decreases a bit, as adding these controls helps correcting omitted variable bias, but the slopes do not differ among groups. 
#The effect of income remains the same for every group. 



#c) What does the coefficient on the Africa region indicator tell about infant mortality in Africa, controlling for income?
#Changing the reference group, as right now Africa is reference
m3_ref_change = lm(log(infant) ~ log(income) + relevel(factor(region), ref = "Europe") + oil, data = df)
tidy(m3_ref_change)
# Holding income constant, the Africa region indicator shows that for African countries the infant mortality increases by 1.03% compared to European countries (reference group), intercept increases while the slope stays for all groups. 


#d) Average marginal effects
avg_slopes(m3)
avg_slopes(m3, variables = "income")
#AME of income: -0.00159
# The AME averages the effect of income across all observations. 
#On average the increase of 1$ in income leads to a decrease of of infant mortality(per 1,000 live births) by 0.00159



##2.4 Interaction: oil status and income

#a) Estimation of interaction model between oil status and log income
m4 = lm(log(infant) ~ log(income) * oil + region, data = df)
tidy(m4)

#b) Marginal effect of income separately for oil-exporting and non-oil exporting countries. 
avg_slopes(m4, variables = "income", by = "oil")

#c) Does the relationship between income and infant mortality differ for oil-exporting countries? 
#What might be the reason for this?
#Yes the relationship between income and infant mortality changes based on whether a country exports oil or not. 
#For oil exporting countries the effect of income increases by 0.00111 => the slope of the income effect changes, it increases. 
#For non-exporting countries the effect of income decreases by 0.00209 => the slope of income effect decreases
#Therefore, it a country does not export the slope of income becomes slightly more negative, leading to the conclusion that for non-exporting countries the infant mortality decreases a little more.  
#The reason for this change in income effect across oil-exporting group vs. not, is the use of the interaction of log(income) * oil. The interaction causes that effect size adn direction might change across groups and is not equal for all anymore. 


#d) Plot: marginal effect of income varying by oil status
p7 = plot_slopes(m4, variables = "income", condition = "oil")
p7
ggsave("p7_marginaleff_income_by_oil_m4.png", p7, width = 6, height = 4)



##2.5 Predicted values for specific scenarios

#a) Using m3(no interaction); Predicted mortality rates
#A non-oil African country with income  = $1000
#A non-oil European country wiht income = $20000
#A oil-exporting country in the Americas with income = $10000

pred_m3 = predictions(m3, 
            newdata = data.frame(
              income = c(1000, 20000, 10000), 
              region = c("Africa", "Europe", "Americas"), 
              oil = c("no", "no", "yes")))
tidy(pred_m3)
#Outcome is log(infant) => exponentiate predictions to get infant mortality in the original scale
exp(pred_m3$estimate)

#b) Are these predicted values plausible? How large is the gap between the African and European scenarios
#The model predicts the number of infant deaths per 1,000 live births given different conditions. 
#These results seem plausible and emphasize the effect income and region can have in terms of reducing infant mortality when controlling for oil-export. 
#The gap between Africa and Europe in infant mortality is 58.38, meanign that in an African level at the given income level 58.38 more infants die per 1,000 live births in comparison to Europe at the given income level. 
exp(pred_m3$estimate)[1] - exp(pred_m3$estimat)[2]


##2.6 Publication-quality visualization

#a) Prediction plot: Predicted infant mortality across income levels, separately by region
p8 = plot_predictions(m3, condition = c("income", "region")) + 
  labs(
    title = "Predicted Infant Mortality by Income and Region",
    x = "Income per capita (USD)", 
    y = "Predicted Infant mortality (per 1,000 live births)", 
    color = "Region"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"), 
    axis.title = element_text(size = 12)
  ) + 
  guides(fill = "none")
p8

ggsave("p8_predictions_income_infmort_by_region_m3.png", p8, width = 6, height = 4)


#b) Comment & Discussion
#To a general audience, this plot suggest a clear relationship between wealth and income mortality, however the returns of income in regards of infant mortality decrease once a certain level of income is reached. 
#Overall, the plot shows a clear negative relationship, as income increases, the predicted infant mortality decreases. 
#However, as the slope of all curves flattens as wealth further increases, these diminishing returns show that income gains matter most for poorer countries with a lower baseline of income per capita. 
#Geography plays an important role, as it can be seen that at similar income levels Africa is still predicted to have higher infant mortality rates that all other regions. This indicates that regional factors beyond income contribute to these differences in infant mortality. 
# However, this analysis also has important limitations. Omitted variable bias may be present as similar relevant determinant of infant mortality, such as healthcare access or sanitation are not included in the sutdy. 
#Reverse Causality may also be an issue as the link between income and infant mortality can be seen as bidirectional. While higher income deccreases mortality of infants, therefore improves health of the population, better health and lower mortality rates can also boost income by enhancing productivity and human capital.
#This makes it harder to determine the causal chain of events, which limits the validity of of the obtained results from the study. Without accounting for this, the study might overstate the impact of income on mortality rates. 
#Additionally, the conclusion of this analysis are based on inferences made on the country-aggregate level, however, this hided the distribution of observation within the countries. 
#This might lead to misleading conclusions about individuals, or regions within a specific country, ignoring potential variability and diversity within countries. 

(e.g. omitted variables, reverse causality, ecological fallacy)? 

##2.7 Diagnostics and robust inference

#a) Residuals vs. fitted values plot M3
m3_aug = augment(m3) 
p9 = ggplot(m3_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")
p9

ggsave("p9_residuals_fitted_m3aug.png", p9, width = 6, height = 4)

#The plot shows a rather constant spread of residuals centered around 0 as the fitted values increase. 
#However, heteroskedasticity still seems to be present to as for middle-range fittet values the spread of residuals significantly increases. Therefore, heteroskedasticity is present, however to a lesser extent than in the level model. 

#b) Comparison of Regression tables with robust SE's
modelsummary(
  list("Level" = m1, "Log-Log" = m2,
       "Controls" = m3, "Interaction" = m4),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "modelsummary_with_robust.html")


#c) Comparison of robust vs. default standard errors for m3
# Modelsummary without robust s.e: 
modelsummary(
  list("Level" = m1, "Log-Log" = m2,
       "Controls" = m3, "Interaction" = m4),
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "modelsummary_without_robust.html")


#Why use robust SEs? Do the conclusions change? 
#When incorporating robust standard errors, the conclusions do not change, but the inferences made become stronger and more robust. 
#As the Residual vs. Fitted plots show, some heteroskedasticity remains in the used models; robust SE's correct the spread in the standard errors so they remain valid even if heteroskedasticity is present, therefore fixing the inference while retaining the same conclusion. 
#The modelsummaries show that the robust SE's are smaller than standard SE's, making our inferences even more reliable. 

