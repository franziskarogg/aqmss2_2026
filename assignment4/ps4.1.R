###Assignment 4.1 - Franziska Rogg 
library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)
library(marginaleffects)
library(readstata13)

#Predictions: The model’s best guess of the dependent variable for each observation ≠ actual value!!!!!!

##1.1 Setup & Data preparation
#a) Loading the dataset
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment4')
corruption = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")

#b) Drop NA's
df = df %>% filter(!is.na(ti_cpi) & !is.na(undp_gdp))
nrow(df)

#c) Summary statistics
summary(df$ti_cpi)
sd(df$ti_cpi)
summary(df$undp_gdp)
sd(df$undp_gdp)
#right skewness: mean > median


##2.2 Exploratory visualization
#a)  Scatter plot of corruption vs. GDP per capita (level):
p1 <- ggplot(df, aes(x = undp_gdp, y = ti_cpi)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(
    x = "GDP per capita (ppp)", 
    y = "Corruption Perceptions Index")
ggsave("p1.1.png", p1, width = 6, height = 4)

#b) positive, but non-linear, as most countries are at lower gdp values. general pattern - low gdp => low corruption index
#Answer from Solution: The relationship is positive—richer countries tend to be less corrupt—but the pattern is clearly non-linear. Most countries cluster at low GDP values, and the linear fit does not capture the curvature well.

#c) Scatter plot with log-transformed GDP:
p2 = ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", y = "Corruption Perceptions Index")
ggsave("p1.2.png", p2, width = 6, height = 4)
#log-transformation spreads the distribution of data points; as it is not forced into linearity
#Answer from Solution: The log transformation spreads out the lower-income countries and compresses the upper tail, producing a much more linear relationship.


##1.3 Bivariate Regression (predictions)
#a) Estimate the level-level model:
m1 = lm(ti_cpi ~ undp_gdp, data = df)

#b)
tidy(m1)
#The coefficient on undp_gdp gives the predicted change in the corruption index for a one-dollar increase in GDP per capita. For a $10,000 increase, multiply the coefficient by 10,000:
coef(m1)["undp_gdp"] * 10000 #to make the coefs bigger so we can interpret them better, the coefs are super small because of the different scales of the variables 
#this gives the coefficient for a 10000$ in GDP


#c)
q25 = quantile(df$undp_gdp, 0,25)
q75 = quantile(df$undp_gdp, 0.75)
c(q25,q75)
predictions(m1, newdata = datagrid(undp_gdp = c(q25,q75))) #you can also do it with different ranges, this is just an example
#For a gdp/capita: 520: 95th percentile: [2.35; 2.83] => the predicted value of corruption: 2.59
#For a gdp/capita: 10862: 95th percentile: [4.20, 4.57] => the predicted value of corruption: 4.38
#The difference in predicted corruption between a country at the 75th percentile and one at the 25th percentile of GDP captures the interquartile range effect. The confidence intervals indicate the precision of these predictions.
plot_predictions(m1, condition = "undp_gdp")


##1.4 Non-linear specifications (predictions)
#a)-b) Level-Log model 
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
tidy(m2)
#In a level-log model, a 1% increase in GDP per capita is associated with a change of β1/100 in the corruption index. For a doubling of GDP (log(2) ≈ 0.693):
coef(m2)["log(undp_gdp)"] * log(2)
plot_predictions(m2, condition = "undp_gdp")

#c) Quadratic model
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = df) #to make it quadratic
tidy(m3)
plot_predictions(m3, condition = "undp_gdp")

#d) Comparison of R2
r2 = c(
  "Level-Level1" = summary(m1)$r.squared, 
  "Level-Log" = summary(m2)$r.squared, 
  "Quadratic" = summary(m3)$r.squared)
r2 # => the level-log is the best fit when comparing the r2(error), because it is the smallest
modelsummary(list(m1,m2,m3),output = "markdown") # to look at them together, also shows the r2, so you dont have to do the above thing 

#Answer from Solution: The log specification fits the data best, consistent with the scatter plots showing a concave relationship. A non-linear specification is appropriate because the marginal return to additional GDP diminishes at higher income levels: moving from $1,000 to $5,000 matters more for governance quality than moving from $25,000 to $29,000

##1.5 Marginal effects

#a)Avg Marginal effects from the log model 
avg_slopes(m2, variables = "undp_gdp")

#b)
#Answers from Solution: The AME differs from the raw coefficient on log(undp_gdp) because the marginal effect of GDP in a level-log model depends on the level of GDP: ∂y/∂x = β/x. The AME averages this over all observed values. It tells us the average predicted change in the corruption index for a one-dollar increase in GDP across all countries in the sample.

#c) Marginal effects of the quadratic model at specific GDP values:
slopes(m3, variables = "undp_gdp", newdata = datagrid(undp_gdp = c(2000, 10000, 30000))) # this is looking at the marginal effect at specific points
#Answer from Solution: The marginal effect of GDP on corruption diminishes as countries become richer. At low GDP levels, an additional dollar of income has a larger predicted effect on corruption than at high GDP levels. This is consistent with the concave shape of the relationship.


##1.6 Prediction plots

#a)prediction plot for log model 
p3 = plot_predictions(m2, condition = "undp_gdp")
ggsave("p1.3.png", p3, width = 6, height = 4)

#b) prediction plot for quadratic model 
p4 = plot_predictions(m3, condition = "undp_gdp")
ggsave("p1.4.png", p4, width = 6, height = 4)


##1.7 Residual diagnostics

#Residuals = actual -predicted value
#=> They show how wrong the model is for each observation

#a) Residuals vs. fitted for the level-level model:
m1_aug = augment(m1) #this adds predicted values & residuals
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")
#Answer from Solution: The residual plot shows a clear curved pattern, indicating that the linear specification misses the non-linear relationship. The spread of residuals also appears to increase with fitted values, suggesting heteroskedasticity.
#m1: doesn't fit well, neglects non-linearity and variance increases with fitted values (heteroskedasticity)

#So the X-axis shows what the model estimates for Y.
#if the model would be correct: centered horizontally at 0
#if residuals would spread increasingly => heteroskedasticity
#if residuals curve upward => model misses non-linearity
#if residuals randomply scatter around 0 => model is well specified

#b)Residuals vs. fitted for the log model: 
m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")
#Answer from Solution: The log transformation substantially improves the residual pattern. The curvature is reduced, though some heteroskedasticity may remain.
#m2: curve shape reduces, better pattern => m2 fits a little better => residuals look more random



#c) Cook's distance
#this checks for influential observations: if some outliers determine the results

n = nrow(df) 
threshold = 4 / n #this defines a cut off rule: to flag observations that influence the model much more than average
cooks_d = cooks.distance(m2) #calculates influence for each observation
influential = which(cooks_d > threshold) #finds observations wiht high influence
df$cname[influential] #this selects the columns called cname from dataset, [influential] => selects only the rows whose indices are on in the object influential, that means some potential outliers
plot(m2, which = 4)

#d) 
#Answer from solution: Influential observations should not be removed automatically. They may represent genuine cases (e.g., very wealthy or very corrupt countries) rather than data errors. A recommended robustness check would be to re- estimate the model excluding these observations and compare the coefficients. If the results are similar, the original estimates are robust.


##1.8 Publication-quality table 
#a)
modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")


#b) 
#Answer from solution: The level-log model (m2) is the preferred specification. It has the highest R2, produces the best residual diagnostics, and its functional form has a clear substantive interpretation: the relationship between wealth and corruption is one of diminishing returns. The log transformation also avoids the quadratic model’s problem of an eventual sign reversal at extreme values.


