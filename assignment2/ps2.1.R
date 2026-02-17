###Assignment 2.1 - Franziska Rogg 

setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment2')
qog = read.csv('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/data/qog_std_cs_jan26.csv')
library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)


##1. 
#1.1 Setup and data preparation
#a) load & rename the variables 
df = qog %>% 
  select(country = cname,
         epi = epi_epi, 
         women_parl = wdi_wip, 
         gov_eff = wbgi_gee, 
         green_seats = cpds_lg)

#b) Dropping NA's
#Using na.omit() drops too many observations because green_seats is only available for a small subset of (mostly European) countries. Since the exercises focus on the other variables, it is better to keep the full sample and let R handle missing values automatically in each regression. # na.omit() would leave very few countries -- not recommended here
nrow(df)

#c) summary statistics
head(df)
summary(df)

#1.2 Exploratory Visualization -> use geom_smooth (method lm), geompoint

#a)-b) Scatterplot with linear fit
plot(df$women_parl, df$epi) 
ggplot(df, aes(x = women_parl, y = epi)) +
    geom_point() + 
  geom_smooth(method = 'lm')  +
  labs(x = "Women in Parliament (%)", y = "EPI Score")
  
#c) 
#Positive relationship: countries with more women in parliament tend to have higher EPI scores. This likely reflects that both variables are associated with broader development and governance quality.


#1.3 Bivariate Regression
#a) Run the bivariate model: 
model1 = lm(epi ~ women_parl, data = df)


#b) Extract results
modelsummary(model1)
tidy(model1)
summary(model1)
#epi= b0+ b1women_parl + e => epi = 39.2 + o.3*15.3

#to show the quantiles!
quantile(df$epi, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=TRUE)
quantile(df$women_parl, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=TRUE)


#c)
#The coefficient on women_parl indicates the predicted change in EPI score for each additional percentage point of women in parliament. To get the predicted difference between the 25th and 75th percentile, we can either multiply the coefficient by the IQR or use predict(). Both give the same result:

#Option1: use predict & create new data frame
pred = predict(model1, newdata = data.frame(women_parl = c(p25, p75))) 
#predict does: 'according to my regression model, what epi score would i expect if women_parl equals this value
pred[2] - pred[1]
#pred[1] → predicted EPI for low women representation
#pred[2] → predicted EPI for high women representation => How much higher is EPI in high-representation countries compared to low-representation countries?

# Option 2: multiply coefficient by IQR
p25 = quantile(df$women_parl, 0.25, na.rm = TRUE) #to find 25th percentile of women parliament: 25% of countries have this value or less
p75 = quantile(df$women_parl, 0.75, na.rm = TRUE) #to find 75th percentile of women in parliament
coef(model1)["women_parl"] * (p75 - p25) #if a country moves from low women representation to high women representation, its predicted epi increases by 5.6 points
#=> "Countries with high women representation score about 5.6 points higher in EPI than low-representation countries"

#1.4 Multivariate Regression
#a) Add Government efficiency
model2 = lm(epi ~ women_parl + gov_eff, data = df)
modelsummary(model2)
tidy(model2)

#b)
#The coefficient on women_parl decreases substantially once gov_eff is included. This suggests that part of the bivariate association was driven by government effectiveness being correlated with both women in parliament and environmental performance (omitted variable bias).
  
#1.5 Demonstrating OVB
#Recall the OVB formula: β ̃1 = βˆ1 + βˆ2 · δ ̃

#a) Extract the relevant coefficients:using pipes
beta1_biva = tidy(model1) %>% filter(term == "women_parl") %>% pull(estimate) #b1hat coefficient of women_parl from m1
beta1_mult = tidy(model2) %>% filter(term == "women_parl") %>% pull(estimate) #b1hat coefficient of women_parl from m2
beta2_mult = tidy(model2) %>% filter(term == "gov_eff") %>% pull(estimate) #b2hat coefficient gov_eff from m2
# This collects the exact numbers needed to plug into the OVB formula.

#b) Auxilliary Regression
aux = lm(gov_eff ~ women_parl, data = df)
delta = tidy(aux) %>% filter(term == "women_parl") %>% pull(estimate) #delta = coefficient of women_parl in this aux model
#It measures the relationship between the omitted variable (gov_eff) and your main X (women_parl); If women_parl goes up by 1 point, how much does gov_eff tend to go up?


#c) Verify the OVB Formula: 
# Right-hand side: beta1_mult + beta2_mult * delta
round(beta1_mult + beta2_mult * delta, 4)

# Left-hand side: beta1_biva
round(beta1_biva, 4)

#If the formula is correct, those two numbers should match (approximately). In the solution, they match (after rounding), confirming the formula. It proves that the difference between the bivariate and multiple coefficient is exactly explained by omitted variable bias.
#Both values match, confirming the OVB formula.

#d) 
#The bias is positive because gov_eff is positively correlated with both women_parl (δ ̃ > 0) and with epi (βˆ2 > 0). This inflated the bivariate estimate.



#1.6 Robust Standard Errors
#a) Classical SEs:
modelsummary(model2, output = "markdown")

#b) Robust SEs:
modelsummary(model2, vcov = "robust", output = "markdown")
  
#c) SEs may differ somewhat but conclusions typically don’t change with this sample.
  
  
#1.7 Presenting Results
#a) Side-by-side table:
modelsummary(list("Bivariate" = model1, "Multiple" = model2),
               vcov = "robust", output = "markdown")

#to check
modelsummary(list(model1, model2), output = "html")

#b)-c) Coefficient Plot
modelplot(list("Bivariate" = model1, "Multiple" = model2),
          vcov = "robust")

