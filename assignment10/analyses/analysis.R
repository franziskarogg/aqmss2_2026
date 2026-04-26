###Assignment 10 - Franziska Rogg
###Analysis

setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment10')
library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)
library(marginaleffects)
library(readstata13)

#-------------------------------------------------------------------------------
##1. Loading Data 
#-------------------------------------------------------------------------------

corruption = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")

#-------------------------------------------------------------------------------
##2. Cleaning & Summary Statistics 
#-------------------------------------------------------------------------------

df = df %>% filter(!is.na(ti_cpi) & !is.na(undp_gdp))
nrow(df)

summary(df$ti_cpi)
sd(df$ti_cpi)
summary(df$undp_gdp)
sd(df$undp_gdp)

#-------------------------------------------------------------------------------
##3. Analyses
#-------------------------------------------------------------------------------

#Level-Level Model
m1 = lm(ti_cpi ~ undp_gdp, data = df)
coef(m1)["undp_gdp"] * 10000 #coefficient for a 10000$ increase in GDP
tidy(m1)

#Level-Log Model
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
coef(m2)["log(undp_gdp"] * log(2) #In a level-log model, a 1% increase in GDP per capita is associated with a change of β1/100 in the corruption index. For a doubling of GDP (log(2) ≈ 0.693):
tidy(m2)

#Scatter plot with log-transformed GDP:
ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", y = "Corruption Perceptions Index")
ggsave("graphs/scatter.pdf", width = 6, height = 4)


#Modelsummary
modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2), 
  vcov = "robust", 
  stars = TRUE, 
  gof_map = c("r.squared", "nobs"),
  output = "tables/regression_table.tex", 
  booktabs = TRUE)


modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2), 
  vcov = "robust", 
  stars = TRUE, 
  gof_map = c("r.squared", "nobs"),
  output = "tables/regression_table.png")
