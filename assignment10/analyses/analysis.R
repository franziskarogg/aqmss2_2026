###Assignment 10 - Franziska Rogg
###Analysis

library(dplyr)
library(ggplot2)
library(modelsummary)
library(broom)
library(marginaleffects)
library(readstata13)

setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment10/analyses')

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
coef(m1["undp_gdp"] * 10000) #coefficient for a 10000$ increase in GDP

#Level-Log Model
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
coef(m2)["log(undp_gdp"] * log(2) #In a level-log model, a 1% increase in GDP per capita is associated with a change of β1/100 in the corruption index. For a doubling of GDP (log(2) ≈ 0.693):
