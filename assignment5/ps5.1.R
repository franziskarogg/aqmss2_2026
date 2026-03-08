###Assignment 5.1 - Franziska Rogg 
library(dplyr)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(fixest )
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment5')

##1.Setup and Data exploration

#a) Load dataset 
presapproval = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")
df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

#The panel contains the following unique states and years: 
length(unique(df$State))
length(unique(df$Year))
table(table(df$State))
#The panel looks unbalanced to me as states have different numbers of years of data

#b) Summary statistics for the key variable 
summary(df$PresApprov)
summary(df$UnemPct)
df_sub = df %>% 
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) + 
  geom_line() +
  theme_minimal() + 
  labs(x = "Year", y = "Presidential approval (%)", color = "State")


#they more or less move together, experiencing the same spikes and falls in presidential approval over time, suggesting that common drivers shape the approval rates over the years
#Answer from Solution: The three states move closely together over time, tracking the same large swings in approval. This parallel movement suggests that common national factors (e.g., the incumbent president’s party, economic cycles, foreign policy events) are the dominant driver of approval, while state-level differences are relatively stable.

#c) Cross-sectional scatter of approval against unemployment
ggplot(df, aes(x = UnemPct, y = PresApprov)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_minimal()+ 
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")
#Across states and years, higher unemployment is slightly associated with lower presidential approval. 
#But this plot pools observations across state and year, therefore it reflects both within-state variations over time and permanent between-state differences in unemployment levels and approval. 
#This makes it difficult to draw causal conclusions, as we cannot determine where the differences are coming from. 

##2. Pooled Regression
#a) Pooled OLS regressing approval on unemployment
m_pooled = lm(PresApprov ~ UnemPct, data = df)
modelsummary(m_pooled)

#The coefficient on UnemPct is negative: a one-percentage point increase in unemployment is associated with a decrease of -0.144 in presidential approval rating, across time and states. 
#It seems to be statistically significant, however it conflates variations acorss states with variations within states over time

#b) Adding South as a control
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
modelsummary(m_pooled2)

#When controlling for Southern states, the coefficent on unemployment rate becomes slightly more negative (-0.347)
#This suggests that the bivariate pooled OLS estimate was not strongly confounced by the North-South distinction. 
#But it can be seen that Southern states show significantly higher rates of presidential approval, however that difference does not seem to be correlated with the unemployment-approval relationship in this specification. 

#c) 
#Pooled OLS can be seen as problematic for panel data because it ignores unobserved, time-invariant differences across states that may be correllated wiht unemployment. 
# For example: (1) states with historically weaker economies may have structurally higher unemployment and different political cultures that shape baseline approval;
#2) states in particu- lar regions may have persistent partisan leanings that affect how residents evaluate the president independently of economic conditions; 
#(3) states with large unionized labor forces may have both higher unemployment sensitivity and different approval baselines. All of these would produce omitted variable bias in the pooled OLS estimate.

##3. Entity fixed effects

#a) State fixed effects
m_fe = feols(PresApprov ~ UnemPct | State, data = df)
modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe), 
  vcov = ~State, 
  stars = TRUE, 
  gof_map = c("r.squared", "nobs"), 
  output = "markdown") 
)

#The coefficient on UnemPct changes relative to pooled OLS
#THe state fixed effects model compares approval within the same state across different years, removing the influence of any time-invariant state characteritics

#b) 
#The state fixed efffects absorb all of the time-invariant differences across states - including geography, political culture, long-run economic structure, adn regional identity. 
#South drops from the model => it does not vary within a state over time, so its effect is indistinguishable from the state-specific intercept (fixed effect)
#Any time-invariant variable is collinear with the set of state dummies and cannot be estimated separately.

#c)
#The coefficient on UnemPct in the state FE model identifies a within-state effect: it measures how approval changes in a given state when its unemployment rate rises or falls, compared to that state’s own average. This is fundamentally different from pooled OLS, which compares states with different unemployment levels to each other. The FE estimator controls for all stable state-level confounders (observed or not) but cannot account for time-varying omitted variables.

##4. Two-way fixed effects
#a-b)Adding year fixed effects to control for common time shocks:
m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)
modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe), vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

#c) 
#Year fixed effects absorb common time shocks: national economic cycles, presidential scandals, wars, or any other event that affects approval in all states simultaneously in a given year. If national unemployment rises during a recession, both the unemployment rate and presidential approval will move together in all states at once — not because of a state-level effect but because of the shared macro environment. Adding year dummies removes this source of confounding and identifies the effect of a state’s unemployment relative to the national average in each year. If the coefficient on UnemPct changes noticeably after adding year FEs, it suggests that common time trends were partly driving the relationship estimated with state FEs alone.







