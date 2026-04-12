### Assignment 9.1 - Franziska Rogg 

library(carData)
library(MASS)
library(nnet)
library(pscl)
library(AER)
library(marginaleffects)
library(ggplot2)
library(tidyverse)
data(BEPS)

setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment9')


#In this lab we use the British Electoral Panel Study (BEPS), a survey of British voters con- ducted around the 1997 general election. 
#We model two outcomes: (1) voters’ perceptions of national economic conditions (an ordinal 1–5 scale), and (2) their vote choice (a nominal three-category outcome). 
#Both require models that go beyond binary logistic regression. 
#Re- call from class that the key question before choosing a model is: what is the data-generating process for your outcome? 
#Ordinal outcomes have a natural ordering but not equal spacing; nominal outcomes have unordered categories.

#-------------------------------------------------------------------------------
##1. Ordered logit: perceptions of the national economy
#-------------------------------------------------------------------------------

#We first model respondents’ perceptions of national economic conditions as an ordinal out- come. 
#Recall from class: OLS treats categories as equally spaced on the underlying scale, which is almost certainly wrong for survey Likert items. 
#Ordered logit instead estimates both regression coefficients and threshold parameters (τ1, τ2, . . .) that cut the latent continu- ous propensity Y ∗ into the observed ordered categories.

#a) 
table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)

#Distribution across categories: the most common category is 3, then 4, then 2, then 5, and the least common is 1. 
#The variable is ordinal, that means it has a natural order and the categories are most likely not equally spaced, however the OLS would treat them as such (equally spaced). 
#This is problematic as the OLS would give as resutls that are in no clear category and it also assumes normal distribution, whihc is rarely the case for ordinal outcomes, they are often skewed

#Answer from Solution: 
#The distribution is concentrated in the middle categories (2, 3, and 4), with category 3 (“stayed about the same”) being the modal response. 
#Very few respondents chose the extreme ends (1 = got much worse, 5 = got much better). 
#OLS treats the numeric values 1–5 as equally spaced, implying that the difference between “got much worse” and
#“got a little worse” is identical to the difference between “stayed the same” and “got a little better.” 
#This is almost certainly wrong for Likert-type survey items, where psychological distances between adjacent categories need not be equal. 
#Ordered logit avoids this assumption by estimating threshold parameters that let the data determine the spacing of the latent scale.


#b) Ordered logit
m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge, 
                data = BEPS, Hess = TRUE)
summary(m_ologit)

#Note: polr() uses a reversed sign convention — the raw coefficients in the output are negated relative to the standard parameterization. 
#A positive raw coefficient corre- sponds to a negative association with higher categories. 
#Always interpret via marginal effects (see below).

#Raw coefficient on Europe: 0.123

#The raw coefficient on Europe is negative, which — after applying the sign reversal — implies a positive association: respondents with stronger pro-EU attitudes tend to perceive national economic conditions as having improved. 
#This is plausible: Blair’s government was broadly pro-European and pro-single-market, so EU supporters may have held more favorable views of its economic stewardship. 
#For reliable interpretation of magnitudes, we use average marginal effects below.



#c)
avg_slopes(m_ologit) #This reports one AME per predictor per response category

#A one-unit increase in pro-Europe attitude increased the probability of perceiving the economy as improved, category 4-5 (because of the sign reversal!!!)
#The AMEs show the average change in the probability of each response category associated with a one-unit increase in each predictor. 
#For Europe, the AMEs on the lower categories (1 and 2) are negative, while the AMEs on the higher categories (4 and 5) are positive, consistent with a positive association between pro-EU sentiment and more optimistic economic assessments. 
#As a sanity check, the AMEs for any given predictor must sum to zero across the five categories because probabilities are constrained to sum to one.

#Sanity Check: 
2.90e-03 + 1.58e-02  + 9.75e-03 + -2.22e-02 + -6.20e-03 #this is almost 0

#d) Predicted Probabilities: for the five response categories at the mean of all covariates, separately for women and males 
predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))

#Women are more likely to think the economy is worse (category 1), than man, and less likely to think the economy is good (category 5), than men.

#Who is more likely to think the economy is worse off, men or women (compare the probs for category 1 & 2 by gender)? 
preds_m_ologit = tidy(marginaleffects::predictions(m_ologit, by = "gender"))
women1 = preds_m_ologit %>%  
  filter(group ==1 & gender == "female") %>%
  pull(estimate)
women2 = preds_m_ologit %>% filter(group ==2 & gender == "female") %>%
  pull(estimate)
women12 = women1 + women2
male1 = preds_m_ologit %>% filter(group ==1 & gender == "male") %>%
  pull(estimate)
male2 = preds_m_ologit %>% filter(group ==2 & gender == "male") %>%
  pull(estimate)
male12 = male1 + male2
women12 - male12

#women are 3.15% more likely to think the economy is worse than men!

#-------------------------------------------------------------------------------
##2. Multinomial logit: vote choice
#-------------------------------------------------------------------------------

#We now turn to vote choice (Conservative, Labour, Liberal Democrat) — a nominal three- category outcome with no natural ordering. 
#Recall from class that the multinomial logit estimates J − 1 sets of coefficients, each comparing one category to a reference category. 
#All predicted probabilities sum to 1, and the model does not impose any ordering on the alternatives.


#a)

BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague + Kennedy + Europe, data  = BEPS, trace = FALSE)
summary(m_mlogit) #The model produces two sets of coefficients: Labour vs. Conservative and Liberal Democrat vs. Conservative. 


#The model produces two sets of log-odds coefficients: Labour vs. Conservative and Liberal Democrat vs. Con- servative. 
#The coefficient on Blair in the Labour vs. Conservative equation is strongly positive: higher approval of Tony Blair is associated with substantially greater log-odds of voting Labour rather than Conservative. 
#This makes intuitive sense — Blair was the Labour leader, so voters who rated him favorably were much more likely to have voted for his party. 
#By contrast, the Blair coefficient in the Liberal Democrat vs. Conservative equation is expected to be smaller or near zero, since Blair approval does not strongly differentiate Liberal Democrat voters from Conservatives.

#b) AMEs
avg_slopes(m_mlogit)

#AME of Blair on the probability of voting Labour: 0.1156
#This means the probability of voting for the Labour party when the blair approval increases by one unit, increases by 11.6%!
#The AME of Blair on the probability of voting Labour is positive and substantial. 
#A one-unit increase in Blair approval (on the 1–5 scale) is associated with a meaningful increase in the average probability of voting Labour, holding all other variables constant. This reflects the strong personalization of vote choice in 1997: feelings toward the party leader were a major driver of vote intention, and Blair in particular was unusually popular relative to his Conservative counterpart.

#c) 
#The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the odds ratio between any two alternatives is unaffected by the presence or absence of other alternatives. 

#What IIA means in this context, does it hold here? 
#IAA: odds ratio btw any two alternatives unaffected by presence of other alternatives
#The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the odds ratio between any two alternatives (e.g., Labour vs. Conservative) is unaffected by the presence or characteristics of the third alternative (Liberal Democrats). 
#In the red bus / blue bus analogy, IIA fails because two alternatives are near-perfect substitutes and removing one simply shifts its probability to the other rather than distributing it proportionally. 
#For British party choice, IIA is a moderate concern: Labour and the Liberal Democrats are both centre-left parties, sharing some ideological space, so some voters may treat them as partial substitutes in a way IIA cannot accommodate. 
#The Conservatives, however, occupy a clearly distinct ideological position (right-wing), so the three-party menu is not as degenerate as two buses of different colours. 
#Overall, IIA is plausible for Conservative vs. the others but is a more legitimate worry for the Labour/Liberal Democrat distinction.



#-------------------------------------------------------------------------------
##3. Poisson regression: publication counts
#-------------------------------------------------------------------------------

#We now analyze the number of articles published by biochemistry PhD students in the last three years of their doctorate, using the bioChemists dataset from the pscl package. 
#The outcome (art) is a non-negative integer count. Recall from class that count outcomes have a natural lower bound of zero and cannot take negative values, which makes OLS inap- propriate. 
#The natural starting model is Poisson regression; we then diagnose and address overdispersion using the negative binomial.

data(bioChemists)

#a) Exploration of outcome variable art
summary(bioChemists$art)
var(bioChemists$art)

pdf("art_histogramm.pdf", width = 6, height = 4)
hist(bioChemists$art, breaks = 20, main  = "Distribution of articles", xlab = "Number of articles", col = "gray80")
dev.off()

#The distribution of art is right-skewed, with a mode at zero and a long upper tail. 
#The mean is around 1.69 while the variance is approximately 3.71 — roughly twice the mean. 
#Under the Poisson assumption, the variance should equal the mean; a ratio substantially above 1 indicates overdispersion. 
#This pattern is a first signal that a standard Poisson model may underestimate uncertainty and produce anti-conservative standard errors.

#b) Fit the Poisson regression of art on all predictors
m_pois = glm(art ~ fem + mar + kid5 + phd + ment, 
             data = bioChemists, family = poisson)

summary(m_pois)

#Coefficient of ment: 0.0255
##IRR
exp(0.025543) #1.025872
#The incidence rate ratio (IRR) for ment is 1.026: each additional article published by the mentor is associated with a multiplicative increase in expected student articles by that factor, holding all else constant. 
#The effect is modest but positive, suggesting that more productive mentors slightly boost student output. 
#The residual deviance is substantially larger than the residual degrees of freedom (their ratio is well above 2), which is another clear diagnostic signal of overdispersion — the Poisson model does not adequately capture the variation in publication counts.


#c) Formal test for overdispersion
dispersiontest(m_pois)

#dispersion parameter: 1.82454
#p-value =  3.681e-09
#This makes the alternative hypothesis of overdispersion statistically significant

#The dispersion test strongly rejects the null hypothesis of equidispersion (p < 0.001). 
#The estimated dispersion parameter is well above 1, confirming that the variance in art substantially exceeds its mean. 
#This means the Poisson standard errors are too small: the model underestimates uncertainty, inflates test statistics, and produces p-values that are misleadingly small. 
#A model that explicitly accounts for overdispersion — such as the negative binomial — is needed.


#-------------------------------------------------------------------------------
##4. Negative binomial regression 
#-------------------------------------------------------------------------------

#The negative binomial (NB) model generalizes Poisson by adding a dispersion parameter θ that allows the variance to exceed the mean: Var(Yi) = μi + μ2i /θ. When θ → ∞, the NB reduces to Poisson. 
#A small estimated θ indicates severe overdispersion; a large θ indicates the extra dispersion is modest.

#a) Negative binomial model 

m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment, 
              data = bioChemists)
summary(m_nb)

#ment coeff in negative binomial model: 0.0291
#ment coeff in poisson model: 0.025543 
#theta (estimated overdispersion parameter): 2.264

#The coefficient on ment is similar to the Poisson estimate, indicating that the point estimate is reasonably stable. 
#The key difference is in the standard errors: the negative binomial model produces larger, more honest uncertainty estimates. 
#The estimated overdispersion parameter theta (shown in the summary) quantifies how much the variance exceeds the Poisson prediction; a smaller theta means more severe overdispersion. 
#Here theta is moderate, indicating meaningful but not extreme extra-Poisson variation.

#b) Comparing model fit using AIC: 

AIC(m_pois, m_nb)
#m_pois  3314.113
#m_nb    3135.917

#AIC for NB model is slightly lower, which means the improvement in fit outweighs complexity 
#The negative binomial AIC is substantially lower than the Poisson AIC, despite the NB model having one additional parameter (theta). Under AIC, the improvement in fit more than compensates for the added complexity. 
#This confirms that overdispersion is a genuine feature of the data, not noise, and that the negative binomial is the more appropriate model for these publication counts.


#c) Predicted article counts for male vs. women researchers, holding all other variables at their sample means
predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))

#The predicted number of articles for men exceeds that for women, holding marital status, number of young children, PhD prestige, and mentor productivity constant at their sample means. 
#The confidence intervals provide information on whether this gender gap is statistically distinguishable: if the intervals do not overlap, the difference is significant at conventional levels. 
#The gap reflects a persistent within-group gender difference in publication productivity that is not simply an artefact of other observable characteristics.


#d) 
#From Solution: Summary of findings:
#The Poisson model is not adequate for this dataset. 
#The variance-to-mean ratio of art is roughly double, the residual deviance far exceeds the degrees of freedom, and the formal dispersiontest() rejects equidispersion with a p-value well below 0.001. 
#The negative binomial model, which adds a dispersion parameter to accommodate this extra variation, achieves a substantially lower AIC and produces more reliable (wider) standard errors. On substantive findings: the mentor’s productivity (ment) has a positive and statistically significant effect, with an IRR slightly above 1 — each additional mentor article is associated with a modest multiplicative increase in expected student articles, suggesting that working with a productive mentor confers a real, if small, boost. 
#Gender (fem) and number of young children (kid5) are both negative and statistically significant: women publish fewer articles on average, and each additional child under age 5 is associated with reduced output. 
#PhD program prestige (phd) and marital status (mar) are not statistically significant in the negative binomial model. 
#Together, the results point to early-career productivity being shaped by mentor environment, gender, and family demands — patterns consistent with broader literature on PhD student outcomes in STEM fields.

