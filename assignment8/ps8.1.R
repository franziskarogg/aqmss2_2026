### Assignment 8.1 - Franziska Rogg

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
##1. Setup and OLS baseline
#-------------------------------------------------------------------------------

#a)
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp),]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)
nrow(world) #160 observation remain

#The log transformation of the gdpPercap deals with the skewed distribution. There are a few countries with very high vs. a large fraction with super low GDP, whihc makes the data dominated by outliers adn hard to interpret. 
#The log transformation spreads out the distribution by compressing the upper tail and spreading out the lower tail, making it better to interpret. 

#b) OLS regression: life expectancy on log GDP

ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

#Coeffienct 5.5403
#R2: 0.6449
#The coefficient on log_gdp is positive and statistically significant (p < 0.001). 
#It means that a one-unit increase in log GDP per capita — roughly a doubling of GDP per capita — is associated with higher life expectancy by approximately that many years on average. The model explains a substantial share of cross-country variation in life expectancy, as reflected by the R2.

#c) Plot of the OLS Residuals

world$ols_resid = residuals(ols_fit)
ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
ggsave("ols_residuals_map.pdf", width = 10, height = 5)

#What are the residuals: actual value-predicted values
#Therefore the negative residuals mean that the predicted value is larger than the actual one one. 
#That means that in Africa, life expectancy is lower than the income levels predict in the model => Cluster of negative residuals 
#Potential reasons: deseases such as hiv additional impact life expectancy

#Answer from Solution: 
#The residual map reveals clear geographic clustering. 
#Sub-Saharan Africa shows a concentration of negative residuals — countries with lower life expectancy than the model predicts given their income level, likely due to high HIV/AIDS prevalence and disease burden. 
#Western Europe and parts of East Asia display positive residuals, indicating that these regions achieve higher life expectancy than income alone predicts. This non-random geographic pattern in the residuals is a visual signal of spatial autocorrelation.

#-------------------------------------------------------------------------------
##2. Spatial weights matrix
#-------------------------------------------------------------------------------

#a) Quenn contiguity neighborhood
#Queen contiguity: defines neighbors as any polygon sharing at least one point

nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

#=> 16 numbers with no links, no neighbours; such as islands
#Some countries have zero neighbors in the contiguity matrix. 
#These are island nations (e.g., New Zealand, Japan, Caribbean states, Pacific island states) that share no land boundary or common border point with any other polygon in the dataset. 
#Queen contiguity requires at least one shared point; islands surrounded by ocean have none, so they are isolated nodes in the weights graph. 
#The zero.policy = TRUE argument allows these units to remain in the analysis despite having no neighbors.

sample  = world %>% 
  filter(name_long %in% c("Morocco", "Portugal", "Spain"))

nb = poly2nb(sample, queen = TRUE)
listw = nb2listw(nb, zero.policy = TRUE)
sample$name_long

#to calculate distance
centroids  = st_centroid(sample)
nb = dnearneigh(centroids, d1 = 0, d2 = 100)

distmat = st_distance(sample)
distmat_logical = drop_units(distmat)
nbdist = mat2listw(distmat_logical, styles = "w")

#b) Moran's I on the OLS residual
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

#Moran I statistic =  0.437486921   
#p-value = 8.054e-12

#Test statistic is positive and statistically significant. This suggests that there is a positive spatial autocorrelation. 
#Countries close to each other tend to have similar residuals — 
#either both overestimated or both underestimated — which violates the OLS assumption of independent errors. 
#Ignoring this pattern yields inefficient estimates and invalid standard errors.

#-------------------------------------------------------------------------------
##3. Lagrange Multiplier tests
#-------------------------------------------------------------------------------

#The Moran's I on residuals is positive and significant, therefore we need to decide:
#Should we use the Spatial Error model (SEM) or the Spatial Lag Model (SLM)? 
#The Lagrance Multiplier (LM) tests help guide this decision => Running all four tests at once: 

lm_tests = lm.LMtests(ols_fit, listw = listw, 
                      test = c("LMerr", "LMlag", "RLMerr", "RLMlag"), 
                      zero.policy = TRUE)
summary(lm_tests)


#LMERR: 
#Test-stat:  52.170055 
#p-value: 5.089e-13 

#LMlag: 
#Test-stat:   0.061576 
#p-value:  0.8040    

#a)LMerr tests for spatial dependence in the error term (λ ̸= 0), while LMlag tests for a spatially lagged dependent variable (ρ ̸=)
#LMlag tests whether a spatially lagged dependent variable belongs in the model (ρ ̸= 0 in the SLM). Both tests are significant (p < 0.05), meaning both types of spatial dependence appear to be present in some form when tested individually.
#When both standard LM tests are significant, we turn to the robust versions to discriminate.).

#b)The robust tests (RLMerr, RLMlag) each control for the presence of the other type of spatial dependence. 
#Comparing them: if RLMerr is more significant than RLMlag, the evidence favors the SEM; if RLMlag dominates, the SLM is preferred. Based on the decision rule from class — select the model whose robust test is more significant — the output above guides the choice between the two spatial models for Part 2.


#-------------------------------------------------------------------------------
##4. Spatial Error Model (SEM)
#-------------------------------------------------------------------------------

#Fitting the Spatial Error Model

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

#a) The coefficient on log_gdp from the SEM and the OLS estimate are both reported above. The SEM coefficient may shift somewhat from OLS because the error-structure correction absorbs spatial confounding. The λˆ (lambda) parameter captures spatial autocorrelation in the errors; if it is positive and statistically significant, the SEM has identified genuine spatial dependence in the residual variation.
#b) In the SEM, λ governs the spatial autoregressive process in the disturbances: u = λWu + ε. A positive and significant λ means that the unmeasured factors driving life expectancy are spatially correlated — omitted variables such as regional disease environments, cultural practices around healthcare, or cross-border health infrastructure are themselves geographically clustered. The SEM filters this spatial correlation out of the residuals without positing that life expectancy itself directly diffuses across borders.

#Decision rule (from class): Pick whichever robust test is more significant → here RLMerr wins easily → use the Spatial Error Model
#he spatial clustering in the residuals is driven by omitted geographic factors (things correlated across borders but not in the model), not by life expectancy in one country directly causing it in neighbors. SEM is the appropriate fix.


#c)Check Moran’s I on SEM residuals: Check whether the SEM actually fixed the spatial autocorrelation problem that OLS had.
world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

#Comparing this Moran’s I to the one from question 2b, the SEM substantially reduces the spatial autocorrelation in the residuals. The test statistic is now much closer to zero and the p-value is no longer significant (or much less so), indicating that the spatial error correction has absorbed most of the geographic clustering that OLS left behind in its residuals.




#-------------------------------------------------------------------------------
##5. Distance-based weights: an alternative neighborhood
#-------------------------------------------------------------------------------

#So far we have used queen contiguity to define neighbors: two countries are neighbors if their polygons share at least one point. But this misses island nations entirely and treats all shared-border pairs as equally connected regardless of distance. An alternative is to define neighbors based on geographic proximity: two countries are neighbors if the distance between their centroids is below a threshold.
#Test whether the results change if you define "neighbors" differently — using distance between country centers instead of shared borders.

#a) Computing centroids of every country => distance-based neighbourhood if centroids are within 300km of each other

coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)

#Note on distances and projections. Earlier in the course we said that computing distances requires projecting to a planar CRS (e.g. UTM). That advice applies when you work within a limited area where a single projection is accurate. Here we have a global dataset: no single planar projection preserves distances everywhere on Earth. The function dnearneigh() handles this automatically — when it receives an sf object with a geographic CRS (WGS84), it computes great-circle distances on the ellipsoid, which are accurate worldwide. The 300 km threshold is therefore interpreted in kilometers without needing to reproject.


#b)
listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)
sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(sem_dist)


#c) 
world$sem_dist_resid = residuals(sem_dist)
moran.test(world$sem_dist_resid, listw = listw_dist, zero.policy = TRUE)