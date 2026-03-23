
###Assignment 7.2 - Franziska Rogg 
### Point Data and Spatial Joins

library(dplyr)
library(tidyr)
library(ggplot2)
library(modelsummary)
library(sf)
library(spData)
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment7')

#Loading data
data(world)
events = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/spatial/conflict_events.csv")

#-------------------------------------------------------------------------------
##1. Converting tabular data to sf
#-------------------------------------------------------------------------------

#a) Converting events data frame to sf object
events_sf = st_as_sf(events, coords = c("longitude", "latitude"),crs = 4326)
class(events_sf)
st_crs(events_sf)

#st_as_sf transforms a normal data frame into an simple features (sf) object => spatial object, that can be used for spatial operations in R. This also adds the geometry column 
#coord argument assigns in which colums the x and y coordinates are; longitude first, then latitude. 
#crs = 4326 sets the coordinate reference system to the standard format of WGS84

#b) 
nrow(events_sf) #There are 68354 events in the dataset
table(events_sf$event_type) #The most common event type is state-based events with a total count of 33487

#c)
#Plot with the whole world
ggplot() + 
  geom_sf(data = world,fill = "grey") + 
  scale_fill_viridis_d(option = "magma", na.value = "grey80") +
  geom_sf(data = events_sf, 
          aes(color = event_type)) + 
  scale_color_viridis_d(option = "magma", name = "Events") +
         theme_void() +
  labs(title = "World conflicts")
ggsave("world_conflicts.png")


#Plot with Africa
africa = world %>% filter(continent == "Africa")
ggplot() + 
  geom_sf(data = africa, fill = "grey") + 
  scale_fill_viridis_d(option = "magma", na.value = "grey80") +
  geom_sf(data = events_sf, aes(color = event_type)) + 
  scale_color_viridis_d(option = "magma", name = "Events") +
  theme_void() +
  labs(title = "Africa conflicts")
ggsave("africa_conflicts.png")

#The conflicts are most concentrated in the central belt of Africa, as well as on the south-eastern coast. 
#Furthermore there is a concentration of conflicts in the central northern part of the continent. 



#-------------------------------------------------------------------------------
##2.Spatial join: events to countries
#-------------------------------------------------------------------------------
st_crs(events_sf)
st_crs(world)
#They both share the same CRS

#Joining country attributes to conflict events
events_joined = st_join(events_sf, world[, c("name_long", "continent", "gdpPercap", "pop")])
head(events_joined)
names(events_joined)
nrow(events_joined) #still 68354 event observations!

#st_join combines the events_sf data frame with country attributes from the world polygon it falls within. 
#By default it uses the st_intersects(), meaning for each point it asks which polygon does this point intersect with and returns the attributes of the matching polygon. 
#If two spatial objects use different CRS, their coordinates are expressed in completely different units and reference systems - which makes it hard to compare them. 

#b) Checking for events that don't match any country polygon
sum(is.na(events_joined$name_long))
1576/68354 #sum(is.na(events_joined$name_long)) / nrow(events_joined)

#2.31% of the events in the dataset cannot be matched with a country.
#Potential reasons for this: conflicts at sea in international waters, conflicts right on the border


#c) 
event_counts_country = events_joined  %>% 
  st_drop_geometry() %>% 
  filter(!is.na(name_long)) %>% 
  group_by(name_long) %>% 
  summarise(
    n_events = n(), 
    total_fatalities = sum(fatalities, na.rm = TRUE)
  ) %>% 
  arrange(desc(n_events))
print(event_counts_country)

#The list is more or less consistent with current armed conflicts.
#I would have thought that given the recent events Sudan would be higher up, if not even the leader of this lists in terms of highest number of conflicts. 

#-------------------------------------------------------------------------------
##3. Choropleth of conflict intensity
#-------------------------------------------------------------------------------

#a)
event_counts_country_df = event_counts_country %>%
  st_drop_geometry()
world_joined = world %>% 
  left_join(event_counts_country_df, by = "name_long")
world_joined = world_joined %>%
  mutate(
    n_events = replace_na(n_events, 0),
    total_fatalities = replace_na(total_fatalities, 0)
  )
nrow(world)
nrow(world_joined)

#b)
ggplot() + 
  geom_sf(data = world_joined, aes(fill = n_events)) + 
  scale_fill_distiller(
    palette = "Reds", 
    name = "Number of Events"
  ) +
  theme_void() + 
  labs(title = "Conflict events by country")
  
ggsave("conflict_events_by_country.png")

#The coropleth map aggregates conflict event counts to the country level, therefore it is consistent with the map from 1c) but is more readable when comparing conflict counts between countries. 
#Yes the geographic pattern matches the event-level map from 1c)

#c)
ggplot() + 
  geom_sf(data = world_joined, aes(fill = log1p(n_events))) + 
  scale_fill_distiller(
    palette = "YlOrRd", 
    direction = 1,
    name = "Log(events + 1)"
  ) +
  theme_void() + 
  labs(title = "Conflict Log Map")

ggsave("conflict_log_map.png")

#The log transformation spreads out the variation at the lower end of the scale, while compressing the upper end of the scale closer to the rest. 
#The +1 ensures that countries with 0 conflicts also stay 0 with the log transformation,  log(1) = 1
#Therefore the log map reveals the countries with a "moderate" conflict level, rather than extreme cases. 
#This gives a more nuanced picture of the conflict distribution in Africa.


#-------------------------------------------------------------------------------
##4. Discussion
#-------------------------------------------------------------------------------

#a) Limitations of spatial join approach
#If an event falls direclty on the border between two countries,
#the spatial join approach might arbitrarily assign it two one of the countries or 
#might duplicate it for both of the countries. This might affect the results, as there is no way to assign the conflict to the "true" country. 

#If events fall outside a polygon due to small coordinate imprecision, they will be returned as NA even though they clearly belong to one country. 
#st_buffer might be used to fix this. 

#b) st_join vs. left_join

#left_join matches according to whether two rows share an identical values, e.g. name of a country
#This is preferable when both data sets are aggregated at the same unit of analysis and spatial information is not necessary or available for min. 1 data set. 

#st_join matches based on spatial location, whether a specific point fits into a polygon or not. 
#This is preferable when you only have spatial data, but not clear country identifier available adn you want to assign polygon attributes the spatial reference points. 

#-------------------------------------------------------------------------------
##5. Bonus: Are events far from the capital city more deadly?
#-------------------------------------------------------------------------------

#a-b) Subset Nigeria
nigeria_events = events_joined %>% 
  filter(name_long == "Nigeria")

#c) Capitals dataframe
capitals = data.frame(
  city = "Abuja", 
  country = "Nigeria", 
  latitude = 9.0, 
  longitude = 7.5
)

#Transform into a spatial object
capitals_sf = st_as_sf(capitals, 
                       coords = c("longitude", "latitude"),
                       crs = 4326)


#d) Transform into UTM projection
nigeria_events_utm = st_transform(nigeria_events, crs = 32632)
capitals_sf_utm = st_transform(capitals_sf, crs = 32632)

st_crs(nigeria_events_utm)
st_crs(capitals_sf_utm)

#e) st_distance:pairwise distance matrix between features
distance_matrix = st_distance(nigeria_events_utm, capitals_sf_utm)

#Add this matrix as a column in nigeria_events_utm
nigeria_events_utm = nigeria_events_utm %>% 
  mutate(distance_abuja_km = as.numeric(distance_matrix)/1000)

#f) Linear regressions

#general regression
m1 = lm(fatalities ~ distance_abuja_km, data = nigeria_events_utm)
summary(m1)
#If distance to capital increases, the fatalities slightly decrease => conflicts become less deadly (only slightly)

#using log fatalities and log distance
m2 = lm(log1p(fatalities) ~ log1p(distance_abuja_km), data = nigeria_events_utm)
summary(m2)

#controlling for event type
m3 = lm(log1p(fatalities) ~ log1p(distance_abuja_km) + event_type, data = nigeria_events_utm)
summary(m3)

#When controlling for event-type the conflicts fatalities increase slightly the larger the distance to the capital 

#interacting with event type
m4 = lm(log1p(fatalities) ~ log1p(distance_abuja_km)*event_type, data = nigeria_events_utm)
summary(m4)

#The effect of the event_type impacts the effect of distance on fatalities, making the negative effect of distance on fatalities less negative. 
#This shows that distance alone is not a sufficient determinant of the fatality count of a conflict, the type of conflict also matters!

