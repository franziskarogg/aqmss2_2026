###Assignment 7.1 - Franziska Rogg 
###
library(dplyr)
library(tidyr)
library(ggplot2)
library(modelsummary)
library(sf)
library(spData)
setwd('/Users/Franzi/Desktop/AQMSS - RSTUDIO/AQMSS2_2026_RSTUDIO/assignment7')

data(world)
df  = world

##1. Inspecting an sf object
#a) Inspect the structure of the world dataset:
class(world)
names(world)
nrow(world)

#A sf object is a similar to a regular dataframe, with regular colums. 
#Additionally, it contains a geometry column (type simple-feature-column; list of object shapes) that stores the shapes of the objects
#All geometry types: POINT, LINESTRING, POLYGON


#b) Check the coordinate reference system: 
st_crs(world)
#EPSG:4326 
#WGS84 defines a geographic coordinate reference system using the World Geodetic System 1984, commonly used for GPS/global data/raw CSV coordinates


#c) Inspect geometry types
unique(st_geometry_type(world))

#d) Quick base-R map of GDP per capita
plot(world)
pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

# Display inline as well
plot(world["gdpPercap"], main = "GDP per capita by country")

##2. Attribute operations
#a) Filter to African counties 
 
africa = filter(world, continent == "Africa")
nrow(africa)
plot(africa["gdpPercap"], main = "GDP per capita -- Africa")


#b) Add pop_millions and summarize GDP per capita by continent: 
world = world %>%
  mutate(pop_millions = pop / 1e6)
gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))
print(st_drop_geometry(gdp_by_continent))


#c) Top 5 African countries by GDP per capita: 
africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)
print(head(st_drop_geometry(africa_sorted), 5))


#Extra:
centroids = st_centroid(world) #this finds the coordinates 
world2 = st_transform(world, 
                      crs = "+proj=laea +y_0=0 +lon_0=0 +lat_0=90 +ellps=WGS84 + no_defs")
ggplot(world2) +
  geom_sf() +
  theme_void()

ggplot() + 
  geom_sf(data = world) + 
  geom_sf(data = centroids, color = "red")

us = world %>% 
  filter(name_long == "United States")
plot(us)

ggplot() + 
  geom_sf(data = world) + 
  coord_sf(xlim = c(-25, 25), ylim=c(20,50))

##3. Simple visualization with ggplot2
#a) Choropleth map of world GDP per capita: 
ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() + 
  labs(title = "GDP per capita by country")
ggsave("world_gdp.pdf", width = 10, height = 5)

#b) Africa map with magma palette: 
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() + 
  labs(title = "GDP per capita -- Africa") +
  coord_sf(crs = 8857) #transforms something, but does not have to be added; you can also add projections here 

ggsave("africa_gdp.pdf", width = 7, height = 6)

#c) Africa map with country borders: 
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() + 
  labs(title = "GDP per capita -- Africa (with borders)")
ggsave("africa_gdp_borders.pdf", width = 7, height = 6)

