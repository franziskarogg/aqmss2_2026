##Assignment 1 - Franziska Rogg 
getwd()
install.packages("gapminder")
library(gapminder)
write.csv(gapminder, "data/gapminder.csv", row.names = FALSE)
dir.create("data")
write.csv(gapminder, "data/gapminder.csv", row.names = FALSE)
file.exists("data/gapminder.csv")
