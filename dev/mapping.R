install.packages("raster")
install.packages("rgeos")
install.packages("maptools")

library(raster)
states.shp <- getData("GADM", country = "NOR", level = 2)
class(states.shp)

names(states.shp)
head(states.shp$NAME_2)
## fortify shape file to dataframe format
library(rgeos)
library(maptools)
library(ggplot2)
states.shp.f <- fortify(states.shp, region = "NAME_2")
class(states.shp.f)
head(states.shp.f)

## create random data to plot on map
num.states <- length(states.shp$NAME_2)
set.seed(123)
mydata <- data.frame(id = states.shp$NAME_2, prevalence = rnorm(num.states,
                                                                55, 20))
head(mydata)

## merge shape file with data
merge.shp.coef <- merge(states.shp.f, mydata, by = "id")
head(merge.shp.coef)

     ## basic plot
ggplot() +
  geom_polygon(data = merge.shp.coef,
               aes(x = long, y = lat, group = group,
                  fill = prevalence),
               color = "black", size = 0.25) + coord_map()


     ## nicer plot
     library(scales)
     library(ggmap)
     ggplot() + geom_polygon(data = merge.shp.coef, aes(x = long, y = lat, group = group,
                                                        fill = prevalence), color = "black", size = 0.25) + coord_map() + scale_fill_distiller(name = "prevalence",
                                                                                                                                               palette = "YlGn", breaks = pretty_breaks(n = 5)) + theme_nothing(legend = TRUE) +
       labs(title = "Prevalence of X in Norway")
