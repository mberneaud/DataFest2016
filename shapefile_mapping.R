# Mapping with shapefiles
# Autor: Malte

# Loading libraries
library(rgeos)
library(maptools)
library(gpclib)

setwd("/home/malte/Git/DataFest2016/")

counties <- read.csv("USA_adm_shp/USA_adm2.csv")
states <- read.csv("USA_adm_shp/USA_adm1.csv")

# Reading in data
us.states <- readShapeSpatial("us_state_boundaries/cb_2014_us_state_5m.shp")


plot(us.states)

no.camping.parking <- filter(pur, minor_cat_name != "PARKING" &
                            minor_cat_name != "CAMPING")

